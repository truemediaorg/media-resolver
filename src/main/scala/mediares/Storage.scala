package mediares

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future, blocking}
import scala.jdk.FutureConverters._

import java.net.{URL, HttpURLConnection}
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.{Collections, Timer, TimerTask, HashMap}
import java.util.concurrent.{TimeUnit, Executors}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.logging.Level

import upickle.default._

import software.amazon.awssdk.auth.credentials.{
  AwsBasicCredentials, StaticCredentialsProvider, ContainerCredentialsProvider}
import software.amazon.awssdk.core.async.AsyncRequestBody
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.model.GetObjectRequest
import software.amazon.awssdk.services.s3.model.HeadObjectResponse
import software.amazon.awssdk.services.s3.model.NoSuchKeyException
import software.amazon.awssdk.services.s3.model.PutObjectRequest
import software.amazon.awssdk.services.s3.presigner.S3Presigner
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest
import software.amazon.awssdk.transfer.s3.S3TransferManager
import software.amazon.awssdk.transfer.s3.model.{Upload, CompletedUpload}

object Storage {

  private val log = java.util.logging.Logger.getLogger("storage")
  import Pool.ctx

  // we create a thread-pool to handle downloading media
  private val execService = Executors.newCachedThreadPool()

  private def s3credsFromEnv = {
    val Seq(awsAccessKeyId, awsSecretAccessKey) =
      Env.getSecrets("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
    StaticCredentialsProvider.create(AwsBasicCredentials.create(awsAccessKeyId, awsSecretAccessKey))
  }
  private def s3credsFromContainer = ContainerCredentialsProvider.builder().build()
  private lazy val s3creds = {
    if (Env.getEnv("ECS_CONTAINER_METADATA_URI").isDefined) s3credsFromContainer
    else s3credsFromEnv
  }
  private def javaDuration (seconds :Int) = java.time.Duration.ZERO.withSeconds(seconds)
  private lazy val s3client = S3AsyncClient.builder.
    region(Region.US_EAST_2).
    credentialsProvider(s3creds).build()
  private lazy val s3xfer = S3TransferManager.builder.s3Client(s3client).build()
  private lazy val s3presigner = S3Presigner.builder.credentialsProvider(s3creds).build()

  private val MediaBucketName = "OPEN-TODO-PLACEHOLDER-deepfake-media"
  private val ThumbnailBucketName = "OPEN-TODO-PLACEHOLDER-deepfake-thumbnails"

  private val MaxRetries = 3

  private lazy val disableReuse = Env.getBool("DISABLE_REUSE", false)
  private val statuses = HashMap[String, Status]()

  /** Operational stats. */
  def stats = statuses.synchronized { Status.summarize(statuses.values.asScala) }

  /** Returns the current status of a `cacheMedia` request. */
  def status (id :String) :Future[Option[Status]] = getStatus(id) match {
    case null =>
      log.info(s"Checking status of unknown media [id=$id]")
      check(MediaBucketName, id).map(_ match {
        case S3Status.Uploaded => Some(getStatus(id))
        case _ => None
      })
    case status => Future.successful(Some(status))
  }

  /** Registers a webhook to be notified when caching is complete for `id`. Note: if media in
    * question is unknown, has failed, or is already completed, no webhook will be registered.
    * @return whether a webhook was registered for `id`. */
  def registerWebhook (id :String, url :String) :Boolean = statuses.synchronized {
    statuses.get(id) match {
      case null => false
      case status =>
        if (status.isComplete) false
        else {
          log.info(s"Adding webhook [id=$id, url=$url]")
          status.addWebhook(url)
          true
        }
    }
  }

  /** Starts downloading and storing to S3: the main media for `m` as well as its thumbnail and
    * audio track if appropriate.
    * @return a media instance that will have its audio track metadata filled in if we decided that
    * we need to extract the audio track from the video track. */
  def cacheMedia (m :Media) :Media = {
    // determine whether or not we need to extract a thumbnail or audio track
    val isVideo = m.mimeType.startsWith("video/")
    val extractAudio = isVideo && m.audio == null
    val extractThumb = isVideo && m.videoThumbnailUrl.isEmpty

    // if we need to extract a thumbnail or audio track, then we need to download the video to the
    // local filesystem, run the extraction job(s) and then upload the results to S3
    if (extractAudio || extractThumb) checkStoreAndExtract(m.id, m.downloadUrl, extractAudio, extractThumb)
    // otherwise stream the main media straight to S3
    else checkStore(MediaBucketName, m.id, m.downloadUrl)
    // if there's a separate downloadable audio track, store that directly
    if (m.audio != null) checkStore(MediaBucketName, m.audio.id, m.audio.downloadUrl)
    // if there's a separate downloadable thumbnail, store that directly
    m.thumbnailUrl.foreach { checkStore(ThumbnailBucketName, Media.thumbnailId(m.id), _) }

    // if we will be extracting the audio, return a Media with metadata that reflects that
    if (extractAudio) m.copy(audio = Media.extractedAudio(m)) else m
  }

  /** Adds "preflight" status for main and audio ids. This is needed for the `yt-dlp` backend, so
    * that our media appears to exist during the time that it is downloading but before the upload
    * starts. */
  def preuploadMedia (mainId :String, audioId :String) = {
    addStatus(mainId, new PreflightStatus(mainId))
    addStatus(audioId, new PreflightStatus(audioId))
  }

  /** A special mechanism to upload the results of `yt-dlp` into our S3 cache. */
  def uploadMedia (
    mainId :String, main :Local.File, audioId :String, audio :Local.File, thumbnail :Local.File
  ) :Future[Unit] = {
    def uploadWithStatus (id :String, file :Local.File) :Future[Unit] = {
      val (prog, res) = upload(MediaBucketName, id, file)
      addStatus(id, new Status(id) {
        def progress = prog()
        def result = res.map(_ => getSignedUrl(id)).value
      })
      res.onComplete(_ match {
        case util.Success(r) => markComplete(id)
        case util.Failure(err) => markFailed(id, err)
      })
      res
    }

    // start all the uploads and return a future that aggregates their completion
    val thumbRes = upload(ThumbnailBucketName, Media.thumbnailId(mainId), thumbnail)._2
    val mainRes = uploadWithStatus(mainId, main)
    val audioRes = uploadWithStatus(audioId, audio)
    for (_ <- thumbRes ; _ <- mainRes ; _ <- audioRes) yield ()
  }

  /** Handles any post-processing needed to "finalize" user uploaded media. This includes
    * extracting and storing a thumbnail image, as well as extracting and storing an audio track
    * for videos. */
  def finalizeUpload (mediaId :String, mimeType :String) :Unit = {
    log.info(s"Finalizing file upload [id=$mediaId, mimeType=$mimeType]")
    // for images, just copy the image to the thumbnail repo as the thumbnail
    if (mimeType.startsWith("image/")) checkStore(
      ThumbnailBucketName, Media.thumbnailId(mediaId), getSignedUrl(mediaId))
    // for videos, extract the thumbnail and audio track from the video data
    else if (mimeType.startsWith("video/")) checkStoreAndExtract(
      mediaId, getSignedUrl(mediaId), true, true)
  }

  /** Determines the duration of the specified media, in seconds. The media will be downloaded from
    * S3, run through ffmpeg to determine its duration, and then deleted. The media must refer to a
    * video. */
  def getDuration (mediaId :String) :Future[Int] = {
    val temp = Files.createTempFile("duration-", s"-${mediaId}")
    val (_, dres) = download(getSignedUrl(mediaId), temp)
    val res = dres.flatMap { file => Local.extractVideoDuration(file.path).map(dd => dd.toInt) }
    res.onComplete { _ => Files.delete(temp) }
    res
  }

  /** Downloads the video identified by `videoId` and `audioId`, trims it to the segment identified
    * by `start` and `end`, and creates new media objects with the result.
    * @param videoId the id of the video to trim.
    * @param audioIdOpt the (optional) id of the separate audio track that matches the video.
    * @param start the starting offset in seconds.
    * @param end the ending offset in seconds.
    * @return a `Media` record representing the newly created trimmed video. */
  def trimVideo (videoId :String, audioIdOpt :Option[String], start :Int, end :Int) :Future[Media] = {
    def trimAndUpload (mediaId :String, mimeType :String) = {
      val source = Files.createTempFile("trim-", s"-${mediaId}")
      val trimmedId = Util.deriveId(mediaId, s"trimmed:mediaId:$start:$end")
      val trimmed = Files.createTempFile("trim-", s"-${trimmedId}")
      val trimmedUrl = s"trimmed:${mediaId}:${start}:${end}"
      val trimmedMedia = Media(trimmedId, mimeType, trimmedUrl, duration = end-start)
      check(MediaBucketName, trimmedId).flatMap(_ match {
        case S3Status.Uploaded =>
          log.info(s"Reusing already trimmed media [id=$mediaId, trimmed=$trimmedId]")
          Future.successful(trimmedMedia)
        case _ =>
          for (
            sfile <- download(getSignedUrl(mediaId), source)._2 ;
            tsize <- Local.trimMedia(source, start, end, trimmed) ;
            _ <- upload(MediaBucketName, trimmedId, Local.File(trimmedUrl, tsize, mimeType, trimmed))._2
          ) yield trimmedMedia
      })
    }

    audioIdOpt match {
      case Some(audioId) =>
        log.info(s"Trimming video [id=$videoId, audio=$audioId, start=$start, end=$end]")
        for (video <- trimAndUpload(videoId, Util.guessMimeType(videoId, "video/mp4")) ;
             audio <- trimAndUpload(audioId, Util.guessMimeType(audioId, "audio/mp3")))
        yield video.copy(audio = audio)
      case None =>
        log.info(s"Trimming video [id=$videoId, start=$start, end=$end]")
        trimAndUpload(videoId, Util.guessMimeType(videoId, "video/mp4"))
    }
  }

  private enum S3Status { case Uploaded, Missing, Failed }

  private def check (bucket :String, id :String) :Future[S3Status] =
    s3client.headObject(hh => hh.bucket(bucket).key(id).build()).asScala.transform(
      res => util.Success(res match {
        case util.Success(rsp) =>
          if (bucket == MediaBucketName) addStatus(
            id, CompletedStatus(id, Progress(rsp.contentLength, rsp.contentLength)))
          S3Status.Uploaded
        case util.Failure(err) => if (isNoSuchKey(err)) S3Status.Missing else {
          log.log(Level.WARNING, s"Media check failed [bucket=$bucket, id=$id]", err)
          if (bucket == MediaBucketName) markFailed(id, err)
          S3Status.Failed
        }
      }))

  // exceptions end up wrapped inside concurrent.CompletionException, so we have to unpack
  private def isNoSuchKey (err :Throwable) :Boolean = err match {
    case nsk :NoSuchKeyException => true
    case null => false
    case _ => isNoSuchKey(err.getCause)
  }

  private def openConnection (url :String) :HttpURLConnection = {
    val conn = URL(url).openConnection.asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    conn.setDoOutput(false)
    conn.setConnectTimeout(5000)
    conn.setReadTimeout(30000)
    conn
  }

  // we manage the statuses table using old school Java synchronization because we maintain a
  // mutable list of webhooks inside a pending status which will be notified when the media
  // download completes or fails; so we need to mutate both the statuses table and individual
  // statuses while inside the same lock
  private def getStatus (id :String) = statuses.synchronized { statuses.get(id) }

  private def addStatus (id :String, status :Status) :Unit = statuses.synchronized {
    // if we have a preflight status for this media, we need to migrate its webhooks (if any)
    statuses.get(id) match {
      case preflight :PreflightStatus =>
        preflight.webhooks.foreach { url => status.addWebhook(url) }
      case _ => // nothing doing
    }
    statuses.put(id, status)
  }

  private def markComplete (id :String) :Status = statuses.synchronized {
    statuses.get(id) match {
      case null => null
      case status =>
        val complete = status.toComplete
        statuses.put(id, complete)
        queueWebhooks(id, status.webhooks, complete)
        complete
    }
  }

  private def markFailed (id :String, err :Throwable) :Unit = statuses.synchronized {
    val oldStatus = statuses.get(id)
    val newStatus = FailedStatus(id, err)
    statuses.put(id, newStatus)
    if (oldStatus != null) queueWebhooks(id, oldStatus.webhooks, newStatus)
  }

  // note: this just queues up zero or more async jobs, so it's OK that we call it while holding
  // the statuses table lock
  private def queueWebhooks (id :String, urls :List[String], finalStatus :Status) :Unit = {
    urls foreach { url => Webhook.queue(url, finalStatus) }
  }

  private def download (url :String, file :Path) :(() => Progress, Future[Local.File]) = {
    val sent = AtomicLong(0)
    val total = AtomicLong(0)
    (() => Progress(sent.get, total.get), Future {
      log.info(s"Downloading media [url=$url, file=$file]")
      val conn = openConnection(url)
      val (responseCode, contentLength, contentType) = blocking {
        (conn.getResponseCode(), conn.getContentLengthLong(), conn.getContentType())
      }
      total.set(contentLength)

      if (responseCode != 200) throw Exception(s"Failed to download media [url=$url, rcode=$responseCode]")

      blocking {
        val fileOut = Files.newOutputStream(file, StandardOpenOption.TRUNCATE_EXISTING)
        try Transfer.copyStreams(conn.getInputStream, fileOut, sent)
        finally conn.disconnect()
      }

      Local.File(url, Files.size(file), contentType, file)
    })
  }

  private def upload (bucket :String, id :String, file :Local.File) :(() => Progress, Future[Unit]) = {
    log.info(s"Uploading media [bucket=$bucket, id=$id, size=${file.contentLength}]")
    val upload = s3xfer.upload(
      up => up.putObjectRequest(
        put => put.bucket(bucket)
                  .key(id)
                  .contentType(file.contentType)
                  .metadata(Collections.singletonMap("Source-URL", file.url))
                  .build()).
        requestBody(AsyncRequestBody.fromFile(file.path)).
        build())
    val upRes = upload.completionFuture.asScala
    upRes.onComplete(_ match {
      case util.Success(r) => log.info(s"Storage complete [id=$id, result=$r]")
      case util.Failure(err) => log.info(s"Storage failed [id=$id, err=$err]")
    })
    (() => Progress(upload.progress.snapshot.transferredBytes, file.contentLength), upRes.map(_ => ()))
  }

  private val delayTimer = new Timer()
  private def retry (op :() => Unit) = delayTimer.schedule(new TimerTask() {
    override def run () = op()
  }, 5000)

  private def checkStore (bucket :String, id :String, url :String) :Unit = {
    // check whether we're in the process of storing this media or have already stored it; if not,
    // put a preflight status in place to denote that we now _are_ in the process of storing it
    if (!statuses.synchronized {
      if (statuses.containsKey(id)) false
      else {
        addStatus(id, PreflightStatus(id))
        true
      }
    }) return

    def fail (responseCode :Int, attempt :Int, err :Throwable) :Unit = {
      // if the HTTP response code was Forbidden, don't bother retrying
      if (attempt < MaxRetries && responseCode != 403) retry(() => store(attempt+1))
      else markFailed(id, err)
    }

    def store (attempt :Int = 0) :Unit = try {
      log.info(s"Storing media [bucket=$bucket, id=$id, url=$url]")
      val conn = openConnection(url)
      val (responseCode, contentLength, contentType) = blocking {
        (conn.getResponseCode(), conn.getContentLengthLong(), conn.getContentType())
      }
      if (responseCode != 200) {
        log.warning(s"Failed to start download [id=$id, rcode=$responseCode]")
        val msg = if (responseCode == 403) "The host of that media forbids us from downloading it"
                  else s"Failed to download media ($responseCode)"
        fail(responseCode, attempt, Exception(msg))
      }
      else if (contentLength <= 0) {
        log.info(s"Missing content-length, reverting to slower download+upload [id=$id]")
        storeHarder(bucket, id, url) // sigh
      }
      else {
        val upload = s3xfer.upload(
          up => up.putObjectRequest(
            put => put.bucket(bucket)
                      .key(id)
                      .contentType(contentType)
                      .metadata(Collections.singletonMap("Source-URL", url))
                      .build()).
            requestBody(AsyncRequestBody.fromInputStream(
              conn.getInputStream, contentLength, execService)).
            build())
        val upres = upload.completionFuture.asScala
        addStatus(id, new Status(id) {
          def progress = Progress(upload.progress.snapshot.transferredBytes, contentLength)
          def result = upres.map(_ => getSignedUrl(id)).value
        })
        upres.onComplete(res => {
          res match {
            case util.Success(r) =>
              log.info(s"Storage complete [bucket=$bucket, id=$id, result=$r]")
              markComplete(id)
            case util.Failure(err) =>
              log.info(s"Storage failed [bucket=$bucket, id=$id, err=$err]")
              fail(500, attempt, err)
          }
          conn.disconnect()
        })
      }
    } catch {
      case e :Throwable =>
        log.log(Level.WARNING, s"Failed to store media [id=$id]", e)
        fail(500, attempt, e)
    }

    // if our check for already cached media is disabled, start caching immediately
    if (disableReuse) store()
    // otherwise, first check whether the media has already been downloaded to S3
    else check(bucket, id).map(_ match {
      case S3Status.Uploaded => log.info(s"Reusing already cached media [bucket=$bucket, id=$id, url=$url]")
      case S3Status.Missing => store()
      case S3Status.Failed => // check will have reported and noted the failure
    })
  }

  private def checkStoreAndExtract (
    mediaId :String, mediaUrl :String, extractAudio :Boolean, extractThumb :Boolean
  ) :Unit = {
    def eitherIs (mm :S3Status, aa :S3Status, qq :S3Status) = mm == qq || aa == qq
    // check whether the main media or audio media are already uploaded; skip completed steps
    val main = check(MediaBucketName, mediaId)
    val audio = if (extractAudio) check(MediaBucketName, Media.extractedAudioId(mediaId))
                else Future.successful(S3Status.Uploaded)
    for (mm <- main ; aa <- audio) {
      if (eitherIs(mm, aa, S3Status.Failed)) log.warning("Aborting store due to check failure [id=${m.id}]")
      else if (eitherIs(mm, aa, S3Status.Missing)) storeAndExtract(
        mediaId, mediaUrl, mm == S3Status.Missing, aa == S3Status.Missing, extractThumb)
      else log.info(s"Reusing already cached media [id=${mediaId}]")
    }
  }

  // Instagram sometimes does not return a content-length header, and we can't stream directly
  // from the Instagram download to the S3 upload unless we know the content length in advance.
  // So we have to download the media and then upload to S3... yay.
  private def storeHarder (bucket :String, id :String, url :String) :Unit = {
    val mainUp = AtomicReference[() => Progress](null)
    val temp = Files.createTempFile("store-", s"-${id}")

    val (dprog, dres) = download(url, temp)
    val res = dres.flatMap { file =>
      val (upprog, upres) = upload(bucket, id, file)
      mainUp.set(upprog)
      upres
    }
    res.onComplete(_ match {
      case util.Success(_) => markComplete(id)
      // if the upload to S3 fails, something serious is wrong, not much use in retrying
      case util.Failure(err) => markFailed(id, err)
    })
    res.onComplete { _ => Files.delete(temp) }

    addStatus(id, new Status(id) {
      def progress = mainUp.get match {
        // if we're still downloading, adapt the download progress (x2 the total)
        case null => dprog().map(p => Progress(p.transferred, p.total*2))
        // otherwise we're uploading, so shift everything up by total
        case up => up().map(p => Progress(p.total+p.transferred, p.total*2))
      }
      def result = res.map(_ => getSignedUrl(id)).value
    })
  }

  private def storeAndExtract (
    mediaId :String, mediaUrl :String, uploadMain :Boolean, extractAudio :Boolean, extractThumb :Boolean,
    attempt :Int = 0
  ) :Unit = {
    val mainUp = AtomicReference[() => Progress](null)
    val temp = Files.createTempFile("store-", s"-${mediaId}")

    val audioId = Media.extractedAudioId(mediaId)
    val audioUp = AtomicReference[() => Progress](null)

    // start the various async steps going
    val (dprog, dres) = download(mediaUrl, temp)

    // if the download fails, then we will attempt to retry from the beginning
    dres.recover {
      case err :Throwable =>
        if (attempt < MaxRetries) retry(() => storeAndExtract(
          mediaId, mediaUrl, uploadMain, extractAudio, extractThumb, attempt+1))
        else markFailed(mediaId, err)
    }

    // if/when the download succeeds, do our extractions and upload the results
    val res = dres.flatMap { file =>
      val ops = collection.mutable.Buffer[Future[Unit]]()

      def handleUploadComplete (id :String)(res :util.Try[Unit]) = res match {
        case util.Success(_) =>
          // If we completed the audio with a total size of -1, that indicates that we found no
          // audio track in the video. We fail with a special error code to that the detect webapp
          // knows to clean up its metadata and note that the video has no audio track.
          def audioSize = Option(audioUp.get).map(_.apply().total) getOrElse 0L
          if (id == audioId && audioSize == -1) markFailed(id, new Exception(Result.NoAudioTrackError))
          else markComplete(id)
        // if the upload to S3 fails, something serious is wrong, not much use in retrying
        case util.Failure(err) => markFailed(id, err)
      }

      if (uploadMain) {
        val (upprog, upres) = upload(MediaBucketName, mediaId, file)
        mainUp.set(upprog)
        upres.onComplete(handleUploadComplete(mediaId))
        ops += upres
      }

      if (extractAudio) {
        val audioTemp = Files.createTempFile("store-", s"-$audioId")
        val audup = Local.checkHasAudioTrack(temp).flatMap { hasAudio =>
          if (hasAudio) Local.extractAudioTrack(temp, mediaUrl, audioTemp).flatMap { file =>
            val (audprog, audres) = upload(MediaBucketName, audioId, file)
            audioUp.set(audprog)
            audres
          } else {
            log.info(s"Video has no audio track, skipping extract [id=$mediaId]")
            audioUp.set(() => Progress(-1, -1))
            Future.successful(())
          }
        }
        ops += audup
        audup.onComplete(handleUploadComplete(audioId))
        audup.onComplete { _ => Files.delete(audioTemp) }
      }

      if (extractThumb) {
        val thumbnailId = Media.thumbnailId(mediaId)
        val thumbTemp = Files.createTempFile(thumbnailId, ".jpg")
        val thumbup = Local.extractVideoThumbnail(temp, mediaUrl, thumbTemp).flatMap { file =>
          upload(ThumbnailBucketName, thumbnailId, file)._2
        }
        ops += thumbup
        thumbup.onComplete { _ => Files.delete(thumbTemp) }
      }

      Future.sequence(ops)
    }

    res.onComplete { _ => Files.delete(temp) }

    if (uploadMain) addStatus(mediaId, new Status(mediaId) {
      def progress = mainUp.get match {
        // if we're still downloading, adapt the download progress (x2 the total)
        case null => dprog().map(p => Progress(p.transferred, p.total*2))
        // otherwise we're uploading, so shift everything up by total
        case up => up().map(p => Progress(p.total+p.transferred, p.total*2))
      }
      def result = res.map(_ => getSignedUrl(mediaId)).value
    })

    // for audio status, we just report 0/0 until we get to the upload phase
    if (extractAudio) addStatus(audioId, new Status(audioId) {
      def progress = audioUp.get match {
        case null => Progress(0, 0)
        case up => up()
      }
      def result = res.map(_ => getSignedUrl(audioId)).value
    })
  }

  private val cacheUrlExpireMinutes = 30

  /** Obtains a URL that can be used to download the media in the S3 object identified by `key`.
    * @return the signed URL.
    */
  def getSignedUrl (key :String) :String = {
    val req = GetObjectPresignRequest.builder().
      signatureDuration(java.time.Duration.ofMinutes(cacheUrlExpireMinutes)).
      getObjectRequest(GetObjectRequest.builder().bucket(MediaBucketName).key(key).build()).
      build()
    s3presigner.presignGetObject(req).url.toExternalForm
  }

  /**
    * Obtains a URL that can be used to upload the media to the S3 object identified by `key`.
    * @return the signed URL.
    */
  def getPutSignedUrl (key :String) :String = getPutSignedUrl(MediaBucketName, key)

  private def getPutSignedUrl (bucket :String, key :String) :String = {
    val req = PutObjectPresignRequest.builder().
      signatureDuration(java.time.Duration.ofMinutes(cacheUrlExpireMinutes)).
      putObjectRequest(PutObjectRequest.builder().bucket(bucket).key(key).build()).
      build()
    s3presigner.presignPutObject(req).url.toExternalForm
  }
}
