package mediares

import scala.concurrent.{Await, Future, Promise, blocking}
import scala.concurrent.duration.Duration

import java.io.{BufferedReader, InputStreamReader, IOException}
import java.nio.file.{Files, Paths, Path, StandardOpenOption, SimpleFileVisitor, FileVisitResult}
import java.nio.file.attribute.BasicFileAttributes
import java.util.logging.{Logger, Level}
import java.util.concurrent.atomic.{AtomicReference}

import upickle.default._

object YTDLP {

  import Pool.ctx
  private val log = Logger.getLogger("yt-dlp")

  /** Passes the URL to `yt-dlp` and has it download the video, audio and thumbnail data.
    * @return a list of media found on the page or a failed future with error explanation.
    */
  def resolveMedia (url :String) :Future[Result] = {
    val tempDir = Files.createTempDirectory("mediares")
    val result = Promise[Result]()
    val media = AtomicReference[Media](null)
    val parser = new OutputParser()

    // This doesn't fit into our existing model of "get the URLs and return the resolve response and
    // then go off and download the media in the background".

    // So we have to do more complex coordination:
    // 1. Start the yt-dlp process and parse its output as it comes
    // 2. When the output seems to indicate success, compute Media and a Result from it
    // 3. Let yt-dlp handle the download of the media and then hand off to Storage for the S3
    // upload; presently only the upload will report progress, but maybe some day we can parse the
    // output of yt-dlp to convert its progress indicators into progress that we can report to the
    // website
    val ytdlpRes = Jobs.startJob({
      // start the yt-dlp process
      val proc = new ProcessBuilder(
        "yt-dlp",
        "--keep-video", "--write-thumbnail", "--write-info-json", "--no-playlist",
        "--progress-template", "download-title:%(info.id)s-%(progress.eta)s",
        "-S", "height:480", // pick best 480p format (to avoid huge 720p or 1080p videos)
        "-S", "ext", // and pick "best" extension (video: mp4 > mov > webm > flv; audio: m4a > aac > mp3)
        "-o", "%(id)s.%(ext)s",
        url
      ).directory(tempDir.toFile).redirectErrorStream(true).start()

      // this will be called with each line of output from the yt-dlp process
      def onOutput (line :String) = parser.onOutput(line) match {
        case Some(gmd :GotMetadata) =>
          log.info(s"yt-dlp resolved metadata [url=$url, meta=${gmd.metadataFile}]")
          val json = Files.readString(tempDir.resolve(gmd.metadataFile))
          val info = parseInfo(json, gmd.videoFormat, gmd.audioFormat)
          info match {
            case Result.Resolved(_, Seq(mm), _, _) =>
              Storage.preuploadMedia(mm.id, mm.audio.id)
              media.set(mm)
            case _ => log.info(s"yt-dlp failed to resolve media [result=$result]")
          }
          result.success(info)
        case _ => // nothing to see here, move it along
      }

      // start up a thread that will read the output from yt-dlp line by line
      val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
      val reporter = new Thread() {
        override def run () = {
          var line :String = ""
          while (line != null) {
            line = reader.readLine()
            if (line != null) onOutput(line)
          }
        }
      }
      reporter.start()

      blocking { proc.waitFor() }
    })

    // catch any errors thrown by the yt-dlp future and turn those into resolution failure if we
    // haven't already reported a resolution result
    ytdlpRes.onComplete { _ match {
      case util.Success(_) => // all clear, nothing to do here
      case util.Failure(err) =>
        if (result.isCompleted) log.log(Level.WARNING, s"Failed to run yt-dlp [url=$url]", err)
        else result.failure(err)
    }}

    // when the yt-dlp process is finished, check the process return code, potentially extract the
    // audio track (we don't have yt-dlp do that because it picks weird audio formats), and then
    // hand everything off to Storage to be uploaded to S3
    val uploadRes = ytdlpRes.flatMap { code =>
      val mm = media.get()
      // if yt-dlp completed its download without error, start the upload
      if (mm != null && code == 0) {
        val videoFile = parser.videoFile(url, tempDir)
        val thumbFile = parser.thumbnailFile(url, tempDir) match {
          case Some(tf) => Future.successful(tf)
          case None => Local.extractVideoThumbnail(
            videoFile.path, url, tempDir.resolve(s"${Media.thumbnailId(mm.id)}.jpg"))
        }
        // if we need to extract the audio track, do that now
        val audioFile = parser.audioFile(url, tempDir) match {
          case Some(af) => Future.successful(af)
          case None => Local.extractAudioTrack(
            videoFile.path, url, tempDir.resolve(Media.extractedAudioId(mm.id)))
        }
        for (af <- audioFile ;
             tf <- thumbFile ;
             res <- Storage.uploadMedia(mm.id, videoFile, mm.audio.id, af, tf))
        yield res

      } else {
        if (code != 0) log.warning(s"yt-dlp process exited with failure code $code [url=$url]")
        else if (mm != null) log.info("yt-dlp yielded no media metadata [url=$url]")
        // if we already completed our promise, then we have no way to report further errors, but
        // if not, report media resolution failure
        if (!result.isCompleted) result.success(Result.failure("Failed to resolve media", parser.error))
        Future.successful(()) // nothing to wait for
      }
    }

    // once the S3 upload is complete, recursively delete the temp directory we ran yt-dlp in
    uploadRes.onComplete(res => {
      res.recover { err => log.log(Level.WARNING, s"Failed to upload media [url=$url]", err) }
      log.info(s"Cleaning download dir: $tempDir")
      Files.walkFileTree(tempDir, new SimpleFileVisitor[Path] {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }
        override def postVisitDirectory (dir :Path, exc :IOException) = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    })

    result.future
  }

  case class Format (
    format_id :String,
    ext :String,
    protocol :String,
    url :String,
    width :Int = 0,
    height :Int = 0,
    audio_ext :String = null,
    video_ext :String,
  ) derives ReadWriter {
    def fileUrl (dir :Path, id :String) = dir.resolve(s"${id}.f${format_id}.${ext}").toUri.toString
    def video (mediaId :String, duration :Int) = Media(mediaId, s"video/${ext}", url, width, height, 0, duration)
    def audio (mediaId :String) = Media(mediaId, s"audio/${ext}", url)
  }

  case class Thumbnail (url :String, id :String, width :Int = 0, height :Int = 0) derives ReadWriter

  case class Video (id :String, extractor :String, duration :Int = 0, formats :Seq[Format]) derives ReadWriter {
    private def mediaId (track :String, ext :String) = Util.hashUrl(s"ytdlp:$extractor:$track:$id") + "." + ext

    def videoMedia (formatId :String) = formats.find(ff => ff.format_id == formatId).map(
      ff => ff.video(mediaId("video", ff.ext), duration).makeCachedByResolver)
    def audioMedia (formatId :String) = formats.find(ff => ff.format_id == formatId).map(
      ff => ff.audio(mediaId("audio", ff.ext)).makeCachedByResolver)
    def extractedAudioMedia = Media(mediaId("audio", "mp3"), s"audio/mp3", "file://extracted").makeCachedByResolver
  }

  def parseInfo (json :String, videoFormat :String, audioFormat :Option[String]) :Result = try {
    val video = read[Video](json)
    video.videoMedia(videoFormat) match {
      case Some (videoMedia) =>
        val media = audioFormat.flatMap(af => video.audioMedia(af)) match {
          case Some(audioMedia) => videoMedia.copy(audio = audioMedia)
          // Note: if the yt-dlp output claimed it found an audio format, but then we failed to
          // find matching metadata in the json file, we'll end up claiming here that we're going
          // to extract the audio, but then we may actually see (and use) a separate audio stream
          // downloaded by yt-dlp. I think that should never happen because yt-dlp figures out what
          // audio stream to use based on the json file too, but... I'll just leave this note here.
          case None => videoMedia.copy(audio = video.extractedAudioMedia)
        }
        Result.resolved(Seq(media), json)

      case None =>
        log.warning("Failed to find metadata for video chosen format " +
                    s"[format=${videoFormat}, formats=${video.formats.map(_.format_id)}]")
        Result.failure("Failed to extract metadata", s"Failed to find matching format for $videoFormat")
    }

  } catch {
    case t :Throwable =>
      log.log(Level.WARNING, "Failed to parse media json", t)
      log.log(Level.WARNING, json)
      Result.failure("Failed to extract metadata", s"Failed to parse yt-dlp json: $t")
  }

  case class GotMetadata (
    videoId :String, videoFormat :String, audioFormat :Option[String], metadataFile :String
  )

  class OutputParser {
    private var videoId :String = null
    private var videoFormat :String = null
    private var audioFormat :String = null
    private var videoFileName :String = null
    private var audioFileName :String = null
    private var thumbnailFileName :String = null
    private var metadataFile :String = null
    private var errorMessage :String = null

    def videoFile (url :String, dir :Path) :Local.File = file(url, dir, videoFileName, "video/mp4")
    def audioFile (url :String, dir :Path) :Option[Local.File] =
      Option(audioFileName).map(af => file("audio:" + url, dir, af, "audio/mp3"))
    def thumbnailFile (url :String, dir :Path) :Option[Local.File] =
      Option(thumbnailFileName).map(tf => file("thumb:" + url, dir, tf, "image/jpg"))
    def error :String = errorMessage

    private def file (url :String, dir :Path, file :String, fallbackMimeType :String) = {
      val path = dir.resolve(file)
      val ext = Util.fileSuffixFromFilename(file) getOrElse ".dat"
      val contentLength = if (Files.exists(path)) Files.size(path) else 0
      Local.File(url, contentLength, Util.guessMimeTypeFromSuffix(ext, fallbackMimeType), path)
    }

    def onOutput (line :String) :Option[GotMetadata] = {
      downloadingWebpage.findFirstMatchIn(line).flatMap(mm => {
        videoId = mm.group(1)
        // println(s"Video id: $videoId")
        None
      }) orElse downloadingFormats.findFirstMatchIn(line).flatMap(mm => {
        mm.group(2).split("\\+", 2) match {
          case Array(video, audio) =>
            videoFormat = video
            audioFormat = audio
          case Array(merged) =>
            videoFormat = merged
        }
        // println(s"Formats: $videoFormat & $audioFormat")
        None
      }) orElse writingThumbnail.findFirstMatchIn(line).flatMap(mm => {
        thumbnailFileName = mm.group(2)
        // println(s"Thumbnail: ${mm.group(1)}: $thumbnailFile")
        None
      }) orElse writingMetadata.findFirstMatchIn(line).flatMap(mm => {
        metadataFile = mm.group(1)
        // println(s"Metadata: $metadataFile")
        None
      }) orElse downloadDestination.findFirstMatchIn(line).flatMap(mm => {
        val hadVideo = videoFileName != null
        val phase = mm.group(1)
        if (phase == "download") {
          if (videoFileName == null) videoFileName = mm.group(2)
          else if (audioFileName == null) audioFileName = mm.group(2)
          else log.warning(s"Have audio and video file but got another download? ${mm.group(1)}")
        } else if (phase == "ExtractAudio") {
          if (audioFileName == null) audioFileName = mm.group(2)
          // otherwise ignore it, we'll use the original separate audio track
        }
        // println(s"Video: $videoFile - Audio: $audioFile - $hadVideo")
        if (hadVideo) None
        else Some(GotMetadata(videoId, videoFormat, Option(audioFormat), metadataFile))
      }) orElse errorReport.findFirstMatchIn(line).flatMap(mm => {
        if (errorMessage != null) log.warning(s"Overwriting error: $errorMessage")
        errorMessage = mm.group(1)
        None
      }) orElse {
        // println(s"Unmatched: $line")
        None
      }
    }
  }

  private val downloadingWebpage = "([^ ]+): Downloading webpage".r
  private val downloadingFormats = "Downloading (\\d+) format\\(s\\): (.*)".r
  private val writingThumbnail = "Writing video thumbnail (\\d+) to: (.*)".r
  private val writingMetadata = "Writing video metadata as JSON to: (.*)".r
  private val downloadDestination = "\\[(.*)\\] Destination: (.*)".r
  private val errorReport = "ERROR: (.*)".r
}
