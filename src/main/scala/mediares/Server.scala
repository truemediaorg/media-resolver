package mediares

import scala.collection.JavaConverters._
import scala.concurrent.{Future, blocking}
import scala.util.{Success, Failure}
import java.util.logging.{Logger, Level}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.undertow.{Handlers, Undertow}
import io.undertow.server.{HttpHandler, HttpServerExchange}
import io.undertow.util.{Headers, Methods, HttpString}

import upickle.default._

object Server {

  import Pool.ctx
  private val log = Logger.getLogger("mediares")
  private val DisableCaching = Env.getBool("DISABLE_CACHING", false)

  val resolvers = Seq(
    Passthrough.tryResolveMedia,
    Reddit.tryResolveMedia,
    // these are handled by YTDLP now:
    // YouTube.tryResolveMedia,
    // TikTokVideo.tryResolveMedia,

    // this is now handled by TikTok:
    // TikTokPhoto.tryResolveMedia,
    TikTok.tryResolveMedia,
    TruthSocial.tryResolveMedia,
    Twitter.tryResolveMedia,
    Mastodon.tryResolveMedia,
    GoogleDrive.tryResolveMedia,
    Instagram.tryResolveMedia,
    Facebook.tryResolveMedia,
  )

  def resolveMedia (url :String) :Future[Result] = try {
    // only one of the resolvers will match the URL and attempt to produce a result
    val media = resolvers.flatMap(_.apply(url)).headOption getOrElse YTDLP.resolveMedia(url)
    media.map(_ match {
      case r :Result.Resolved => r.copy(media = r.media.map(m => {
        // start the caching of this media's various parts
        val mx = if (DisableCaching || m.isCachedByResolver) m else Storage.cacheMedia(m)
        // and remove the secret download URLs if we had any
        mx.clean
      }))
      case r => r
    })
  } catch {
    case err => Future.failed(err)
  }

  private def respond (exch :HttpServerExchange, code :Int, json :String) :Unit = {
    exch.setStatusCode(code)
    exch.getResponseHeaders.put(Headers.CONTENT_TYPE, "application/json")
    // TODO: maybe only allow from truemedia.org once we have that domain set up?
    exch.getResponseHeaders.put(HttpString("Access-Control-Allow-Origin"), "*");
    exch.getResponseSender.send(json)
    noteResponse(code)
  }

  private def respond (exch :HttpServerExchange, code :Int, result :Result) :Unit =
    respond(exch, code, result.toJson)

  private def respond (
    exch :HttpServerExchange, code :Int, error :Throwable, rspMsg :String, logMsg :String
  ) :Unit = error match {
    case error :Util.ReportException =>
      respond(exch, 500, Result.failure(error.getMessage, error.details))
    case error =>
      log.log(Level.WARNING, logMsg, error)
      respond(exch, 500, Result.failure(rspMsg, error.getMessage))
  }

  private def handleResolveRequest (exch :HttpServerExchange) = {
    if (exch.getRequestMethod != Methods.POST) respond(
      exch, 500, Result.failure("Please submit a POST request with the URL in the body."))
    else exch.getRequestReceiver.receiveFullString((ex, body) => {
      exch.dispatch()
      val url = body.trim() // the only thing in the POST body is the URL
      if (Jobs.atMaxJobs) {
        respond(exch, 503, Result.failure("Too many pending resolutions. Please try again later."))
      } else {
        log.info(s"Resolving: $url")
        resolveMedia(url).onComplete(_ match {
          case Success(result) => respond(exch, 200, result)
          case Failure(error) => respond(exch, 500, error, "Internal error", s"Failed to resolve $url")
        })
      }
    })
  }

  private def handleStatusRequest (
    exch :HttpServerExchange, fn :String => Future[Option[Status]]
  ) = exch.getQueryParameters.get("id") match {
    case null => respond(exch, 500, Result.failure("Missing 'id' parameter."))
    case jids =>
      exch.dispatch()
      val ids = jids.asScala.filter(_ != "")
      Future.sequence(ids.map(fn)).onComplete(_ match {
        case Failure(err) => respond(
          exch, 500, err, "Internal error", s"Failed to get status [ids=${ids}, error=${err.getMessage}]")
        case Success(results) =>
          // if a webhook URL was supplied, register it for the specified id(s)
          getParam(exch, "webhook", url => url).fold(_ => None, opt => opt).foreach { url =>
            for (id <- ids) Storage.registerWebhook(id, url)
          }
          val statusMap = ids.zip(results).map(res => (res._1 -> Status.toTransferStatus(res._2))).toMap
          respond(exch, 200, Result.progress(statusMap))
      })
  }

  private def handleGetDuration (exch :HttpServerExchange) = exch.getQueryParameters.get("id") match {
    case null => respond(exch, 500, Result.failure("Missing 'id' parameter."))
    case ids =>
      exch.dispatch()
      val id = ids.asScala.head
      Storage.getDuration(id).onComplete(_ match {
        case Failure(err) => respond(
          exch, 500, err, "Internal error", s"Failed to get duration [id=${id}, error=${err.getMessage}]")
        case Success(duration) => respond(exch, 200, Result.duration(duration))
      })
  }

  private def handleTrimVideo (exch :HttpServerExchange) = (
    requireParam(exch, "id", id => id),
    getParam(exch, "audioId", id => id),
    requireParam(exch, "start", Integer.parseInt),
    requireParam(exch, "end", Integer.parseInt)
  ) match {
    case (Right(id), Right(audioId), Right(start), Right(end)) =>
      exch.dispatch()
      Storage.trimVideo(id, audioId, start, end).onComplete(_ match {
        case Failure(err) => respond(
          exch, 500, err, "Internal error",
          s"Failed to get trim video [id=$id, start=$start end=$end error=${err.getMessage}]")
        case Success(video) => respond(exch, 200, Result.trimmed(video))
      })
    case (videoId, audioId, start, end) =>
      val msg = (Seq(videoId, audioId, start, end) collect {
        case Right(error) => error
      }).mkString("\n")
      respond(exch, 500, Result.failure(msg))
  }

  private def handleTransferRequest (exch :HttpServerExchange) = {
    def invalid () = respond(exch, 500, Result.failure(
      "Please submit a POST request with [key, source, dest], separated by newlines."))
    if (exch.getRequestMethod != Methods.POST) invalid()
    else exch.getRequestReceiver.receiveFullString((ex, body) => {
      body.split("\n").map(_.trim()) match {
        case Array(key, src, dest) =>
          exch.dispatch()
          log.info(s"Starting transfer: [key=$key, src=$src, dest=$dest]")
          Future { Transfer.transfer(key, src, dest) }.onComplete(_ match {
            case Success(status) =>
              respond(exch, 200, Result.progress(
                Map(key -> Result.TransferStatus(0, status.progress.total))))
            case Failure(error) =>
              respond(exch, 500, error, s"Internal error transferring '$key'.",
                      s"Failed to transfer [key=$key]")
          })

        case _ => invalid()
      }
    })
  }

  private def handleCreateFileUploadRequest (exch :HttpServerExchange) = {
    if (exch.getRequestMethod != Methods.POST) respond(
      exch, 500, Result.failure("Please submit a POST request with the filename in the body."))
    else exch.getRequestReceiver.receiveFullString((ex, body) => {
      exch.dispatch()
      val filename = body.trim()
      Util.fileSuffixFromFilename(filename) match {
        case None => respond(exch, 500, Result.failure(s"Invalid filename: '$filename' in body, must have a suffix."))
        case Some(suffix) => {
          // generate a random mediaId for the file to be uploaded later (no caching concept here)
          val mediaId = Util.hashUrl(java.util.UUID.randomUUID.toString()) + suffix
          log.info(s"Creating file upload presigned url for generated key: $mediaId")
          respond(exch, 200, write(Result.fileUpload(
                                      mediaId,
                                      Util.guessMimeTypeFromSuffix(suffix, "unknown"),
                                      Storage.getPutSignedUrl(mediaId))))
        }
      }
    })
  }

  private def handleFinalizeFileUpload(exch :HttpServerExchange) = {
    if (exch.getRequestMethod != Methods.POST) respond(
      exch, 500, Result.failure("Please submit a POST request with the JSON { mediaId, mimeType } in the body"))
    else exch.getRequestReceiver().receiveFullString((ex, body) => {
      val json = read[Map[String, String]](body)
      (json("mediaId"), json("mimeType")) match {
        case (mediaId, mimeType) if (mediaId == null || mimeType == null) =>
          // if we dont have required fields, return error
          respond(exch, 500, Result.failure("Please provide mediaId and mimeType in the body"))
        case (mediaId, mimeType) =>
          // otherwise just trust that the correct wheels are set in motion and return success
          Storage.finalizeUpload(mediaId, mimeType)
          respond(exch, 200, "")
      }
    })
  }

  private def requireParam[T] (exch :HttpServerExchange, name :String, convert :String => T) :Either[String, T] =
    getParam(exch, name, convert).flatMap(_ match {
      case Some(other) => Right(other)
      case None => Left(s"Missing '$name' parameter.")
    })

  private def getParam[T] (exch :HttpServerExchange, name :String, convert :String => T) :Either[String, Option[T]] = {
    val values = exch.getQueryParameters.get(name)
    if (values == null) Right(None)
    else try {
      Right(Some(convert(values.asScala.head)))
    } catch {
      case t :Throwable => Left(s"Invalid '$name' parameter.")
    }
  }

  private val reqCounts = ConcurrentHashMap[String, AtomicInteger]()
  private def note (path :String) :String = {
    val count = reqCounts.computeIfAbsent(path, _ => AtomicInteger(0))
    count.addAndGet(1)
    path
  }

  private val rspCounts = ConcurrentHashMap[Int, AtomicInteger]()
  private def noteResponse (code :Int) :Int = {
    val count = rspCounts.computeIfAbsent(code, _ => AtomicInteger(0))
    count.addAndGet(1)
    code
  }

  case class TransferStats (pending :Int, completed :Int) derives ReadWriter
  case class Stats (
    pending :Int,
    requests :Map[String, Int],
    responses :Map[Int, Int],
    storageStats :Status.Stats,
    transferStats :Status.Stats,
  ) derives ReadWriter

  private def handleRootRequest (exch :HttpServerExchange) = {
    import collection.JavaConverters.mapAsScalaMapConverter
    val stats = Stats(Jobs.pendingJobs.get,
                      reqCounts.asScala.mapValues(v => v.get).toMap,
                      rspCounts.asScala.mapValues(v => v.get).toMap,
                      Storage.stats, Transfer.stats)
    respond(exch, 200, write(stats))
  }

  def main (args :Array[String]) = {
    val (host, port) = ("0.0.0.0", 8080)

    val handler = new HttpHandler() {
      @Override def handleRequest (exch :HttpServerExchange) = note(exch.getRequestPath) match {
        case "/mediares/resolve" => handleResolveRequest(exch)
        case "/mediares/cache_status" => handleStatusRequest(exch, Storage.status)
        case "/mediares/get_duration" => handleGetDuration(exch)
        case "/mediares/trim_video" => handleTrimVideo(exch)
        case "/mediares/transfer" => handleTransferRequest(exch)
        case "/mediares/transfer_status" => handleStatusRequest(exch, Transfer.status)
        case "/mediares/create_file_upload" => handleCreateFileUploadRequest(exch)
        case "/mediares/finalize_file_upload" => handleFinalizeFileUpload(exch)
        case "/" | "/mediares/" => handleRootRequest(exch)
        case path =>
          log.info(s"Sent 404: '$path'")
          respond(exch, 404, Result.failure(s"Unknown endpoint: '$path'"))
      }
    }
    val shutdown = Handlers.gracefulShutdown(handler)
    val server = Undertow.builder.
      addHttpListener(port, host).
      setHandler(shutdown).
      build()
    // quiet a spurious warning generated by the Twitter SDK
    Twitter.createPlaceholderSDKProperties()
    server.start()
    log.info(s"Media Resolver listening on $host:$port")

    Runtime.getRuntime().addShutdownHook(new Thread() {
      override def run() = {
        // for some reason log messages are not shown if they occur here, so we just write to
        // stderr so that we have some feedback about the shutdown process
        System.err.println("Stopping server, waiting for pool to drain...")
        // stop the Undertow server and our thread pool
        shutdown.shutdown()
        Pool.pool.shutdown()
        // wait for all requests to complete and for our thread pool to drain
        shutdown.awaitShutdown()
        Pool.pool.awaitTermination(300, TimeUnit.SECONDS)
        // everyone has gone home, we can close up shop
        System.err.println("Shutdown process complete")
      }
    })
  }
}
