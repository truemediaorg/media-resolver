package mediares

import scala.concurrent.Future

import upickle.default._
import sttp.client4._

object TikTokVideo {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = java.util.logging.Logger.getLogger("tiktokvideo")
  private val tiktokRegexes = Seq(
    "^https://www.tiktok.com/\\S+/video/\\d+.*".r,
    "^https://www.tiktok.com/t/\\S+/".r,
    "^https://vm.tiktok.com/\\S+/".r)

  object V1 {
    case class Video (
      noWatermark :String,
      watermark :String,
      cover :String,
      dynamic_cover :String,
      origin_cover :String,
      width :Int,
      height :Int,
      durationFormatted :String,
      duration :Int,
      ratio :String
    ) derives ReadWriter {
      def toMedia = Media.video(
        Util.guessMimeType(url, "video/unknown"), url, width, height, 0, duration, Option(cover))
      private def url = watermark // TODO: use noWatermark if we have it?
    }

    case class Author (
      id :String,
      name :String,
      unique_id :String,
      signature :String,
      avatar :String,
      avatar_thumb :String
    ) derives ReadWriter

    case class Response (
      id :String,
      title :String,
      url :String,
      video :Video,
      author :Author
    ) derives ReadWriter

    def decode (res :Either[String, String]) :Option[Response] = res.fold(
      err => { println(s"TikTok v1 API failure: $err") ; None },
      json => try Some(read[Response](json)) catch {
        case err :Throwable => println(s"Failed to decode v1 API response [json=$json, err=$err]") ; None
      }
    )
  }

  object V2 {
    case class Author (
      avatar :String,
      nickname :String
    ) derives ReadWriter

    case class Result (
      `type`: String,
      author :Author,
      desc :String,
      music :String,
      video1 :String,
      video2 :String,
      video_hd :String,
      video_watermark :String
    ) derives ReadWriter {
      // these URLs don't have a mime-type or extension, but they seem to usually be MP4, so we
      // fall back to that in the absence of other information
      def toMedia = Media.video(Util.guessMimeType(video_hd, "video/mp4"), video_hd, 0, 0, 0, 0, None).
        copy(audio = Media.audio(Util.guessMimeType(music, "audio/unknown"), music))
    }

    case class Response (
      status :Int,
      result :Result
    ) derives ReadWriter

    def decode (res :Either[String, String]) :Option[Response] = res.fold(
      err => { println(s"TikTok v2 API failure: $err") ; None },
      json => try Some(read[Response](json)) catch {
        case err :Throwable => println(s"Failed to decode v2 API response [json=$json, err=$err]") ; None
      }
    )
  }

  object V4 {
    case class Author (
      username :String,
      url :String,
      avatar :String,
      nickname :String,
    ) derives ReadWriter

    case class Content (noWatermark :String, watermark :String) derives ReadWriter {
      def toMedia(optThumbnailUrl :Option[String]) = Media.video(Util.guessMimeType(url, "video/unknown"), url, 0, 0, 0, 0, optThumbnailUrl)
      private def url = watermark // TODO: use noWatermark if we have it?
    }

    case class Stats (
      playCount :String,
      likeCount :String,
      commentCount :String,
      saveCount :String,
      shareCount :String,
    ) derives ReadWriter

    case class Audio (name :String, url :String) derives ReadWriter {
      def toMedia = Media.audio(Util.guessMimeType(url, "audio/unknown"), url)
    }

    case class TTResult (
      `type` :String,
      desc :String,
      author :Author,
      content :Content,
      stats :Stats,
      audio :Audio = null,
    ) derives ReadWriter {
      def toMedia(optThumbnailUrl :Option[String]) = Option(audio) match {
        case Some(audio) => {
          val audio = this.audio.toMedia
          // sometimes the TikTok resolver returns the video track as the audio track; only trust the
          // audio track if its mime-type is audio
          if (audio.mimeType.startsWith("audio/")) content.toMedia(optThumbnailUrl).copy(audio = audio)
          // TODO: in this case we need to extract the audio track oursevles
          else content.toMedia(optThumbnailUrl)
        }
        case None => content.toMedia(optThumbnailUrl)
      }
    }

    case class Response (status: Int, result :TTResult = null, error :String = null) derives ReadWriter

    def decode (res :Either[String, String]) :Option[Response] = res.fold(
      err => { println(s"TikTok v4 API failure: $err") ; None },
      json => try Some(read[Response](json)) catch {
        case err :Throwable => println(s"Failed to decode v4 API response [json=$json, err=$err]") ; None
      }
    )
  }

  /** Decodes a Tikly API response, returning a resolved `Result` if it is a valid response, or a
    * failure `Result` with the reason for failure if it is an error response.
    */
  def decodeMedia (v1 :Option[V1.Response], v1json :String, v4 :Option[V4.Response], v4json :String) :Result = {
    // if we have a v4 response, use that as the base and then get the thumbnail from the v1
    v4 match {
      case Some(V4.Response(200, result, _)) =>
        val maybeCoverUrl = v1 match {
          case Some(rsp) => Some(rsp.video.cover)
          case _ => None
        }
        Result.resolved(Seq(result.toMedia(maybeCoverUrl)), v4json)
      case _ =>
        v1 match {
          case Some(rsp) => Result.resolved(Seq(rsp.video.toMedia), v1json)
          case _ => Result.failure(
            "TikTok media extractor temporarily unavailable.", s"{ v1: $v1json, v4: $v4json }")
        }
    }
  }

  def decodeMedia (v2 :Option[V2.Response], v2json :String) :Result = {
    v2 match {
      case Some(V2.Response(200, result)) => Result.resolved(Seq(result.toMedia), v2json)
      case _ => Result.failure("TikTok media extractor temporarily unavailable.", v2json)
    }
  }

  /** Resolves the media metadata for the specified TikTok `url`.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (url :String) :Future[Result] = {
    import sttp.client4.upicklejson.default._
    log.fine(s"Resolving TikTok post [url=$url]")

    val tiklyApi = "https://api.tiklydown.eu.org/api"
    // TEMP: use v2 until v1/v4 are working again

    // // we aggregate information from multiple API endpoints, also using multiple endpoints to
    // // recover if one of the endpoints is currently failing
    // val v1Req = basicRequest.get(uri"$tiklyApi/download?url=$url").send(backend)
    // val v4Req = basicRequest.get(uri"$tiklyApi/download/v4?url=$url").send(backend)
    // for {
    //   (v1, v1json) <- v1Req.map(rsp => (V1.decode(rsp.body), rsp.body.fold(l => l, r => r)))
    //   (v4, v4json) <- v4Req.map(rsp => (V4.decode(rsp.body), rsp.body.fold(l => l, r => r)))
    // } yield decodeMedia(v1, v1json, v4, v4json)

    val v2Req = basicRequest.get(uri"$tiklyApi/download/v2?url=$url").send(backend)
    for {
      (v2, v2json) <- v2Req.map(rsp => (V2.decode(rsp.body), rsp.body.fold(l => l, r => r)))
    } yield decodeMedia(v2, v2json)
  }

  /** Returns `true` if `url` appears to be a TikTok post URL, `false` otherwise. */
  def isTikTokUrl (url :String) :Boolean = tiktokRegexes.exists(_.matches(url))

  /** Resolves the streaming media metadata for `url` if it represents a TikTok page.
    * @return `None` if `url` is not a TikTok page, or `Some` future that resolves to a list of
    * media URLs for the video, or an error explaining failure.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    if (isTikTokUrl(url)) Some(resolveMedia(url)) else None
}
