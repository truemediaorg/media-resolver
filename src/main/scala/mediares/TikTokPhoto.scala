package mediares

import scala.concurrent.Future

import upickle.default._
import sttp.client4._

object TikTokPhoto{

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = java.util.logging.Logger.getLogger("tiktokphoto")
  private val tiktokRegexes = Seq(
    "^https://www.tiktok.com/\\S+/photo/\\d+.*".r)

  object V2 {
    case class Author (
      avatar :String = null,
      nickname :String
    ) derives ReadWriter

    case class Result (
      `type`: String,
      author :Author,
      images :Seq[String],
      music :String,
    ) derives ReadWriter {
      def toMedia = images.map(url => Media.image(Util.guessMimeType(url, "image/jpeg"), url))
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

  def decodeMedia (v2 :Option[V2.Response], v2json :String) :Result = {
    v2 match {
      case Some(V2.Response(200, result)) => Result.resolved(result.toMedia, v2json)
      case _ => Result.failure("TikTok slideshow media extractor temporarily unavailable.", v2json)
    }
  }

  /** Resolves the media metadata for the specified TikTok `url`.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (url :String) :Future[Result] = {
    import sttp.client4.upicklejson.default._
    log.fine(s"Resolving TikTok post [url=$url]")

    val tiklyApi = "https://api.tiklydown.eu.org/api"

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
