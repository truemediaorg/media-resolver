package mediares

import scala.concurrent.Future

import upickle.default._
import sttp.client4._

object TikTok {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = java.util.logging.Logger.getLogger("tiktokphoto")

  private val shortURLRegex = "^https://www.tiktok.com/t/\\S+/".r
  private val videoURLRegex = "^https://www.tiktok.com/\\S+/video/\\d+.*".r

  private val tiktokRegexes = Seq(
    "^https://www.tiktok.com/\\S+/photo/\\d+.*".r,
    shortURLRegex,
    videoURLRegex,
  )

  /** Resolves the media metadata for the specified TikTok `url`.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (url :String) :Future[Result] = {
    import sttp.client4.upicklejson.default._
    log.fine(s"Resolving TikTok post [url=$url]")

    if (shortURLRegex.matches(url)) {
      // handle the redirect to the actual video/photo URL
      basicRequest
        .followRedirects(false)
        .head(uri"$url")
        .send(backend)
        .flatMap { rsp =>
          if (rsp.code.code == 301) {
            val redirectUrl = rsp.headers("Location").head
            resolveMedia(redirectUrl)
          } else {
            Future.successful(
              Result.failure(
                "Unexpected non-redirect response for tiktok.com/t/ url",
                s"Response code: ${rsp.code.code}"
              )
            )
          }
        }
    } else if (videoURLRegex.matches(url)) {
      // delegate to YTDLP for video urls
      return YTDLP.resolveMedia(url)
    } else {
      // delegate to TikTokPhoto for photo urls
      return TikTokPhoto.resolveMedia(url)
    }
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
