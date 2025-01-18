package mediares

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

import sttp.client4._
import sttp.model.StatusCode

object Generic {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val browser = JsoupBrowser()
  private val log = java.util.logging.Logger.getLogger("generic")

  private val bitchuteRegex = "^(https://)?(www.)?bitchute.com/video/.*".r
  private val imageMetas = Set("og:image", "twitter:image")

  def extractVideos (html :String) :Seq[Media] = {
    val doc = browser.parseString(html)
    for (video <- doc >> elementList("video") ; source <- video >?> element("source") ;
         url = source.attr("src"))
    yield Media.video(source.attrs.getOrElse("type", Util.guessMimeType(url, "video/unknown")), url)
  }

  // /** Looks for `og:image` or `twitter:image` meta tags and extracts them as media. `og:image` is
  //   * preferred to `twitter:image` if both exist.
  //   * @return zero or one media objects.
  //   */
  // def extractMetaImages (html :String) :Seq[Media] = {
  //   val doc = browser.parseString(html)
  //   def metas (nameKey :String, namePrefix :String) :Map[String, String] = Map() ++ (
  //     for (meta <- doc >> elementList("meta") ; name <- meta.attrs.get(nameKey)
  //          if (name.startsWith(namePrefix)))
  //     yield (name.substring(namePrefix.length) -> meta.attr("content")))

  //   val ogMetas = metas("property", "og:")
  //   val ogMedia = ogMetas.get("image") map { url =>
  //     val mimeType = Util.guessMimeType(url, "image/unknown")
  //     val width = ogMetas.get("image:width").map(_.toInt) getOrElse 0
  //     val height = ogMetas.get("image:height").map(_.toInt) getOrElse 0
  //     Media.image(mimeType, url, width, height)
  //   }

  //   val twitterMetas = metas("name", "twitter:")
  //   val twitterMedia = twitterMetas.get("image") map { url =>
  //     val mimeType = Util.guessMimeType(url, "image/unknown")
  //     Media.image(mimeType, url, 0, 0)
  //   }

  //   // prefer og: media to twitter: media as it might have width and height
  //   Seq() ++ (ogMedia orElse twitterMedia)
  // }

  /*** Fetches a social media post and extracts the <video> tag URLs contained therein.
    * @param url the post URL.
    * @return either an error message or a list of video URLs found in the post.
    */
  def resolveVideos (url :String) :Future[Result] =
    basicRequest.get(uri"${url}").send(backend).map(_.body match {
      case Left(error) => throw Exception(error)
      case Right(html) => Result.resolved(extractVideos(html))
    })

  def resolveGeneric (url :String) :Future[Result] = Util.tryParseUri(url) match {
    case Some(uri) =>
      def unsupported (details :String) = uri.host match {
        case Some(site) => Result.failure(s"$site is not a supported site.", details)
        case None => Result.failure(s"That site is not supported.", details)
      }
      basicRequest.get(uri).send(backend).map(res => {
        if (res.code == StatusCode.Ok) res.header("Content-Type") match {
          case Some(contentType) =>
            if (contentType.startsWith("text/html")) res.body match {
              case Left(error) => unsupported(error)
              case Right(html) => unsupported(null)
            }
            else if (contentType.startsWith("video/") ||
                     contentType.startsWith("image/") ||
                     contentType.startsWith("audio/")) Result.resolved(Seq(
              Media(Util.computeId(url, contentType), contentType, url)))
            else {
              log.info(s"Unhandled content-type [type=${contentType}, url=${url}]")
              Result.failure(s"Unknown content-type: $contentType")
            }
          case None =>
            log.info(s"No content-type [url=${url}]")
            Result.failure(s"Missing content-type")
        }
        else res.body match {
          case Left(error) => unsupported(error)
          case Right(content) => throw Exception(s"${res.code}: ${content}")
        }
      }).recover({
        case e :Throwable => Result.failure("Failed to contact website.", e.getMessage)
      })
    case None =>
      Future.successful(Result.failure("That is not a valid URL."))
  }

  /** If `url` matches one of our known "generic" social media sites, fetches the post HTML and
    * extracts the media in any `<video>` tags. Otherwise fetches the HTML and looks for `meta`
    * tags which describe images associated with the page. Not great, but a last resort.
    * @return a list of media found on the page or a failed future with error explanation.
    */
  def resolveMedia (url :String) :Future[Result] =
    if (bitchuteRegex.matches(url)) resolveVideos(url)
    else resolveGeneric(url)
}
