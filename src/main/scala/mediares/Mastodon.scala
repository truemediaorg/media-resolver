package mediares

import scala.concurrent.Future

import upickle.default._
import sttp.client4._

object Mastodon {

  import Pool.ctx
  private val backend = DefaultFutureBackend()

  // An indirect URL is hosted by node A and refers to a user and post on node B, where in the
  // below URL, A = mastodon.gamedev.place and B = mastodon.social
  // https://mastodon.gamedev.place/@RustyBertrand@mastodon.social/111728190835688257
  private val indirectUrlRegex = "(https?://)?([^/]+)/@([^@]+)@([^/]+)/(\\d+)".r

  // A direct URL is hosted by node A and refers to a user and post on node A, like:
  // https://mastodon.gamedev.place/@badlogic/111772886977661740
  private val directUrlRegex = "(https?://)?([^/]+)/@([^@]+)/(\\d+)".r

  case class MediaMeta (
    width :Int,
    height :Int,
    frame_rate :String = null,
    duration :Double = 0,
    bitrate :Int = 0,
    size :String = null,
    aspect :Double = 0,
  ) derives ReadWriter

  case class MediaMetas (
    original :MediaMeta,
    small :MediaMeta = null,
  ) derives ReadWriter

  case class PostMedia (
    id :String,
    `type` :String, // image, video, ?
    url :String,
    preview_url :String = null,
    remote_url :String = null,
    preview_remote_url :String = null,
    text_url :String = null,
    description :String = null,
    blurhash :String = null,
    meta :MediaMetas = null,
  ) derives ReadWriter {
    /** Converts Mastodon's media metadata into our `Media` metadata record. */
    def toMedia = Option(this.meta).map(meta => meta.original) match {
      case None => Media(Util.computeId(mediaUrl, mimeType), mimeType, mediaUrl)
      case Some(mm) => Media(Util.computeId(mediaUrl, mimeType), mimeType, mediaUrl,
                             mm.width, mm.height, mm.bitrate, mm.duration.toInt)
    }
    private def mimeType :String = Util.guessMimeType(mediaUrl, s"${`type`}/unknown")
    // If a post originated on node A but is being viewed from node B, the `remote_url` will point
    // to the media on the originating node (A), and `url` will point to the media cached by node
    // B. We resolve the origin node's media so that we have a more canonical URL and don't end up
    // repeating analysis on a bunch of cached copies of an image or video.
    private def mediaUrl :String = Option(remote_url) getOrElse url
  }

  case class Post (
    id :String,
    created_at :String,
    url :String,
    replies_count :Int,
    favourites_count :Int,
    content :String,
    media_attachments :Seq[PostMedia],
  ) derives ReadWriter {
    def toMedia :Seq[Media] = media_attachments.map(_.toMedia)
  }

  def decodeMedia (json :String) :Seq[Media] = try read[Post](ujson.read(json)).toMedia catch {
    case e :Throwable => throw new Error(s"Parse failure: ${e.getMessage}\n$json")
  }

  def decodeResponse (server :String, json :String) :Result = {
    val res = ujson.read(json).obj
    res.get("error") match {
      case Some(error) => Result.failure(s"$server returned an error", error.toString)
      case None => Result.resolved(decodeMedia(json), json)
    }
  }

  /** Resolves the media metadata for the specified Mastodon post.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (serverHost :String, postId :String) :Future[Result] = {
    import sttp.client4.upicklejson.default._
    basicRequest.
      get(uri"https://${serverHost}/api/v1/statuses/${postId}").
      send(backend).
      map { _.body match {
        case Left(error) => throw Error(error)
        case Right(json) => decodeResponse(serverHost, json)
      }}
  }

  /** Fetches the metadata for a Mastodon post & extracts the media therein.
    * @return `None` if `url` is not a Mastodon post, otherwise the media metadata or a failed
    * future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] = serverInfoFromUrl(url) match {
    case Some(serverHost, postId) =>
      // make sure we don't erroneously match some other popular sites that use @username URLs
      if (serverHost.endsWith("tiktok.com") || serverHost.endsWith("twitter.com") ||
          serverHost == "x.com" || serverHost == "www.x.com") None
      else Some(resolveMedia(serverHost, postId))
    case _ => None
  }

  /** Extracts the server and post information from a Mastodon post URL.
    * @return `(serverHost, postId)` or `None` if `url` is not a Mastodon post URL.
    */
  def serverInfoFromUrl (url :String) :Option[(String, String)] =
    indirectUrlRegex.findFirstMatchIn(url).map(m => (m.group(2), m.group(5))) orElse
    directUrlRegex.findFirstMatchIn(url).map(m => (m.group(2), m.group(4)))
}
