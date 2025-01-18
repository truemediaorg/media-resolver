package mediares

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import java.util.logging.{Logger, Level}

import upickle.default._
import sttp.client4._

import com.twitter.clientlib.ApiException
import com.twitter.clientlib.TwitterCredentialsBearer
import com.twitter.clientlib.model.{Media => TMedia, Video, Photo, AnimatedGif, Variant}
import com.twitter.clientlib.api.TwitterApi

object Instagram {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = Logger.getLogger("instagram")

  // https://www.instagram.com/p/CVdTp6krmlF/
  private val urlRegexes = Seq(
    ("^(https?://)?(www.)?instagram.com/p/(\\S+/)/".r, 3))

  private val permalinkRegex = "data-instgrm-permalink=\\\"(https://www.instagram.com/p/[^/]+/)".r

  private lazy val token = Env.getSecrets("VETRIC_INSTAGRAM_TOKEN")(0)

  private case class MediaInfo (
    url :String,
    width :Int,
    height :Int,
    id :String = null,
    `type` :Int = 0,
  ) derives ReadWriter {
    def media (id :String, mimeFallback :String, bitrate :Int = 0, duration :Int = 0) = {
      val mimeType = Util.guessMimeType(url, mimeFallback)
      Media(Util.hashUrl(s"instagram:$id:$mimeType") + Util.fileSuffix(url, mimeType),
            mimeType, url, width, height, bitrate, duration)
    }
  }

  trait MediaItem {
    def media_type :Int
    def media :Media
  }

  object MediaItem {
    implicit val mediaItemReadWriter :ReadWriter[MediaItem] = readwriter[ujson.Value].bimap(
      item => item match {
        case image :ImageItem => write(item)
        case video :VideoItem => write(item)
      },
      value => value("media_type").num.toInt match {
        case 1 => read[ImageItem](value)
        case 2 => read[VideoItem](value)
      }
    )
  }

  private case class ImageVersions (candidates :Seq[MediaInfo]) derives ReadWriter
  private case class ImageItem (
    id :String,
    media_type :Int,
    product_type :String,
    image_versions2 :ImageVersions,
  ) extends MediaItem derives ReadWriter {
    def media :Media = image_versions2.candidates.maxBy(_.width).media(id, "image/unknown")
  }

  private case class VideoItem (
    id :String,
    media_type :Int,
    product_type :String,
    video_versions :Seq[MediaInfo],
    video_codec :String = null,
    video_duration :Double
  ) extends MediaItem derives ReadWriter {
    def media :Media = video_versions.maxBy(_.width).media(id, "video/mp4", 0, video_duration.toInt)
  }

  private case class CarouselItem (
    id :String,
    media_type :Int,
    product_type :String,
    carousel_media_count :Int,
    carousel_media :Seq[MediaItem]
  ) derives ReadWriter {
    def media :Seq[Media] = carousel_media.map(_.media)
  }

  private def decodePostItem (obj :ujson.Obj) :Seq[Media] = try {
    obj.value.get("media_type").flatMap(_.numOpt).map(_.toInt) match {
      case Some(1) => Seq(read[ImageItem](obj).media)
      case Some(2) => Seq(read[VideoItem](obj).media)
      case Some(8) => read[CarouselItem](obj).media
      case n =>
        log.warning(s"Unknown Instagram media_type $n")
        Seq()
    }
  } catch {
    case e :Throwable =>
      log.log(Level.WARNING, "Failed to parse post: $obj", e)
      Seq()
  }

  case class PostMedia (
    status :String,
    num_results :Int,
    more_available :Boolean,
    items :ujson.Arr,
  ) derives ReadWriter

  def decodePostMedia (media :PostMedia) :Seq[Media] = Seq() ++ media.items.value.flatMap(_ match {
    case obj :ujson.Obj => decodePostItem(obj)
    case v =>
      log.warning(s"Invalid post media item? $v")
      Seq()
  })

  def parsePostMedia (json :String) = try read[PostMedia](json) catch {
    case e :Throwable => throw new Exception(s"Failed to parse: $json", e)
  }

  def fetchPostMedia (mediaId :String) :Future[String] = {
    basicRequest.
      header("X-API-Key", token).
      get(uri"https://api.vetric.io/instagram/v1/media/${mediaId}/info?media_id=${mediaId}").
      send(backend).
      map(_.body.fold(err => throw Exception(err), json => json))
  }

  case class Post (
    author_id :Long,
    media_id :String,
    can_view :Boolean = true,
    version :String = null,
    title :String = null,
    author_name :String = null,
    author_url :String = null,
    provider_name :String = null,
    provider_url :String = null,
    `type` :String = null,
    html :String = null,
    thumbnail_url :String = null,
    thumbnail_width :Int = 0,
    thumbnail_height :Int = 0,
    @upickle.implicits.key("width") rawWidth :ujson.Value = null,
    @upickle.implicits.key("height") rawHeight :ujson.Value = null,
  ) derives ReadWriter {
    private def numValue (v :ujson.Value) = v match {
      case ujson.Num(n) => n.toInt
      case _ => 0
    }
    def width = numValue(rawWidth)
    def height = numValue(rawHeight)
    def permalink = permalinkRegex.findFirstMatchIn(html) match {
      case Some(m) => m.group(1)
      case None => ""
    }
  }

  def parsePost (json :String) = try read[Post](json) catch {
    case e :Throwable => throw new Exception(s"Failed to parse: $json", e)
  }

  def resolvePost (url :String) :Future[Post] = basicRequest.
    header("X-API-Key", token).
    get(uri"https://api.vetric.io/instagram/v1/url-resolver?url=${url}").
    send(backend).
    map(_.body.fold(err => throw Exception(err), parsePost))

  /** Fetches the metadata for an Instagram post and resolves the media metadata therein.
    * @return a list of media or a failed future with error explanation.
    */
  def resolveMedia (url :String) :Future[Result] = resolvePost(url).flatMap { p =>
    if (!p.can_view) Future.successful(Result.failure("This Instagram post is private and cannot be analyzed."))
    else for (json <- fetchPostMedia(p.media_id))
    yield Result.resolved(decodePostMedia(parsePostMedia(json)), json, p.permalink)
  }

  def tryResolveMedia (url :String) :Option[Future[Result]] =
    if (url.contains("instagram.com/stories/")) Some(
      Future.successful(Result.failure("Instagram Stories are not supported yet.")))
    else if (url.contains("instagram.com/p/") ||
             // some reels are instagram.com/reel and some are instagram.com/username/reel
             url.contains("instagram.com/") && url.contains("/reel/")) Some(resolveMedia(url))
    else if (url.contains("instagram.com/")) Some(
      Future.successful(Result.failure("That Instagram URL format is not yet supported.")))
    else None
}
