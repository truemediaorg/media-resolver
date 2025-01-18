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

object Facebook {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = Logger.getLogger("facebook")

  private val mediaUrlRegexes = Seq(
    ("^(https?://)?(www.|m.)?(facebook).com/photo/\\?fbid=([0-9]+).*".r, 4),
    ("^(https?://)?(www.|m.)?(facebook).com/watch/\\?v=([0-9]+).*".r, 4),
    ("^(https?://)?(www.|m.)?(facebook).com/reel/([0-9]+).*".r, 4))

  private val urlRegex = "^(https?://)?(www.|m.)?(facebook).com/.*".r

  private lazy val token = Env.getSecrets("VETRIC_FACEBOOK_TOKEN")(0)

  // https://www.facebook.com/reel/7267334893381498
  // https://www.facebook.com/photo/?fbid=717077590580168&set=a.573638464924082
  // https://www.facebook.com/4/videos/751480670244834/
  // https://www.facebook.com/reel/1813596502473642
  // https://www.facebook.com/watch/?v=7058523417565227

  case class PostImage (uri :String, width :Int, height :Int) derives ReadWriter {
    def toMedia = Media.image(Util.guessMimeType(uri, "image/unknown"), uri, width, height)
  }

  case class PostMedia (
    __typename :String,
    id :String,
    image :PostImage = null,
    imageHigh :PostImage = null,
    playable_url :String = null,
    hd_playable_url :String = null,
    bitrate :Int = 0,
    hdBitrate :Int = 0,
    playable_duration_in_ms :Int = 0,
  ) derives ReadWriter {
    private def info :(String, Int) =
      Option(hd_playable_url) map(url => (url, hdBitrate)) getOrElse (playable_url, bitrate)
    private def asVideo = info match {
      case (url, bitrate) => Media.video(
        Util.guessMimeType(url, "video/unknown"), url, 0, 0, bitrate, playable_duration_in_ms/1000)
    }

    def media = __typename match {
      case "Video" => asVideo
      case "Photo" => imageHigh.toMedia
      case _ =>
        log.warning(s"Unknown media type ${__typename}")
        imageHigh.toMedia
    }
  }

  case class PostAttachment (url :String, target :ujson.Obj, media :PostMedia) derives ReadWriter {
    def toMedia = media.media
  }
  case class PostNode (id :String, url :String, attachments :Seq[PostAttachment]) derives ReadWriter
  case class PostData (node :PostNode) derives ReadWriter
  case class Post (data :PostData, extensions :ujson.Obj) derives ReadWriter {
    def media = data.node.attachments.map(_.toMedia)
  }

  def parsePost (json :String) = try read[Post](json) catch {
    case e :Throwable => throw new Exception(s"Failed to parse: $json", e)
  }

  def resolvePost (id :String) =
    basicRequest.
      header("x-api-key", token).
      header("x-updated-client-v12-3", "true").
      get(uri"https://api.vetric.io/facebook/v1/posts/node?node_id=${id}&transform=false").
      send(backend).
      map(_.body.fold(
        err => throw Exception(err),
        json => Result.resolved(parsePost(json).media, json)))

  case class MediaData (nodes :Seq[PostMedia]) derives ReadWriter
  case class MediaResponse (data :MediaData) derives ReadWriter {
    def media = data.nodes.map(_.media)
    def canonicalUrl (id :String) = data.nodes.headOption.flatMap(_.__typename match {
      case "Video" => Some(s"https://www.facebook.com/watch/?v=$id")
      case "Photo" => Some(s"https://www.facebook.com/photo/?fbid=$id")
      case _ => None
    })
  }

  def parseMedia (json :String) = try read[MediaResponse](json) catch {
    case e :Throwable => throw new Exception(s"Failed to parse: $json", e)
  }

  def resolveMedia (id :String) :Future[Result] =
    basicRequest.
      header("x-api-key", token).
      header("x-updated-client-v12-3", "true").
      post(uri"https://api.vetric.io/facebook/v1/posts/media/${id}?transform=false").
      send(backend).
      map(_.body.fold(err => throw Exception(err), json => {
        val rsp = parseMedia(json)
        Result.resolved(rsp.media, json, rsp.canonicalUrl(id) getOrElse "")
      }))

  // NjQyMTgzOTU5MjA4MTA3Omh0dHBzXGEvL3d3dy5mYWNlYm9vay5jb20vcmVlbC83MjY3MzM0ODkzMzgxNDk4Ojo6Og== // ExternalUrl
  // 717077590580168 // Photo
  case class UrlInfo (id :String, `__typename` :String, `strong_id__` :String) derives ReadWriter
  case class UrlData (urlResolver :UrlInfo) derives ReadWriter
  case class ResolvedUrl (data :UrlData) derives ReadWriter

  def parseResolvedUrl (json :String) = try read[ResolvedUrl](json) catch {
    case e :Throwable => throw new Exception(s"Failed to parse: $json", e)
  }

  def resolveUrl (url :String) :Future[(UrlInfo, String)] =
    basicRequest.
      header("x-api-key", token).
      get(uri"https://api.vetric.io/facebook/v1/url-resolver?url=${url}").
      send(backend).
      map(_.body.fold(
        err => throw Exception(err),
        json => (parseResolvedUrl(json).data.urlResolver, json)))

  def extractMediaId (url :String) :Option[String] =
    mediaUrlRegexes.flatMap((r, g) => r.findFirstMatchIn(url) map { _.group(g) }).headOption

  def tryResolveMedia (url :String) :Option[Future[Result]] = extractMediaId(url) match {
    case Some(id) => Some(resolveMedia(id))
    case None =>
      if (urlRegex.matches(url)) Some(resolveUrl(url).flatMap(ij => ij._1.__typename match {
        case "Video" => resolveMedia(ij._1.id)
        case "Photo" => resolveMedia(ij._1.id)
        case "Story" => resolvePost(ij._1.id)
        case tp =>
          log.info(s"Unhandled Facebook node type ${ij._1}")
          Future.successful(Result.failure(s"Unsupported Facebook page type: $tp.", ij._2))
      }))
      else None
  }
}
