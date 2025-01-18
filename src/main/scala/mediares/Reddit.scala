package mediares

import scala.concurrent.Future
import scala.collection.mutable.ArrayBuilder
import java.util.logging.Level

import upickle.default._
import sttp.client4._

object Reddit {

  import Pool.ctx
  private val backend = DefaultFutureBackend()
  private val log = java.util.logging.Logger.getLogger("reddit")

  private val urlRegexes = Seq(
    ("^(https://)?(www.)?reddit.com/+r/.*/comments/(\\w+)/.*".r, 3),
    ("^(https://)?(www.)?reddit.com/+(\\w+)$".r, 3),
    ("^(https://)?redd.it/+(\\w+)$".r, 2))

  private def base64 (text :String) = {
    import java.util.Base64
    import java.nio.charset.StandardCharsets
    Base64.getEncoder.encodeToString(text.getBytes(StandardCharsets.UTF_8))
  }

  //
  // OAuth authentication

  private val OAUTH_ENDPOINT = uri"https://www.reddit.com/api/v1/access_token"
  private val USER_AGENT = "True Media/1.0"

  private case class AuthResult (
    access_token :String,
    token_type :String,
    expires_in :Int,
    scope :String,
  ) derives ReadWriter

  case class OAuthToken (accessToken :String)

  /** Obtains an OAuth token using the supplied app `id` and `secret`. */
  def obtainToken (appId :String, appSecret :String) :Future[OAuthToken] = {
    import sttp.client4.upicklejson.default._
    log.info(s"Obtaining Reddit auth token [appId=${appId}].")
    basicRequest.
      header("User-Agent", USER_AGENT).
      header("Authorization", "Basic " + base64(s"${appId}:${appSecret}")).
      header("Content-Type", "application/x-www-form-urlencoded").
      post(OAUTH_ENDPOINT).
      body("grant_type=client_credentials").
      response(asJson[AuthResult]).
      send(backend).
      // TODO: what do we get back when auth fails?
      map(_.body match {
        case Left(error) => throw error
        case Right(res) => OAuthToken(res.access_token)
      })
  }

  private lazy val oauthToken = {
    val Seq(appId, appSecret) = Env.getSecrets("REDDIT_APP_ID", "REDDIT_APP_SECRET")
    obtainToken(appId, appSecret)
  }

  //
  // DASH media extraction

  /** Extracts the desired video and audio track from the supplied DASH manifest.
    * @return the `Media` object for the video, with the audio track embedded inside, or `None`. */
  def extractMedia (baseUrl :String, duration :Int, dashIn :java.io.InputStream) :Option[Media] = {
    import scala.collection.JavaConverters._
    val parser = new io.lindstrom.mpd.MPDParser()
    val mpd = parser.parse(dashIn)

    val reprs = for (period <- mpd.getPeriods.asScala ;
                     set <- period.getAdaptationSets.asScala ;
                     repr <- set.getRepresentations.asScala) yield repr

    def safeStarts (s :String, p :String) = s != null && s.startsWith(p)
    val videos = reprs.filter(rr => safeStarts(rr.getMimeType, "video/") || safeStarts(rr.getId, "video_"))
    val audios = reprs.filter(rr => safeStarts(rr.getMimeType, "audio/") || safeStarts(rr.getId, "audio_"))

    // if we don't get a mime-type, we have to guess one from the URL, but Reddit uses .mp4 for
    // their audio tracks, and our mime-type guesser turns that into video/mp4, but really we
    // want audio/mp4 in that case, so hackity hack hack
    def coerceMimeType (mp :String, mt :String) =
      if (mp == "audio/" && mt.startsWith("video/")) mp + mt.substring(6) else mt

    def toMedia (repr :io.lindstrom.mpd.data.Representation, mimePrefix :String) = {
      val url = baseUrl + repr.getBaseURLs.get(0).getValue
      val mimeType = coerceMimeType(
        mimePrefix, Option(repr.getMimeType).getOrElse(Util.guessMimeType(url, s"${mimePrefix}unknown")))
      Media(Util.computeId(url, mimeType), mimeType, url,
            repr.getWidth.toInt, repr.getHeight.toInt, repr.getBandwidth.toInt, duration)
    }

    videos.maxByOption(rr => rr.getWidth * rr.getHeight) match {
      case Some(video) =>
        val audio = audios.maxByOption(_.getBandwidth).map(rr => toMedia(rr, "audio/")) getOrElse null
        Some(toMedia(video, "video/").copy(audio=audio))
      case None => None
    }
  }

  //
  // API data model and requests

  case class Video (
    bitrate_kbps :Int,
    height :Int,
    width :Int,
    duration :Int,
    is_gif :Boolean,
    fallback_url :String,
    dash_url :String,
    hls_url :String,
    has_audio :Boolean = false,
  ) derives ReadWriter {
    def toMedia (resolveDash :Boolean) = if (resolveDash) {
      try {
        val url = new java.net.URL(dash_url)
        val conn = url.openConnection
        val baseUrl = dash_url.substring(0, dash_url.indexOf("DASH"))
        extractMedia(baseUrl, duration, conn.getInputStream) getOrElse fallback
      } catch {
        case e :Throwable =>
          log.log(Level.WARNING, "Failed to get video info from DASH manifest", e)
          fallback
      }
    } else fallback
    private def mimeType = Util.guessMimeType(fallback_url, "video/unknown")
    private def fallback = Media.video(
      mimeType, fallback_url, width, height, bitrate_kbps, duration)
  }

  case class RMedia (
    reddit_video :Video = null,
  ) derives ReadWriter {
    // TODO: are there other types of media?
    def video (resolveDash :Boolean) :Option[Media] =
      Option(reddit_video).map(_.toMedia(resolveDash))
  }

  trait Info {
    def canonicalUrl :Option[String]
  }

  case class Listing (children :Seq[ujson.Value]) extends Info derives ReadWriter {
    lazy val parts :Seq[Info] = children.collect(_ match {
      case obj :ujson.Obj => decodeInfo(obj)
    })
    def canonicalUrl = parts.flatMap(_.canonicalUrl).headOption
  }

  case class MediaInfo (x :Int, y :Int, u :String) derives ReadWriter

  case class MediaMetadata (
    id :String,
    status :String, // valid or ?
    e :String, // Image, or ?
    m :String, // mime-type
    p :Seq[MediaInfo], // preview images
    s :MediaInfo, // source image
  ) derives ReadWriter {
    def suffix = Util.fileSuffix(s.u, m)
    def mediaId = Util.hashUrl(s"reddit:$id:${m}:${s.x}:${s.y}") + suffix
    def media :Media = Media(mediaId, m, s"https://i.redd.it/$id$suffix", s.x, s.y)
  }

  case class T3 (
    url :String,
    media_metadata :Map[String, MediaMetadata] = null,
    post_hint :String = null,
    media :RMedia = null,
    permalink :String = null,
  ) extends Info derives ReadWriter {
    def canonicalUrl = Option(permalink).map(path => s"https://www.reddit.com$path")
  }

  case class Unknown (kind :String, data :ujson.Value) extends Info {
    def canonicalUrl = None
  }

  def decodeInfo (obj :ujson.Obj) :Info = obj("kind").str match {
    case "Listing" => read[Listing](obj("data"))
    case "t3" => read[T3](obj("data"))
    case kind => Unknown(kind, obj("data"))
  }

  def parseInfo (json :String) :Info =
    try ujson.read(json) match {
      case obj :ujson.Obj => decodeInfo(obj)
      case _ => throw Exception(s"Unexpected JSON response: $json")
    }
    catch {
      case e :Exception => throw Exception(s"Parse failure: ${e.getMessage}\n${json}")
    }

  def collectMedia (
    info :Info, urls :ArrayBuilder[Media], resolveDash :Boolean
  ) :Unit = info match {
    case listing :Listing =>
      listing.parts.foreach(part => collectMedia(part, urls, resolveDash))
    case t3 :T3 =>
      t3.post_hint match {
        case "hosted:video" if (t3.media != null) =>
          // if this is a video post, it should have a media section
          t3.media.video(resolveDash).foreach { urls += _ }
        case "image" =>
          // if it's an image post, the URL should point to the image
          urls += Media.image(Util.guessMimeType(t3.url, "image/unknown"), t3.url)
        case "link" =>
          throw Util.ReportException(
            "This post does not contain media. It contains a link to another website. " +
              "Please submit a direct link to the media.")
        case hint =>
          if (t3.media_metadata != null) urls ++= t3.media_metadata.values.map(_.media)
          else if (t3.media != null) t3.media.video(resolveDash).foreach { urls += _ }
          else log.info(s"Unhandled post hint: ${hint}")
      }
    case _ => // unknown
  }

  def decodeMedia (info :Info, resolveDash :Boolean) :Seq[Media] = {
    val urls = ArrayBuilder.ofRef[Media]()
    collectMedia(info, urls, resolveDash)
    urls.result
  }

  //
  // Reddit API comms

  def resolveMedia (postId :String, token :OAuthToken) :Future[Result] = {
    log.info(s"Resolving Reddit post [id=${postId}]")
    basicRequest.
      header("Authorization", s"Bearer ${token.accessToken}").
      get(uri"https://oauth.reddit.com/api/info/?id=t3_${postId}").
      send(backend).
      map(_.body.fold(err => throw Exception(err), json => {
        val info = parseInfo(json)
        Result.resolved(decodeMedia(info, true), json, info.canonicalUrl getOrElse "")
      }))
  }

  def resolveMedia (postId :String) :Future[Result] =
    oauthToken.flatMap(resolveMedia(postId, _))

  def decodeMediaUrl (url :String) :Option[String] =
    if (!url.startsWith("https://www.reddit.com/media")) None
    else try {
      (new java.net.URL(url).getQuery.split("&").collect {
        case s"url=$value" => java.net.URLDecoder.decode(value)
      }).headOption
    } catch {
      case e :Throwable =>
        log.warning(s"Unable to parse Reddit media URL: $url")
        None
    }

  /** Fetches the metadata for a Reddit post & extracts the media URL(s) therein. Uses a cached,
    * lazily resolved oauth token derived from environment variables (see [oauthToken]).
    * @return `None` if `url` is not a Reddit page, otherwise a list of media URLs found in the
    * post or a failed future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    postIdFromUrl(url).map(resolveMedia) orElse decodeMediaUrl(url).map(Generic.resolveGeneric)

  /** Extracts the post id from a Reddit page URL.
    * @return the post id or None if `url` is not a Reddit page URL.
    */
  def postIdFromUrl (url :String) :Option[String] =
    urlRegexes.flatMap((r, g) => r.findFirstMatchIn(url) map { _.group(g) }).headOption
}
