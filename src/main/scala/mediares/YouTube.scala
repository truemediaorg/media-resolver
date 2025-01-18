package mediares

import scala.concurrent.Future

import upickle.default._
import sttp.client4._

object YouTube {

  import Pool.ctx
  private val backend = DefaultFutureBackend()

  // I apologize to future developers for the write-only nature of these regexps
  private val urlRegexes = Seq(
    ("^(https?://)?(www.|m.)?youtube(-nocookie)?.com/watch\\?(.*&)?v=(-?\\w+)".r, 5),
    ("^(https?://)?(www.|m.)?youtube(-nocookie)?.com/(v|e|embed|shorts)/(-?\\w+)".r, 5),
    ("^(https?://)?youtu.be/(-?\\w+)".r, 2))

  case class PlayabilityStatus (
    val status :String,
    val contextParams :String = null,
    val reason :String = null,
    val playableInEmbed :Boolean = false,
  ) derives ReadWriter

  case class StreamingFormat (
    val itag :Int,
    val mimeType :String,
    val bitrate :Int,
    val lastModified :String,
    val quality :String,
    val projectionType :String,
    val approxDurationMs :String = null,
    // some videos include a URL, some include a signature cipher
    val url :String = null,
    // TODO: some media is returned with a signature cipher that must be decoded to get the proper
    // URL; we should do the extra shenanigans required for that at some point
    val signatureCipher :String = null,
    // these fields don't appear in audio-only streaming formats
    val width :Int = 0,
    val height :Int = 0,
    val fps :Int = 0,
    val qualityLabel :String = null,
    // these fields only appear in audio-only streaming formats and non-adaptive video formats
    val audioQuality :String = null,
    val audioSampleRate :String = null,
    val audioChannels :Int = 0,
  ) derives ReadWriter {

    /** True if this is a video stream with audio. */
    def hasAudio = mimeType.startsWith("video/") && mimeType.contains("mp4a")

    /** Returns a score of how good this stream will be as our video track. */
    def videoScore :Int = if (url == null) 0 else {
      val qualityScore = if (qualityLabel == "720p") 30
                         else if (qualityLabel == "1080p") 20
                         else 0
      val codecScore = if (mimeType.startsWith("avc1")) 5
                       else if (mimeType.startsWith("av01.0")) 3
                       else 0
      qualityScore + codecScore
    }

    /** Whether this is a purely audio stream. */
    def isAudio = mimeType.startsWith("audio/")

    /** Returns a score of how good this stream will be as our audio track. */
    def audioScore :Int = if (url == null) 0 else {
      val mimeTypeScore = if (mimeType == "audio/mp4") 10 else 0
      val qualityScore = if (audioQuality == "AUDIO_QUALITY_MEDIUM") 5 else 0
      mimeTypeScore + qualityScore
    }

    def toMedia (videoId :String) = Media(
      Util.computeId(s"youtube:$videoId:$mimeType", mimeType), mimeType, url,
      width, height, bitrate, duration)

    def debug :String = s"$mimeType ${bitrate/1024}kbps ${width}x${height} $qualityLabel $audioQuality"

    private def duration =
      try (approxDurationMs.toLong/1000).toInt
      catch { case e :Exception => 0 }
  }

  case class StreamingData (
    val expiresInSeconds :Int,
    val formats :Seq[StreamingFormat],
    val adaptiveFormats :Seq[StreamingFormat],
  ) derives ReadWriter {
    private def unciphered = adaptiveFormats.filter(_.signatureCipher == null)

    /** Checks whether all available media is ciphered. */
    def allCiphered = unciphered.size == 0

    /** Picks the video (with aux audio media) to be returned for this result. */
    def pickMedia (videoId :String, thumbnailUrl :Option[String]) :Media =
      unciphered.filter(_.signatureCipher == null).maxBy(_.videoScore).toMedia(videoId).copy(
        audio = unciphered.maxBy(_.audioScore).toMedia(videoId),
        videoThumbnailUrl = thumbnailUrl)
  }

  case class Thumbnail (url :String, width :Int, height :Int) derives ReadWriter
  case class VideoThumbnail (thumbnails :Seq[Thumbnail]) derives ReadWriter

  case class VideoDetails (
    videoId :String,
    channelId :String,
    author :String,
    title :String,
    thumbnail :VideoThumbnail = null,
    lengthSeconds :Int = 0,
    shortDescription :String = null,
    viewCount :Int = 0,
    keywords :Seq[String] = Seq(),
  ) derives ReadWriter {
    def thumbnailUrl :Option[String] =
      Option(thumbnail).flatMap(_.thumbnails.maxByOption(_.width)).map(_.url)
  }

  case class Response (
    val playabilityStatus :PlayabilityStatus,
    val streamingData :StreamingData = null,
    val videoDetails :VideoDetails = null,
  ) derives ReadWriter {
    def pickMedia (videoId :String) = streamingData.pickMedia(
      videoId, Option(videoDetails).flatMap(_.thumbnailUrl))
  }

  def parseResponse (json :String) :Response =
    try read[Response](ujson.read(json))
    catch {
      case e :Exception => throw Exception(s"Parse failure: ${e.getMessage}\n${json}")
    }

  private def explainFailure (reason :String) = reason match {
    case "This video is unavailable" =>
      "This video is marked as viewable only on YouTube.com. " +
        "YouTube prevents us from downloading and analyzing it."
    case reason => s"YouTube reports: ${reason}"
  }

  def extractMedia (videoId :String, res :Response) :Seq[Media] =
    if (res.playabilityStatus.status != "OK") throw Util.ReportException(
      explainFailure(res.playabilityStatus.reason), res.playabilityStatus.reason)
    else if (res.streamingData.allCiphered) throw Util.ReportException(
      "Unable to download that video from YouTube.", "All streams ciphered.")
    else Seq(res.pickMedia(videoId))

  def decodeMedia (videoId :String, json :String) :Seq[Media] =
    extractMedia(videoId, parseResponse(json))

  /** Resolves the streaming media metadata for the specified YouTube `videoId`.
    * @return metadata for the media, or a failed future with an error message.
    */
  def resolveMedia (videoId :String) :Future[Result] = {
    import sttp.client4.upicklejson.default._

    // the web client and the extra headers below are how Pendulum gets YouTube info; we're now
    // using this old embedded player client because apparently that avoids some restrictions and
    // that would otherwise prevent us from getting the video media
    val clientName = "TVHTML5_SIMPLY_EMBEDDED_PLAYER"
    val clientVersion = "2.0"
    val clientKey = "AIzaSyA8eiZmM1FaDVjRy-df2KTyQ_vz_yYM39w"
    // val clientName = "WEB"
    // val clientVersion = "2.20221207.05.00"
    def payload (videoId :String) = ujson.Obj(
      "context" -> ujson.Obj(
        "client" -> ujson.Obj("clientName" -> clientName, "clientVersion" -> clientVersion)),
      "videoId" -> videoId,
      "thirdParty" -> ujson.Obj("embedUrl" -> "https://www.youtube.com")
    )

    basicRequest.
      // header("x-youtube-client-name", "1").
      // header("x-youtube-client-version", clientVersion).
      // header("Cookie", "YSC=2b2jlODjvxg; VISITOR_INFO1_LIVE=w58NjbAnvSo").
      post(uri"https://www.youtube.com/youtubei/v1/player?key=${clientKey}").
      body(payload(videoId)).
      send(backend).
      map(_.body.fold(
        err => throw Exception(err),
        json => Result.resolved(decodeMedia(videoId, json), json)))
  }

  /** Fetches the metadata for a YouTube page & extracts the media URL(s) therein.
    * @return `None` if `url` is not a YouTube page, otherwise a list of media URLs for the video
    * or a failed future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    videoIdFromUrl(url).map(resolveMedia)

  /** Extracts the video id from a YouTube page URL.
    * @return the video id or None if `url` is not a YouTube page URL.
    */
  def videoIdFromUrl (url :String) :Option[String] =
    urlRegexes.flatMap((r, g) => r.findFirstMatchIn(url) map { _.group(g) }).headOption
}
