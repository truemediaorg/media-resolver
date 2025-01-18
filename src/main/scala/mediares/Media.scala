package mediares

import upickle.default._

case class Media (
  /** A unique identifier assigned to this media for tracking. */
  id :String,
  /** Video/format, image/format, etc. */
  mimeType :String,
  /** The URL of the media at the source website. */
  url :String,
  /** Width of the media, or 0 if unknown. */
  width :Int = 0,
  /** Height of the media, or 0 if unknown. */
  height :Int = 0,
  /** Bitrate of the media (in bits per second), or 0 if unknown or N/A (i.e. for images). */
  bitrate :Int = 0,
  /** Duration of the media (in seconds), or 0 if unknown or N/A (i.e. for images). */
  duration :Int = 0,
  /** An optional audio media resource which contains the audio track for a video. */
  audio :Media = null, // can't use Option because ujson serializes that as an array
  /** An optional URL via which a video thumbnail image can be downloaded for this media. */
  videoThumbnailUrl :Option[String] = None,
  /** The URL via which we should download and cache this media, if needed. If not specified, we'll
    * just download it via `url`. */
  secretUrl :Option[String] = None,
) derives ReadWriter {

  /** The URL via which to download and cache this media. */
  def downloadUrl = secretUrl getOrElse url

  /** The number of pixels in each frame, `width x height`. */
  def pixels = width * height

  /** A very rough estimate of the size of this media. Mainly useful to compare multiple
    * candidates from the same service.
    *
    * TODO: YouTube returns weird bitrates, so we might not even want to use this. They will
    * return video-only streams with high bit rates and video+audio streams with lower bit rates,
    * but the higher bitrate stream is actually smaller than the lower bit rate stream. */
  def estimateSize = if (duration == 0) pixels * 4 else duration * bitrate / 8

  /** The URL via which a thumbnail can be downloaded for this media. In the case of an image, this
    * is the media URL itself, in the case of a video this will contain a URL if the source site
    * provides URLs and will be `None` otherwise (in which case we have to extract a thumbnail
    * ourselves from the video data). */
  def thumbnailUrl :Option[String] = videoThumbnailUrl match {
    case Some(url) => Some(url)
    case None => if (mimeType.startsWith("image/")) Some(downloadUrl) else None
  }

  /** Returns a version of this media with the private download URLs removed. */
  def clean :Media = copy(
    secretUrl = None,
    videoThumbnailUrl = None,
    audio = Option(audio).map(_.clean) getOrElse null
  )

  /** Whether this media is cached by its resolver or should be cached after resolution. Most
    * resolvers just obtain metdata for media which is then cached (downloaded from the source site
    * and uploaded to S3) after resolution, but the yt-dlp resolver downloads the actual media as a
    * part of resolution. Moreover the download will be in progress at the time resolution
    * completes, so we can't cache the media on its behalf. We have to just let it do the whole
    * job. */
  def isCachedByResolver = downloadUrl == "cached-by-resolver"

  /** Returns a clone of this media, configured to note that it will be cached by the resolver. */
  def makeCachedByResolver = copy(secretUrl = Some("cached-by-resolver"))
}

object Media {

  /** Creates a media record for an image with optionally known width and height. */
  def image (mimeType :String, url :String, width :Int = 0, height :Int = 0) = Media(
    Util.computeId(url, mimeType), mimeType, url, width, height)

  /** Creates a media record for a video with optionally known metadata. */
  def video (mimeType :String, url :String, width :Int = 0, height :Int = 0,
             bitrate :Int = 0, duration :Int = 0, thumbnailUrl :Option[String] = None) =
    Media(Util.computeId(url, mimeType), mimeType, url, width, height, bitrate, duration,
          videoThumbnailUrl = thumbnailUrl)

  /** Creates a media record for audio with optionally known metadata. */
  def audio (mimeType :String, url :String, bitrate :Int = 0, duration :Int = 0) =
    Media(Util.computeId(url, mimeType), mimeType, url, 0, 0, bitrate, duration)

  /** Computes thumbnail id for `mediaId` (just `mediaId` minus the file suffix). */
  def thumbnailId (mediaId :String) = resuffix(mediaId, "")

  /** Computes a media id for an audio track extracted from a video with `mediaId`. */
  def extractedAudioId (mediaId :String) = resuffix(mediaId, ".mp3")

  /** Creates a `Media` record for an audio track extracted from `video`. */
  def extractedAudio (video :Media) = Media(extractedAudioId(video.id), "audio/mp3", s"extracted:${video.id}")

  /** Replaces the file suffix of `id` with `suff`.
    * @param suff the new suffix, which _must_ contain the preceding `.`. */
  private def resuffix (id :String, suff :String) = id.lastIndexOf(".") match {
    case -1 => s"$id$suff"
    case ii => s"${id.substring(0, ii)}$suff"
  }
}
