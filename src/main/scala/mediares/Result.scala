package mediares

import upickle.default._

// we don't use a sealed trait here because upickle interprets that to mean that we want its
// built-in "serialized tagged object" stuff, which we don't
trait Result {
  def toJson :String
}

object Result {
  /** Returned when we successfuly resolve media URLs from a website URL.
    * @param media The media that were resolved from the website URL.
    * @param source The source JSON from which we resolved the media, if available. This is
    * returned so that we can save it and potentially extract other metadata in the future.
    * @param canonicalUrl The canonical URL to this post, or `""`. This may be different than the
    * URL that was resolved, due to link shorteners or query parameters or that sort of thing.
    */
  case class Resolved (
    result :String, media :Seq[Media], source :String, canonicalUrl :String,
  ) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def resolved (media :Seq[Media], source :String = "", canonicalUrl :String = "") =
    Resolved("resolved", media, source, canonicalUrl)

  case class FileUpload (
    result :String, id: String, mimeType :String, putUrl :String
  ) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def fileUpload (id: String, mimeType :String, putUrl :String) =
    FileUpload("upload", id, mimeType, putUrl)

  /** This value is returned as the error when the cache status is requested for the audio track
    * extracted from a video, which turned out not to have an audio track. This precise string must
    * be returned so that the detect webapp can do the appropriate post hoc cleanup. */
  final val NoAudioTrackError = "Video has no audio track"

  /** @param url filled in when a caching request is complete, and `null` otherwise. For such a
    * request, `url` is the signed URL via which the cached media can be viewed.
    * @param error filled in if this resource failed to be cached. In this case transferred and
    * total wil be `0`. */
  case class TransferStatus (
    transferred :Long, total :Long, url :String = null, error :String = null
  ) derives ReadWriter

  case class Progress (
    result :String, statuses :Map[String, TransferStatus]
  ) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def progress (statuses :Map[String, TransferStatus]) = Progress("progress", statuses)

  case class Duration (result :String, duration :Int) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def duration (duration :Int) = Duration("duration", duration)

  case class Trimmed (result :String, video :Media) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def trimmed (video :Media) = Trimmed("trimmed", video)

  case class Failure (result :String, reason :String, details :String) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  def failure (reason :String, details :String = null) = Failure("failure", reason, details)
}
