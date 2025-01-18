package mediares

import scala.concurrent.Future

import sttp.client4._

object Passthrough {

  /** If `url` appears to be a raw media file (.mp4, .jpg, etc.) then pass it straight through.
    * @return `None` if `url` does not appear to be a direct media URL, or a `Media` wrapper around
    * it, if it does.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    Util.guessMimeType(url, "unknown") match {
      case mimeType if mimeType.startsWith("video/") ||
                       mimeType.startsWith("image/") ||
                       mimeType.startsWith("audio/") =>
        val media = Media(Util.computeId(url, mimeType), mimeType, url)
        Some(Future.successful(Result.resolved(Seq(media))))
      case _ => None
    }
}
