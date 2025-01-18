package mediares

import java.security.MessageDigest
import java.util.Base64
import sttp.model.Uri

object Util {

  /** An exception for when we want to pass a failure reason back to the API caller. */
  class ReportException (reason :String, val details :String = null) extends Exception(reason)

  /** Tries to parse `url` as a URI, returns `None` if it's invalid. */
  def tryParseUri (url :String) :Option[Uri] = try Uri.parse(url) match {
    case Left(uri) => None
    case Right(uri) => Some(uri)
  } catch {
    case t :Throwable => None
  }

  /** Picks the "best" media from a list of candidates for the same video. Some sites return
    * multiple URLs for the same video, usually with different bitrates. We currently use the
    * highest bitrate option, to maximize the chance of detecting deepfake features.
    * @return a length 1 sequence containing the picked media, or a length 0 sequence if the
    * supplied `media` is also length 0.
    */
  def pickBestMedia (media :Iterable[Media]) :Seq[Media] = if (media.isEmpty) Seq() else {
    // some of our detection partners don't support video/webm
    val notWebm = media.filterNot(_.mimeType.startsWith("video/webm"))
    // most of our detection partners don't support videos > 100MB
    val notTooBig = notWebm.filter(_.estimateSize < 100*1024*1024)
    val maxBit = notTooBig.maxBy(_.bitrate)
    if (maxBit.bitrate > 0) Seq(maxBit)
    else {
      val maxPixels = notTooBig.maxBy(_.pixels)
      if (maxPixels.pixels > 0) Seq(maxPixels)
      // if all else fails, just use the first one
      else Seq(media.head)
    }
  }

  private val base64url = Base64.getUrlEncoder.withoutPadding

  def hashUrl (url :String) :String = {
    val md = MessageDigest.getInstance("SHA-1")
    base64url.encodeToString(md.digest(url.getBytes))
  }

  /** Computes an id for `url`. This will be used to identify the media in S3. */
  def computeId (url :String, mimeType :String) :String = hashUrl(url) + fileSuffix(url, mimeType)

  /** Computes a new media id based on an old media id and new "source" information. The source
    * information will be hashed and combined with the file suffix of the old id to create a new
    * id. */
  def deriveId (oldId :String, newSource :String) :String =
    hashUrl(newSource) + (fileSuffixFromFilename(oldId) getOrElse ".dat")

  /** Infers a file suffix for media with `url` and `mimeType`. If the URL has a file suffix, that
    * is used directly, otherwise if one can be deduced from the mime type, that is used, otherwise
    * `.dat` is returned. */
  def fileSuffix (url :String, mimeType :String) =
    fileSuffixFromUrl(url) orElse fileSuffixFromMimeType(mimeType) getOrElse ".dat"

  /** Returns the file suffix of the file component of the supplied URL, or `""` if the file
    * component has no suffix. For example: `https://pbs.twimg.com/media/foo.jpg?baz=bar` will
    * yield a suffix of `.jpg`.
    */
  def fileSuffixFromUrl (url :String) :Option[String] = {
    try {
      val path = new java.net.URI(url).getPath
      fileSuffixFromFilename(path)
    } catch {
      // if we fail to parse the URI for whatever reason, return the empty string
      case e :Exception => None
    }
  }

  def fileSuffixFromFilename (filename :String) :Option[String] = {
    val didx = filename.lastIndexOf(".")
    if (didx == -1) None
    else Some(filename.substring(didx).toLowerCase)
  }

  /** Returns the standard file suffix for `mimeType` or `""` if none is known. */
  def fileSuffixFromMimeType (mimeType :String) :Option[String] =
    mimeTypeToExt.get(mimeType.split(";")(0))

  /** Guesses the mime-type of a media URL based on a suffix in its path. */
  def guessMimeType (url :String, fallback :String) :String = fileSuffixFromUrl(url) match {
    case Some(suff) => guessMimeTypeFromSuffix(suff, fallback)
    case None =>
      // TikTok media URLs seem to have the mime-type in a query parameter, so look for that
      try {
        val parts = new java.net.URI(url).getQuery.split("&")
        parts.find(pp => pp.startsWith("mime_type=")) match {
          case Some(kv) => kv.split("=")(1).replace("_", "/")
          case None => fallback
        }
      } catch {
        case e :Exception => fallback
      }
  }

  def guessMimeTypeFromSuffix (suffix :String, fallback :String) :String = extToMimeType.get(suffix) getOrElse fallback

  private val extToMimeType = Map(
    // audio types
    ".3g2"    -> "audio/3gpp2",
    ".3gp"    -> "audio/3gpp",
    ".3gpp"   -> "audio/3gpp",
    ".3gpp2"  -> "audio/3gpp2",
    ".aac"    -> "audio/aac",
    ".adts"   -> "audio/aac",
    ".aif"    -> "audio/x-aiff",
    ".aifc"   -> "audio/x-aiff",
    ".aiff"   -> "audio/x-aiff",
    ".ass"    -> "audio/aac",
    ".au"     -> "audio/basic",
    ".loas"   -> "audio/aac",
    ".m4a"    -> "audio/mp4",
    ".mp2"    -> "audio/mpeg",
    ".mp3"    -> "audio/mpeg",
    ".opus"   -> "audio/opus",
    ".ra"     -> "audio/x-pn-realaudio",
    ".snd"    -> "audio/basic",
    ".wav"    -> "audio/x-wav",
    // image types
    ".avif"   -> "image/avif",
    ".bmp"    -> "image/bmp",
    ".gif"    -> "image/gif",
    ".heic"   -> "image/heic",
    ".heif"   -> "image/heif",
    ".ico"    -> "image/vnd.microsoft.icon",
    ".ief"    -> "image/ief",
    ".jpe"    -> "image/jpeg",
    ".jpeg"   -> "image/jpeg",
    ".jpg"    -> "image/jpeg",
    ".pbm"    -> "image/x-portable-bitmap",
    ".pgm"    -> "image/x-portable-graymap",
    ".png"    -> "image/png",
    ".pnm"    -> "image/x-portable-anymap",
    ".ppm"    -> "image/x-portable-pixmap",
    ".ras"    -> "image/x-cmu-raster",
    ".rgb"    -> "image/x-rgb",
    ".svg"    -> "image/svg+xml",
    ".tif"    -> "image/tiff",
    ".tiff"   -> "image/tiff",
    ".xbm"    -> "image/x-xbitmap",
    ".xpm"    -> "image/x-xpixmap",
    ".xwd"    -> "image/x-xwindowdump",
    ".webp"   -> "image/webp",
    // video types
    ".avi"    -> "video/x-msvideo",
    ".m1v"    -> "video/mpeg",
    ".mov"    -> "video/quicktime",
    ".movie"  -> "video/x-sgi-movie",
    ".mp4"    -> "video/mp4",
    ".mpa"    -> "video/mpeg",
    ".mpe"    -> "video/mpeg",
    ".mpeg"   -> "video/mpeg",
    ".mpg"    -> "video/mpeg",
    ".qt"     -> "video/quicktime",
    ".webm"   -> "video/webm",
  )

  private val mimeTypeToExt = Map(
    "audio/aac"       -> ".aac",
    "audio/basic"     -> ".snd",
    "audio/mp4"       -> ".m4a",
    "audio/mpeg"      -> ".mp3",
    "audio/webm"      -> ".opus",
    "audio/x-aiff"    -> ".aiff",
    "audio/x-wav"     -> ".wav",
    "audio/wav"       -> ".wav",
    "image/bmp"       -> ".bmp",
    "image/gif"       -> ".gif",
    "image/heic"      -> ".heic",
    "image/heif"      -> ".heif",
    "image/jpeg"      -> ".jpg",
    "image/png"       -> ".png",
    "image/webp"      -> ".webp",
    "video/mp4"       -> ".mp4",
    "video/mpeg"      -> ".mpeg",
    "video/quicktime" -> ".mov",
    "video/webm"      -> ".webm",
    "video/x-msvideo" -> ".avi",
  )
}
