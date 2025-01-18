package mediares

import scala.concurrent.Future
import scala.collection.mutable.ArrayBuilder

import upickle.default._
import sttp.client4._

object GoogleDrive {

  private val log = java.util.logging.Logger.getLogger("gdrive")
  import Pool.ctx
  private val backend = DefaultFutureBackend()

  private val urlRegexes = Seq(
    // https://drive.google.com/file/d/1bQ71-jwlkdxVtSghnXLUURDf-V3Q0cN_/view?usp=sharing
    // https://drive.google.com/file/d/1Rv7Q3k_ESRL4m8gaYZY6PnnOewOvxVtK/view?usp=drive_link
    ("^(https://)drive.google.com/file/d/([^/]+)/.*".r, 2))

  private final val apiUrl = "https://www.googleapis.com/drive/v3/files"
  private lazy val apiKey = Env.getSecrets("GOOGLE_API_KEY")(0)

  //
  // API data model and requests

  case class File (kind :String, id :String, name :String, mimeType :String) derives ReadWriter {
    def mediaId :String = Util.computeId(driveUrl, mimeType)
    def driveUrl :String = s"https://drive.google.com/file/d/${id}"
    def downloadUrl (apiKey :String) :String =
      s"${apiUrl}/${id}?alt=media&acknowledgeAbuse=true&key=${apiKey}"
  }

  def decodeFile (id :String, json :String) :Result = try {
    val file = read[File](json)
    val media = Media(file.mediaId, file.mimeType, file.driveUrl,
                      secretUrl=Some(file.downloadUrl(apiKey)))
    val canonicalUrl = s"https://drive.google.com/file/d/${id}/view"
    Result.resolved(Seq(media), json, canonicalUrl)
  } catch {
    case e :Exception => throw Exception(s"Parse failure: ${e.getMessage}\n${json}")
  }

  case class ErrorDetail (
    message :String = null, domain :String = null, reason :String = null,
    location :String = null, locationType :String = null
  ) derives ReadWriter
  case class Error (code :Int, message :String, errors :Seq[ErrorDetail]) derives ReadWriter
  case class ErrorResponse (error :Error) derives ReadWriter

  val googleExplain =
    "Please check that the share link you provided is visible to anyone with the link and " +
    "is in a folder that is also visible to anyone with a link."

  def decodeError (json :String) = {
    val error = read[ErrorResponse](json)
    log.warning(s"GoogleDrive resolve failure: $json")
    Result.failure(s"${error.error.message} $googleExplain")
  }

  def resolveMedia (id :String) :Future[Result] = {
    log.fine(s"Resolving Google Drive file [id=${id}]")
    basicRequest.
      get(uri"${apiUrl}/${id}?key=${apiKey}").
      send(backend).
      map(_.body.fold(decodeError, json => decodeFile(id, json)))
  }

  /** Fetches the metadata for a Google Drive share link & extracts the media URL therein.
    * @return `None` if `url` is not a Google Drive share link, otherwise a list of the one media
    * contained by the drive or a failed future with error explanation.
    */
  def tryResolveMedia (url :String) :Option[Future[Result]] =
    fileIdFromUrl(url).map(resolveMedia)

  /** Extracts the file id from a Google Drive share link.
    * @return the file id or None if `url` is not a Google Drive share link.
    */
  def fileIdFromUrl (url :String) :Option[String] =
    urlRegexes.flatMap((r, g) => r.findFirstMatchIn(url) map { _.group(g) }).headOption
}
