package mediares

import scala.concurrent.{Await, Future, blocking}
import java.nio.file.{Files, Path, StandardOpenOption}

/** Utilities for working with media files downloaded to the local file system. */
object Local {

  private val log = java.util.logging.Logger.getLogger("local")
  import Pool.ctx

  /** Represents a file downloaded to the local filesystem.
    * @param url the URL from whence the file was downloaded. We store this as metadata when
    * uploading the file to S3 just to help track provenance.
    * @param contentLength the length of the file in bytes.
    * @param contentType the mime-type of the file.
    * @param path the path on the local file system. */
  case class File (url :String, contentLength :Long, contentType :String, path :Path)

  def checkHasAudioTrack (video :Path) :Future[Boolean] =
    for (lines <- runCommand("ffprobe", "-show_streams", "-select_streams", "a", "-loglevel", "error", video))
    yield lines.length > 0

  // NOTE: this runs via Jobs because it's expensive and we need to track it
  def extractAudioTrack (video :Path, videoUrl :String, audio :Path) :Future[File] =
    for (_ <- Jobs.runCommand("ffmpeg", "-y", "-i", video, audio))
    yield File(s"audio:${videoUrl}", Files.size(audio),
               Util.guessMimeTypeFromSuffix(audio.toString, "audio/mp3"), audio)

  def extractVideoThumbnail (video :Path, videoUrl :String, thumb :Path) :Future[File] =
    for (duration <- extractVideoDuration(video) ;
         _ <- runCommand("ffmpeg", "-y", "-ss", (duration/2), "-i", video, "-frames:v", "1", thumb))
    yield File(s"thumbnail:${videoUrl}", Files.size(thumb),
               Util.guessMimeTypeFromSuffix(thumb.toString, "image/jpg"), thumb)

  def extractVideoDuration (video :Path) :Future[Double] =
    for (lines <- runCommand("ffprobe", "-v", "error", "-show_entries", "format=duration",
                             "-of", "default=noprint_wrappers=1:nokey=1", video))
    yield if (lines.length != 1) throw new Exception(lines.mkString("\n"))
          else lines(0).toDouble

  def trimMedia (media :Path, start :Int, end :Int, trimmed :Path) :Future[Long] =
    for (_ <- runCommand("ffmpeg", "-y", "-ss", start, "-t", end-start, "-i", media, trimmed))
    yield Files.size(trimmed)

  def runCommand (command :Any*) :Future[Seq[String]] = Future(blocking { execCommand(command*) })

  def execCommand (command :Any*) :Seq[String] = {
    import java.io._
    import java.util.stream._
    log.info(s"Exec: ${command.mkString(" ")}")
    val proc = new ProcessBuilder(command.map(_.toString)*).redirectErrorStream(true).start()
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    import scala.jdk.CollectionConverters._
    val lines = reader.lines.collect(Collectors.toList).asScala
    val exitCode = proc.waitFor
    if (exitCode != 0) {
      log.warning(lines.mkString("\n"))
      throw Exception(s"${command(0)} failed: $exitCode")
    }
    Seq() ++ lines
  }
}
