package mediares

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import java.io.{InputStream, OutputStream}
import java.net.{URL, HttpURLConnection}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import upickle.default._

object Transfer {

  import Pool.ctx
  private val log = java.util.logging.Logger.getLogger("upload")

  private val statuses = ConcurrentHashMap[String, Status]()

  /** Operational stats. */
  def stats = Status.summarize(statuses.values.asScala)

  /** Returns the current status of a `transfer` request. */
  def status (key :String) :Future[Option[Status]] = Future.successful(Option(statuses.get(key)))

  /** Downloads the contents of `sourceUrl` and uploads it to `destUrl`.
    * @return a record which communicates the ongoing status of the transfer.
    */
  def transfer (key :String, sourceUrl :String, destUrl :String) :Status = {
    val sourceConn = new URL(sourceUrl).openConnection.asInstanceOf[HttpURLConnection]
    sourceConn.setRequestMethod("GET")
    sourceConn.setDoOutput(false)

    val responseCode = sourceConn.getResponseCode
    if (responseCode != 200) {
      log.warning(s"Failed to GET source [url=$sourceUrl, rcode=$responseCode]")
      throw Exception(s"Media GET failed: $responseCode")
    }
    val contentLength = sourceConn.getContentLengthLong
    val contentType = sourceConn.getContentType

    log.info(s"Transferring [source=$sourceUrl, type=$contentType, size=$contentLength, dest=$destUrl]")
    val destConn = new URL(destUrl).openConnection.asInstanceOf[HttpURLConnection]
    destConn.setRequestMethod("PUT")
    destConn.setRequestProperty("Content-Type", contentType)
    destConn.setDoOutput(true)

    val sent = AtomicLong(0)
    val copy = Future {
      copyStreams(sourceConn.getInputStream, destConn.getOutputStream, sent)
      sourceConn.disconnect()
      val destCode = destConn.getResponseCode()
      if (destCode != 200) throw Exception(s"Upload failed, code: $destCode")
      destConn.disconnect()
    }
    val res = copy.map(_ => key)
    val status = new Status(key) {
      def progress = Progress(sent.get, contentLength)
      def result = res.value
    }
    statuses.put(key, status)

    res.onComplete(res => {
      statuses.put(key, res match {
        case util.Success(key) => status.toComplete
        case util.Failure(err) => FailedStatus(key, err)
      })
      log.info(s"Transfer complete [key=$key, result=$res]")
    })

    status
  }

  def copyStreams (in :InputStream, out :OutputStream, progress :AtomicLong) = {
    val buffer = new Array[Byte](64*1024)
    var read = in.read(buffer, 0, buffer.length)
    while (read >= 0) {
      out.write(buffer, 0, read)
      progress.addAndGet(read)
      read = in.read(buffer, 0, buffer.length)
    }
    out.close()
  }
}
