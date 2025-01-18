package mediares

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import upickle.default._

case class Progress (transferred :Long, total :Long) {
  def map (tx :Progress => Progress) = tx(this)
}

/** Reports the status of an in progress upload or transfer. */
abstract class Status (val key :String) {
  def progress :Progress
  def result :Option[Try[String]]
  def isComplete = false
  def toComplete = CompletedStatus(key, progress)

  // note: these mutable internals are guarded by a lock on the Storage.statuses table
  var webhooks :List[String] = Nil
  def addWebhook (url :String) :Unit = webhooks = url :: webhooks
}

class PreflightStatus (key :String) extends Status(key) {
  def progress = Progress(0, 0)
  def result = None
}

class FailedStatus (key :String, err :Throwable) extends Status(key) {
  def progress = Progress(0, 0)
  def result = Some(Failure(err))
  override def isComplete = true
}

class CompletedStatus (key :String, val progress :Progress) extends Status(key) {
  def result = Some(Success(Storage.getSignedUrl(key)))
  override def isComplete = true
}

object Status {

  case class Stats (pending :Int, preflight :Int, failed :Int, completed :Int) derives ReadWriter

  def summarize (statuses :Iterable[Status]) = Stats(
    statuses.count(!_.result.isDefined),
    statuses.count(_.isInstanceOf[PreflightStatus]),
    statuses.count(_.isInstanceOf[FailedStatus]),
    statuses.count(_.isInstanceOf[CompletedStatus]))

  def toTransferStatus (status :Option[Status]) = status match {
    case None => Result.TransferStatus(0, 0, null, "Unknown media file")
    case Some(status) => status.result match {
      case Some(Failure(err)) => Result.TransferStatus(0, 0, null, err.getMessage)
      case _ =>
        // if the result is incomplete (None) or failure (Some(Failure)), the url is null
        val url = status.result.fold(null)(_.fold(_ => null, url => url))
        Result.TransferStatus(status.progress.transferred, status.progress.total, url)
    }
  }
}
