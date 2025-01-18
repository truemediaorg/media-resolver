package mediares

import scala.concurrent.{Await, Future, blocking}
import scala.util.{Try, Success, Failure}
import sttp.client4._
import upickle.default._

object Webhook {

  private val log = java.util.logging.Logger.getLogger("webhook")
  import Pool.ctx
  private val backend = DefaultFutureBackend()

  // type Result = { result: "cached", id :string, size :number, url :string }
  //             | { result: "failed", id :string, error :string }
  trait Result {
    def id :String
    def toJson :String
  }
  case class Cached (result :String, id :String, size :Long, url :String) extends Result derives ReadWriter {
    def toJson = write(this)
  }
  case class Failed (result :String, id :String, error :String) extends Result derives ReadWriter {
    def toJson = write(this)
  }

  def queue (url :String, status :Status) :Unit = Future { call(url, status) }

  def call (url :String, status :Status) :Unit = {
    val result = status.result match {
      case None => // this should never happen
        log.warning(s"Webhook given incomplete status [status=$status]")
        Failed("failed", status.key, "Internal error")
      case Some(Failure(err)) =>
        Failed("failed", status.key, err.getMessage)
      case Some(Success(url)) =>
        Cached("cached", status.key, status.progress.total, url)
    }
    basicRequest.
      post(uri"$url").
      body(result.toJson).
      send(backend).
      map { _.body match {
        case Left(error) => log.warning(s"Webhook call failed [id=${result.id}, url=$url, error=$error]")
        case Right(rsp) => log.info(s"Webhook call delivered [id=${result.id}, url=$url, rsp=$rsp]")
      }}
  }
}
