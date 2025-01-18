package mediares

import scala.concurrent.{ExecutionContext, Future, blocking}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors

/** Queues expensive jobs to avoid overloading the server. */
object Jobs {

  val MaxPendingJobs = Env.getInt("MAX_PENDING_JOBS", Pool.procCount)
  val pendingJobs = new AtomicInteger(0)

  def atMaxJobs = pendingJobs.get >= MaxPendingJobs

  def startJob[T] (body : => T) :Future[T] = {
    pendingJobs.addAndGet(1)
    val f = Future(body)(Pool.ctx)
    f.onComplete(_ => pendingJobs.addAndGet(-1))(Pool.ctx)
    f
  }

  def runCommand (command :Any*) :Future[Seq[String]] = startJob(blocking { Local.execCommand(command*) })
}
