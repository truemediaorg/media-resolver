package mediares

import scala.concurrent.{ExecutionContext, Future, blocking}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.Executors

/** Provides a thread pool on which we run our async jobs. */
object Pool {

  def procCount = Runtime.getRuntime.availableProcessors

  val pool = Executors.newFixedThreadPool(Math.max(procCount-1, 1))
  implicit val ctx :ExecutionContext = ExecutionContext.fromExecutor(pool)
}
