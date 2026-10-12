package spike.e

import java.util.concurrent.atomic.AtomicReference

// Shared fixtures and helpers for the spike E suites.
object Fx {
  case class D(n: String)

  trait AB { def a: D; def b: D }
  trait ABC { def a: D; def b: D; def c: D }

  // Runs `body` on a daemon thread so a regression into a hang fails the test instead of the run.
  def timed[T](secs: Int = 10)(body: => T): T = {
    val result = new AtomicReference[Either[Throwable, T] | Null]
    val t = new Thread(() => result.set(try Right(body) catch { case e: Throwable => Left(e) }))
    t.setDaemon(true)
    t.start()
    t.join(secs * 1000L)
    result.get match {
      case null => throw new AssertionError(s"did not finish within ${secs}s (hang)")
      case Right(v) => v
      case Left(e) => throw e
    }
  }
}
