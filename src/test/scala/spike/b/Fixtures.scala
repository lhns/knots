package spike.b

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

// Shared targets and helpers of the spike B suites.
object Fx {
  case class Dep(name: String)

  trait Deps {
    def dep1: Dep
    def dep2: Dep
    def dep3: Dep
  }

  // dep3 uses self.dep2, to observe late binding.
  val base: Overridable[Deps] = Overridable[Deps](new {
    def dep1 = Dep("base1")
    def dep2 = Dep("base2")
    def dep3 = Dep("base3(" + self.dep2.name + ")")
  })

  // Targets the macro rejects.
  trait Overloaded { def f(x: Int): Int; def f(x: String): Int }
  trait AbsVal { val x: Int }
  trait AbsVar { var v: Int; def a: Int }
  trait ConcreteDef { def a: Int; def b: Int = a + 1 }
  trait ConcreteVal { def a: Int; val v: Int = 1 }
  trait WithDefault { def f(x: Int = 1): Int; def a: Int }
  trait Single { def a: Int }

  // Inputs: named classes and instances.
  class WrapsDep2(using Upper[Deps]) {
    def dep2 = Dep(s"wrapped(${upper.dep2.name})")
  }
  class UnknownMember(using Upper[Deps]) { lazy val dep9 = Dep("x") }
  class WrongType { lazy val dep2 = "x" }
  class FullBase { lazy val dep1 = Dep("n1"); lazy val dep2 = Dep("n2"); lazy val dep3 = Dep("n3") }
  class Helper { def helper = 1; def dep1 = Dep("x") }
  case class OnlyDep1(dep1: Dep)

  // Higher-kinded target.
  trait HkDeps[F[_]] { def depA: F[Dep]; def depB: F[Dep] }

  // Modules.
  case class AppConfig(dbUrl: String, port: Int)
  case class Transactor(url: String)
  case class UserRepo(tx: Transactor)
  case class Routes(repo: UserRepo, port: Int)

  trait DbDeps { def transactor: Transactor; def userRepo: UserRepo }
  trait ServerDeps { def routes: Routes }
  trait AppDeps { def config: AppConfig; def db: DbDeps; def server: ServerDeps }

  def dbModule(url: => String, created: AtomicInteger): Overridable[DbDeps] =
    Overridable[DbDeps](new {
      def transactor = { created.incrementAndGet(); Transactor(url) }
      def userRepo = UserRepo(self.transactor)
    })

  def serverModule(userRepo: => UserRepo, port: => Int): Overridable[ServerDeps] =
    Overridable[ServerDeps](new { def routes = Routes(userRepo, port) })

  def app(created: AtomicInteger): Overridable[AppDeps] = Overridable[AppDeps](new {
    def config = AppConfig("jdbc:prod", 8080)
    def db = module(dbModule(self.config.dbUrl, created))
    def server = module(serverModule(self.db.userRepo, self.config.port))
  })

  /** Runs each task on its own thread (daemon platform or virtual); fails if one is still running after 10s. */
  def inThreads[A](tasks: List[() => A], virtual: Boolean = false): List[Either[Throwable, A]] = {
    val results = new java.util.concurrent.ConcurrentHashMap[Int, Either[Throwable, A]]
    val threads = tasks.zipWithIndex.map { (task, i) =>
      val body: Runnable = () => results.put(i, try Right(task()) catch { case e: Throwable => Left(e) })
      val t = if virtual then Thread.ofVirtual().unstarted(body) else { val t = new Thread(body); t.setDaemon(true); t }
      t.start()
      t
    }
    threads.foreach(_.join(10000))
    if threads.exists(_.isAlive) then throw new AssertionError("threads still running after 10s (hang)")
    tasks.indices.map(results.get).toList
  }

  /** Runs `body` with a virtual-thread executor that is always shut down afterwards. */
  def withVirtualExecutor[A](body: java.util.concurrent.ExecutorService => A): A = {
    val ex = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
    try body(ex)
    finally { ex.shutdown(); ex.awaitTermination(20, TimeUnit.SECONDS) }
  }
}
