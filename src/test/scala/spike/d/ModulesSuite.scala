package spike.d

import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

class ModulesSuite extends munit.FunSuite {
  import ModulesSuite.*

  private val fakeDb = Override[MainDeps]((db = module(Override[DbDeps]((transactor = Transactor("fake(" + upper.transactor.url + ")"))))))

  test("the module graph is wired from the parent") {
    val d = Overridable.build(app)
    assertEquals(d.server.routes, Routes(UserRepo(Transactor("jdbc:prod")), 8080))
  }

  test("late binding through the tree: an override of config reaches the modules") {
    val d = Overridable.build(app, Override[MainDeps]((config = AppConfig("jdbc:test", 9090))))
    assertEquals(d.db.transactor, Transactor("jdbc:test"))
    assertEquals(d.server.routes, Routes(UserRepo(Transactor("jdbc:test")), 9090))
  }

  test("module members are computed once") {
    val n = new AtomicInteger
    val a = Overridable[MainDeps]((
      config = AppConfig("u", 1),
      db = module(Overridable[DbDeps]((transactor = { n.incrementAndGet(); Transactor("t") }, userRepo = UserRepo(self.transactor)))),
      server = module(ServerModule.base(self.db.userRepo, self.config.port)),
    ))
    val d = Overridable.build(a)
    assertEquals((d.db.transactor, d.db.transactor, d.server.routes.userRepo.transactor), (Transactor("t"), Transactor("t"), Transactor("t")))
    assertEquals(n.get, 1)
  }

  test("modules are lazy") {
    val n = new AtomicInteger
    val a = Overridable[MainDeps]((
      config = AppConfig("u", 1),
      db = module(DbModule.base(self.config.dbUrl)),
      server = module(Overridable[ServerDeps]((routes = { n.incrementAndGet(); sys.error("never") }))),
    ))
    val d = Overridable.build(a)
    assertEquals(d.db.transactor, Transactor("u"))
    assertEquals(n.get, 0)
  }

  test("deep override with upper: only db.transactor changes, the server sees it") {
    val d = Overridable.build(app, fakeDb)
    assertEquals(d.db.transactor, Transactor("fake(jdbc:prod)"))
    assertEquals(d.server.routes.userRepo, UserRepo(Transactor("fake(jdbc:prod)")))
    assertEquals(d.server.routes.port, 8080)
  }

  test("deep override combined with a config override") {
    val d = Overridable.build(app, fakeDb, Override[MainDeps]((config = AppConfig("jdbc:other", 1))))
    assertEquals(d.server.routes, Routes(UserRepo(Transactor("fake(jdbc:other)")), 1))
  }

  test("deep overrides stack through upper") {
    val second = Override[MainDeps]((db = module(Override[DbDeps]((transactor = Transactor("again(" + upper.transactor.url + ")"))))))
    val d = Overridable.build(app, fakeDb, second)
    assertEquals(d.db.transactor.url, "again(fake(jdbc:prod))")
  }

  test("a module in an override via |+|") {
    val d = Overridable.build(app, fakeDb |+| Override[MainDeps]((config = AppConfig("x", 1))))
    assertEquals(d.db.transactor.url, "fake(x)")
  }

  test("a module is replaced by a complete base module") {
    val replaced = Override[MainDeps]((db = module(Overridable[DbDeps]((transactor = Transactor("new"), userRepo = UserRepo(Transactor("repo")))))))
    val d = Overridable.build(app, replaced)
    assertEquals(d.db.transactor, Transactor("new"))
    assertEquals(d.server.routes.userRepo, UserRepo(Transactor("repo")))
  }

  test("module via a tuple variable, and a module override via a tuple variable") {
    // module(...) needs a Self in scope as a guard; outside a literal supply a dummy one.
    given Self[Int] = null
    val fields = (
      config = AppConfig("jdbc:tv", 1),
      db = module(DbModule.base("jdbc:tv")),
      server = module(ServerModule.base(UserRepo(Transactor("fixed")), 1)),
    )
    val a = Overridable[MainDeps](fields)
    assertEquals(Overridable.build(a).db.userRepo.transactor.url, "jdbc:tv")
    val ov = (db = module(Override[DbDeps]((transactor = Transactor("fake(" + upper.transactor.url + ")")))))
    assertEquals(Overridable.build(a, Override[MainDeps](ov)).db.userRepo.transactor.url, "fake(jdbc:tv)")
  }

  test("two apps share nothing") {
    val n = new AtomicInteger
    val a = Overridable[MainDeps]((
      config = AppConfig("u", 1),
      db = module(Overridable[DbDeps]((transactor = Transactor(s"t${n.incrementAndGet()}"), userRepo = UserRepo(self.transactor)))),
      server = module(ServerModule.base(self.db.userRepo, self.config.port)),
    ))
    val d1 = Overridable.build(a)
    val d2 = Overridable.build(a, Override[MainDeps]((config = AppConfig("other", 2))))
    assertEquals(d1.db.transactor.url, "t1")
    assertEquals(d2.db.transactor.url, "t2")
    assertEquals(d2.server.routes.port, 2)
    assertEquals(d1.server.routes.port, 1)
    assertEquals(d1.db.transactor.url, "t1")
  }

  test("a cycle inside a module") {
    val a = Overridable[MainDeps]((
      config = AppConfig("u", 1),
      db = module(Overridable[DbDeps]((transactor = Transactor(self.userRepo.transactor.url), userRepo = UserRepo(self.transactor)))),
      server = module(ServerModule.base(self.db.userRepo, self.config.port)),
    ))
    intercept[CycleException](Overridable.build(a).db.transactor)
  }

  test("a cycle across modules") {
    val a = Overridable[MainDeps]((
      config = AppConfig("u", 1),
      db = module(DbModule.base(self.server.routes.userRepo.transactor.url)),
      server = module(ServerModule.base(self.db.userRepo, self.config.port)),
    ))
    val e = intercept[CycleException](Overridable.build(a).server.routes)
    assert(e.path.nonEmpty)
    assertEquals(e.path.head, e.path.last)
  }

  test("a cycle across modules in several threads") {
    (1 to 10).foreach { _ =>
      val pool = Executors.newFixedThreadPool(2)
      try {
        val latch = new CountDownLatch(2)
        def rendezvous(): Unit = { latch.countDown(); latch.await(10, TimeUnit.SECONDS) }
        val a = Overridable[MainDeps]((
          config = AppConfig("u", 1),
          db = module(Overridable[DbDeps]((
            transactor = { rendezvous(); Transactor(self.userRepo.toString) },
            userRepo = { rendezvous(); UserRepo(self.transactor) },
          ))),
          server = module(ServerModule.base(self.db.userRepo, self.config.port)),
        ))
        val d = Overridable.build(a)
        val results = CycleSuite.run(pool, List(() => d.db.transactor, () => d.db.userRepo))
        results.foreach(r => assert(r.failed.toOption.exists(_.isInstanceOf[CycleException]), s"got $results"))
      } finally CycleSuite.shutdown(pool)
    }
  }
}

object ModulesSuite {
  case class AppConfig(dbUrl: String, port: Int)
  case class Transactor(url: String)
  case class UserRepo(transactor: Transactor)
  case class Routes(userRepo: UserRepo, port: Int)

  trait DbDeps { def transactor: Transactor; def userRepo: UserRepo }
  trait ServerDeps { def routes: Routes }
  trait MainDeps { def config: AppConfig; def db: DbDeps; def server: ServerDeps }

  object DbModule {
    def base(url: => String): Overridable[DbDeps] =
      Overridable[DbDeps]((transactor = Transactor(url), userRepo = UserRepo(self.transactor)))
  }

  object ServerModule {
    def base(userRepo: => UserRepo, port: => Int): Overridable[ServerDeps] =
      Overridable[ServerDeps]((routes = Routes(userRepo, port)))
  }

  val app: Overridable[MainDeps] = Overridable[MainDeps]((
    config = AppConfig("jdbc:prod", 8080),
    db = module(DbModule.base(self.config.dbUrl)),
    server = module(ServerModule.base(self.db.userRepo, self.config.port)),
  ))
}
