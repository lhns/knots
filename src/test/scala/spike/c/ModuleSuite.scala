package spike.c.tests

import spike.c.*

import java.util.concurrent.atomic.AtomicInteger

object ModFixtures {
  case class AppConfig(dbUrl: String, port: Int)
  case class Transactor(url: String)
  case class UserRepo(tx: Transactor)
  case class Routes(repo: UserRepo, port: Int)

  trait DbDeps(using Upper[DbDeps]) {
    def transactor: Transactor = inherited
    def userRepo: UserRepo = inherited
  }
  trait ServerDeps(using Upper[ServerDeps]) { def routes: Routes = inherited }
  trait MainDeps(using Upper[MainDeps]) {
    def config: AppConfig = inherited
    def db: DbDeps = inherited
    def server: ServerDeps = inherited
  }

  val created = new AtomicInteger

  def dbBase(url: => String): Overridable[DbDeps] = Overridable[DbDeps](new DbDeps {
    override lazy val transactor = { created.incrementAndGet(); Transactor(url) }
    override lazy val userRepo = UserRepo(self.transactor)
  })

  def serverBase(repo: => UserRepo, port: => Int): Overridable[ServerDeps] =
    Overridable[ServerDeps](new ServerDeps { override lazy val routes = Routes(repo, port) })

  val app = Overridable[MainDeps](new MainDeps {
    override lazy val config = AppConfig("jdbc:prod", 8080)
    override lazy val db = module(dbBase(self.config.dbUrl))
    override lazy val server = module(serverBase(self.db.userRepo, self.config.port))
  })

  // Strict val and def forms of the same app.
  val appStrict = Overridable[MainDeps](new MainDeps {
    override val config = AppConfig("jdbc:v", 1)
    override def db = module(dbBase(self.config.dbUrl))
    override val server = module(serverBase(self.db.userRepo, self.config.port))
  })

  trait Inner(using Upper[Inner]) { def v: Int = inherited }
  trait Mid(using Upper[Mid]) { def inner: Inner = inherited; def w: Int = inherited }
  trait Top(using Upper[Top]) { def mid: Mid = inherited; def x: Int = inherited }

  val top = Overridable[Top](new Top {
    override lazy val x = 1
    override lazy val mid = module(Overridable[Mid](new Mid {
      override lazy val w = self.inner.v + 10
      override lazy val inner = module(Overridable[Inner](new Inner { override lazy val v = 5 }))
    }))
  })
}

class ModuleSuite extends munit.FunSuite {
  import ModFixtures.*

  private def routesOf(m: MainDeps) = m.server.routes

  test("a module is built from the parent's members") {
    assertEquals(routesOf(Overridable.build(app)), Routes(UserRepo(Transactor("jdbc:prod")), 8080))
  }

  test("modules work as strict val and def members") {
    assertEquals(routesOf(Overridable.build(appStrict)), Routes(UserRepo(Transactor("jdbc:v")), 1))
  }

  test("late binding reaches into modules: overriding config changes db and server") {
    val o = Override[MainDeps](new MainDeps { override lazy val config = AppConfig("jdbc:test", 9090) })
    assertEquals(routesOf(Overridable.build(app, o)), Routes(UserRepo(Transactor("jdbc:test")), 9090))
  }

  test("a deep override changes one member of a module and the rest of the tree sees it") {
    val o = Override[MainDeps](new MainDeps {
      override lazy val db = module(Override[DbDeps](new DbDeps { override lazy val transactor = Transactor("fake") }))
    })
    val m = Overridable.build(app, o)
    assertEquals(m.db.transactor, Transactor("fake"))
    assertEquals(m.db.userRepo, UserRepo(Transactor("fake")))
    assertEquals(routesOf(m).repo.tx, Transactor("fake"))
  }

  test("a deep override can use upper of the module") {
    val o = Override[MainDeps](new MainDeps {
      override lazy val db = module(Override[DbDeps](new DbDeps {
        override lazy val transactor = Transactor("wrapped(" + upper.transactor.url + ")")
      }))
    })
    assertEquals(Overridable.build(app, o).db.userRepo, UserRepo(Transactor("wrapped(jdbc:prod)")))
  }

  test("deep overrides stack, each seeing the previous as upper") {
    val o = Override[MainDeps](new MainDeps {
      override lazy val db = module(Override[DbDeps](new DbDeps {
        override lazy val transactor = Transactor("a(" + upper.transactor.url + ")")
      }))
    })
    assertEquals(Overridable.build(app, o, o).db.transactor, Transactor("a(a(jdbc:prod))"))
  }

  test("a module given as Overridable replaces the module completely") {
    val o = Override[MainDeps](new MainDeps { override lazy val db = module(dbBase("jdbc:other")) })
    val m = Overridable.build(app, o)
    assertEquals(m.db.transactor, Transactor("jdbc:other"))
    assertEquals(routesOf(m).repo.tx, Transactor("jdbc:other"))
  }

  test("modules are built per app: two apps share nothing") {
    created.set(0)
    val a1 = Overridable.build(app)
    val a2 = Overridable.build(app)
    a1.db.transactor; a2.db.transactor; a1.db.transactor
    assertEquals(created.get, 2)
    assert(!(a1.db.transactor eq a2.db.transactor))
    assert(a1.db eq a1.db)
  }

  test("nested modules: three levels") {
    val t = Overridable.build(top)
    assertEquals((t.mid.w, t.mid.inner.v, t.x), (15, 5, 1))
  }

  test("nested modules: a deep override of the innermost member reaches its siblings") {
    val o = Override[Top](new Top {
      override lazy val mid = module(Override[Mid](new Mid {
        override lazy val inner = module(Override[Inner](new Inner { override lazy val v = upper.v * 2 }))
      }))
    })
    val t = Overridable.build(top, o)
    assertEquals((t.mid.w, t.mid.inner.v), (20, 10))
  }

  test("a cycle inside a module throws CycleException") {
    val o = Override[MainDeps](new MainDeps {
      override lazy val db = module(Override[DbDeps](new DbDeps {
        override lazy val transactor = Transactor(self.userRepo.tx.url)
      }))
    })
    intercept[CycleException](Overridable.build(app, o).db.userRepo)
  }

  test("a cycle across modules throws CycleException") {
    val o = Override[MainDeps](new MainDeps {
      override lazy val config = AppConfig(self.server.routes.repo.tx.url, 1)
    })
    intercept[CycleException](routesOf(Overridable.build(app, o)))
  }

  test("an unrewritten module(...) fails at runtime") {
    val e = intercept[IllegalStateException](module(Override[DbDeps](new DbDeps {}))(using null))
    assert(e.getMessage.contains("module(...) must be the whole body"), e.getMessage)
  }
}
