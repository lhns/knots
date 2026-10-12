package spike.a

import cats.syntax.all.*

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.*

class ModulesSuite extends munit.FunSuite {
  override val munitTimeout: Duration = 1.minute

  private def fakeDb(wrap: String) = Override[AppDeps]((
    db = module(Override[DbDeps]((transactor = Transactor(wrap + "(" + upper.transactor.url + ")"))))))

  test("modules are tied through the tree") {
    val app = App()
    assertEquals(Overridable.build(app.base).server.routes, Routes(UserRepo(Transactor("jdbc:prod"))))
  }

  test("late binding reaches into modules from the parent") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((config = AppConfig("jdbc:test", 9090))))
    assertEquals(v.server.routes.repo.tx.url, "jdbc:test")
    assertEquals(v.server.port, 9090)
  }

  test("a deep override reaches modules that depend on it") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((db = module(Override[DbDeps]((transactor = Transactor("fake")))))))
    assertEquals(v.server.routes.repo.tx, Transactor("fake"))
  }

  test("a deep override can use upper") {
    val app = App()
    assertEquals(Overridable.build(app.base, fakeDb("w")).db.userRepo.tx.url, "w(jdbc:prod)")
  }

  test("deep overrides stack through upper, flat, layered and composed") {
    val app = App()
    val expected = "o2(o1(jdbc:prod))"
    assertEquals(Overridable.build(app.base, fakeDb("o1"), fakeDb("o2")).server.routes.repo.tx.url, expected)
    assertEquals(Overridable.build(app.base, fakeDb("o1") |+| fakeDb("o2")).db.transactor.url, expected)
    assertEquals(Overridable.build(app.base |+| fakeDb("o1"), fakeDb("o2")).db.transactor.url, expected)
  }

  test("a module is replaced by a complete one") {
    val app = App()
    val replaced = Override[AppDeps]((db = module(app.dbBase("jdbc:other"))))
    assertEquals(Overridable.build(app.base, replaced).server.routes.repo.tx.url, "jdbc:other")
    assertEquals(Overridable.build(app.base |+| replaced).server.routes.repo.tx.url, "jdbc:other")
  }

  test("a module is untouched by an unrelated override") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((config = AppConfig("x", 1))))
    assertEquals(v.db.transactor, Transactor("x"))
    assertEquals(v.toTuple.db.userRepo, UserRepo(Transactor("x")))
  }

  test("toTuple of the root gives the module views") {
    val v = Overridable.build(App().base)
    assertEquals(v.toTuple.db.transactor, Transactor("jdbc:prod"))
  }

  test("two apps built from one definition share nothing") {
    val app = App()
    val a1 = Overridable.build(app.base)
    val a2 = Overridable.build(app.base)
    a1.db.transactor; a2.db.transactor; a1.db.transactor
    assertEquals(app.transactors.get, 2)
    assert(a1.db.transactor ne a2.db.transactor)
    assert(a1.db ne a2.db)
  }

  test("a module is computed once per app") {
    val app = App()
    val v = Overridable.build(app.base)
    v.db.transactor; v.server.routes; v.db.userRepo
    assertEquals(app.transactors.get, 1)
  }

  test("a cycle inside an overridden module shows the module path") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((
      db = module(Override[DbDeps]((transactor = Transactor(self.userRepo.tx.url)))))))
    val e = intercept[CycleException](v.db.userRepo)
    assert(e.getMessage.contains("db."), clue(e.getMessage))
  }

  test("a cycle inside a base module shows the module path") {
    val v = Overridable.build(Overridable[AppDeps]((
      config = AppConfig("a", 1),
      db = module(cyclicDb),
      server = module(App().serverBase(self.db.userRepo, 1)))))
    val e = intercept[CycleException](v.server.routes)
    assert(e.getMessage.contains("db.transactor (base)") && e.getMessage.contains("db.userRepo (base)"), clue(e.getMessage))
  }

  test("a cycle across modules through a parent field") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((config = AppConfig(self.server.routes.repo.tx.url, 1))))
    val e = intercept[CycleException](v.server.routes)
    assert(e.getMessage.contains("config") && e.getMessage.contains("server."), clue(e.getMessage))
  }

  test("a cycle across modules through a module body") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((db = module(app.dbBase(self.server.routes.repo.tx.url)))))
    val e = intercept[CycleException](v.server.routes)
    assert(e.getMessage.contains("db.") && e.getMessage.contains("server."), clue(e.getMessage))
  }

  test("a cycle in a module throws in every thread") {
    val v = Overridable.build(Overridable[AppDeps]((
      config = AppConfig("a", 1),
      db = module(cyclicDb),
      server = module(App().serverBase(self.db.userRepo, 1)))))
    val executor = Executors.newFixedThreadPool(4)
    try {
      val reads = (1 to 4).map(_ => executor.submit(() => try v.db.transactor.url catch case e: CycleException => "cycle"))
      assertEquals(reads.map(_.get(5, TimeUnit.SECONDS)).toList, List.fill(4)("cycle"))
    } finally executor.shutdownNow()
  }

  test("a module next to other fields in one override: each field on its own") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((
      config = AppConfig("jdbc:multi", 1),
      db = module(Override[DbDeps]((transactor = Transactor("w(" + upper.transactor.url + ")")))),
      server = module(Override[ServerDeps]((port = upper.port + 1))))))
    assertEquals(v.db.transactor.url, "w(jdbc:multi)")
    assertEquals(v.server.port, 2)
    assertEquals(v.config, AppConfig("jdbc:multi", 1))
  }

  test("a module field of a multi-field override is not computed when only another field is read") {
    val app = App()
    val v = Overridable.build(app.base, Override[AppDeps]((
      config = AppConfig("jdbc:multi", 1),
      db = module(app.dbBase("jdbc:other")))))
    assertEquals(v.config.port, 1)
    assertEquals(app.transactors.get, 0)
    assertEquals(v.db.transactor.url, "jdbc:other")
    assertEquals(app.transactors.get, 1)
  }
}
