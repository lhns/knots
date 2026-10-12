package spike.b

import java.util.concurrent.atomic.AtomicInteger

import Fx.*

class ModulesSpec extends munit.FunSuite {
  private val created = new AtomicInteger
  private def build(overrides: Override[AppDeps]*): AppDeps = Overridable.build(app(created), overrides*)

  private val prodTx = Transactor("jdbc:prod")

  test("modules wire the tree") {
    val d = build()
    assertEquals(d.server.routes, Routes(UserRepo(prodTx), 8080))
  }

  test("a root override reaches the modules through the tree (late binding)") {
    val d = build(Override[AppDeps](new { def config = AppConfig("jdbc:test", 9090) }))
    assertEquals(d.db.transactor, Transactor("jdbc:test"))
    assertEquals(d.server.routes, Routes(UserRepo(Transactor("jdbc:test")), 9090))
  }

  test("a deep override of a module member reaches its siblings") {
    val d = build(Override[AppDeps](new {
      def db = module(Override[DbDeps](new { def transactor = Transactor("fake") }))
    }))
    assertEquals(d.db.userRepo, UserRepo(Transactor("fake")))
    assertEquals(d.server.routes.repo, UserRepo(Transactor("fake")))
  }

  test("a deep override can use upper of the module") {
    val d = build(Override[AppDeps](new {
      def db = module(Override[DbDeps](new { def transactor = Transactor("wrapped(" + upper.transactor.url + ")") }))
    }))
    assertEquals(d.db.transactor, Transactor("wrapped(jdbc:prod)"))
    assertEquals(d.db.userRepo, UserRepo(Transactor("wrapped(jdbc:prod)")))
  }

  test("a deep override sees a root override below it through self") {
    val d = build(
      Override[AppDeps](new { def config = AppConfig("jdbc:test", 1) }),
      Override[AppDeps](new {
        def db = module(Override[DbDeps](new { def transactor = Transactor("t:" + upper.transactor.url) }))
      }))
    assertEquals(d.server.routes.repo, UserRepo(Transactor("t:jdbc:test")))
  }

  test("deep overrides of the same module in several layers stack") {
    val wrap = Override[AppDeps](new {
      def db = module(Override[DbDeps](new { def transactor = Transactor("w(" + upper.transactor.url + ")") }))
    })
    assertEquals(build(wrap, wrap).db.transactor, Transactor("w(w(jdbc:prod))"))
  }

  test("a module replaced completely") {
    val d = build(Override[AppDeps](new { def db = module(dbModule("jdbc:other", created)) }))
    assertEquals(d.server.routes.repo, UserRepo(Transactor("jdbc:other")))
  }

  test("a module can be replaced by a complete module and then deep-overridden") {
    val d = build(
      Override[AppDeps](new { def db = module(dbModule("jdbc:other", created)) }),
      Override[AppDeps](new { def db = module(Override[DbDeps](new { def transactor = Transactor(upper.transactor.url + "+") })) }))
    assertEquals(d.db.transactor, Transactor("jdbc:other+"))
  }

  test("two apps share nothing") {
    val count = new AtomicInteger
    val a1 = Overridable.build(app(count))
    val a2 = Overridable.build(app(count))
    val t1 = a1.db.transactor
    val t2 = a2.db.transactor
    assertEquals(a1.db.transactor, t1)
    assert(!(t1 eq t2))
    assertEquals(count.get, 2)
  }

  test("a module member is computed once") {
    val count = new AtomicInteger
    val d = Overridable.build(app(count))
    d.db.transactor; d.server.routes; d.db.userRepo
    assertEquals(count.get, 1)
  }

  test("a module is a base: it can be built on its own") {
    val d = Overridable.build(dbModule("jdbc:solo", new AtomicInteger))
    assertEquals(d.userRepo, UserRepo(Transactor("jdbc:solo")))
  }

  test("a cycle inside a module throws") {
    val d = build(Override[AppDeps](new {
      def db = module(Override[DbDeps](new { def transactor = Transactor(self.userRepo.tx.url) }))
    }))
    val e = intercept[CycleException](d.db.userRepo)
    assert(e.path.nonEmpty)
  }

  test("a cycle across modules throws") {
    val d = build(Override[AppDeps](new { def config = AppConfig(self.server.routes.repo.tx.url, 1) }))
    intercept[CycleException](d.server.routes)
  }
}
