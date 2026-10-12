package spike.e

import java.util.concurrent.atomic.AtomicInteger

import Fx.*

object ModFx {
  case class Cfg(url: String)
  case class Tx(url: String)
  case class Repo(tx: Tx)

  trait DbDeps { def tx: Tx; def repo: Repo }
  trait Inner { def leaf: String; def derived: String }
  trait Mid { def inner: Inner; def m: String }
  trait Root { def cfg: Cfg; def db: DbDeps; def db2: DbDeps; def mid: Mid; def back: String }
  trait Cyc { def a: String; def db: DbDeps }
  type VM = View[(cfg: Cfg, db: DbDeps)]

  // `created` counts the transactors a module builds.
  def dbBase(url: => String, created: AtomicInteger = new AtomicInteger): Overridable[DbDeps] = Overridable[DbDeps]((
    tx = { created.incrementAndGet(); Tx(url) },
    repo = Repo(self.tx),
  ))
  val innerBase: Overridable[Inner] = Overridable[Inner]((leaf = "leaf", derived = "derived(" + self.leaf + ")"))
  val midBase: Overridable[Mid] = Overridable[Mid]((inner = module(innerBase), m = "m:" + self.inner.derived))

  def root(created: AtomicInteger = new AtomicInteger) = Overridable[Root]((
    cfg = Cfg("prod"),
    db = module(dbBase(self.cfg.url, created)),
    db2 = module(dbBase(self.cfg.url + "2")),
    mid = module(midBase),
    back = "back",
  ))
}

class ModulesSuite extends munit.FunSuite {
  import ModFx.*

  test("a module's fields are wired from the parent") {
    val r = Overridable.build(root())
    assertEquals(r.db.repo, Repo(Tx("prod")))
    assertEquals(r.mid.m, "m:derived(leaf)")
  }

  test("two fields of the same module type are independent") {
    val r = Overridable.build(root())
    assertEquals((r.db.repo, r.db2.repo), (Repo(Tx("prod")), Repo(Tx("prod2"))))
  }

  test("late binding reaches into a module: overriding the parent's config changes the module") {
    val r = Overridable.build(root(), Override[Root]((cfg = Cfg("test"))))
    assertEquals(r.db.repo, Repo(Tx("test")))
    assertEquals(r.db2.repo, Repo(Tx("test2")))
  }

  test("a module's own late binding: repo sees an overridden tx") {
    val r = Overridable.build(root(), Override[Root]((db = module(Override[DbDeps]((tx = Tx("fake")))))))
    assertEquals(r.db.repo, Repo(Tx("fake")))
  }

  test("a deep override uses upper of the module below") {
    val r = Overridable.build(root(), Override[Root]((db = module(Override[DbDeps]((tx = Tx("wrapped(" + upper.tx.url + ")")))))))
    assertEquals(r.db.repo, Repo(Tx("wrapped(prod)")))
  }

  test("a deep override two levels down") {
    val o = Override[Root]((mid = module(Override[Mid]((inner = module(Override[Inner]((leaf = "LEAF:" + upper.leaf))))))))
    val r = Overridable.build(root(), o)
    assertEquals((r.mid.inner.derived, r.mid.m), ("derived(LEAF:leaf)", "m:derived(LEAF:leaf)"))
  }

  test("a module replaced by a complete base in an override") {
    val r = Overridable.build(root(), Override[Root]((db = module(dbBase("replaced")))))
    assertEquals(r.db.repo, Repo(Tx("replaced")))
  }

  test("overriding one module does not leak into a sibling of the same type") {
    val r = Overridable.build(root(), Override[Root]((db = module(Override[DbDeps]((tx = Tx("only-db")))))))
    assertEquals((r.db.repo, r.db2.repo), (Repo(Tx("only-db")), Repo(Tx("prod2"))))
  }

  test("a module's field is computed once") {
    val n = new AtomicInteger
    val r = Overridable.build(root(), Override[Root]((db = module(Override[DbDeps]((tx = { n.incrementAndGet(); Tx("t") }))))))
    r.db.tx; r.db.repo; r.db2.repo; r.db.tx
    assertEquals(n.get, 1)
  }

  test("a module's layer given by a variable, an object literal, a named class and a value with context") {
    val dbo = Override[DbDeps]((tx = Tx("var")))
    assertEquals(Overridable.build(root(), Override[Root]((db = module(dbo)))).db.repo, Repo(Tx("var")))
    assertEquals(Overridable.build(root(), Override[Root](new { def db = module(Override[DbDeps]((tx = Tx("lit")))) })).db.repo, Repo(Tx("lit")))
    class M(using Upper[Root]) { def db: Module[DbDeps] = module(Override[DbDeps]((tx = Tx("cls")))) }
    assertEquals(Overridable.build(root(), Override[Root](new M)).db.repo, Repo(Tx("cls")))
    def mk(using Upper[Root]) = (db = module(Override[DbDeps]((tx = Tx("viaValue")))))
    assertEquals(Overridable.build(root(), Override[Root](mk)).db.repo, Repo(Tx("viaValue")))
  }

  test("a module in a View target, overridden deeply") {
    val v = Overridable[VM]((cfg = Cfg("v"), db = module(dbBase(self.cfg.url))))
    val r = Overridable.build(v, Override[VM]((db = module(Override[DbDeps]((tx = Tx("o:" + upper.tx.url)))))))
    assertEquals(r.db.repo, Repo(Tx("o:v")))
    assertEquals(r.toTuple.cfg, Cfg("v"))
  }

  test("a module whose trait is a View") {
    type Sub = View[(tx: String, repo: String)]
    trait VRoot { def db: Sub; def url: String }
    val v = Overridable[VRoot]((db = module(Overridable[Sub]((tx = "t", repo = "r:" + self.tx))), url = "u"))
    val r = Overridable.build(v, Override[VRoot]((db = module(Override[Sub]((tx = "T:" + upper.tx))))))
    assertEquals(r.db.repo, "r:T:t")
    assertEquals(r.db.toTuple, (tx = "T:t", repo = "r:T:t"))
  }

  test("two apps built from the same base share nothing") {
    val created = new AtomicInteger
    val app = root(created)
    val a1 = Overridable.build(app)
    val a2 = Overridable.build(app)
    a1.db.tx; a2.db.tx; a1.db.tx
    assertEquals(created.get, 2)
    assertEquals(a1.db.tx.url, a2.db.tx.url)
    assert(!(a1.db.tx eq a2.db.tx))
  }

  test("two apps with different overrides stay independent") {
    val app = root()
    val a1 = Overridable.build(app, Override[Root]((cfg = Cfg("one"))))
    val a2 = Overridable.build(app, Override[Root]((cfg = Cfg("two"))))
    assertEquals((a1.db.tx.url, a2.db.tx.url), ("one", "two"))
  }

  test("a cycle inside a module is detected") {
    val bad = Overridable[DbDeps]((tx = Tx(self.repo.tx.url), repo = Repo(self.tx)))
    val r = Overridable.build(root(), Override[Root]((db = module(bad))))
    assertEquals(timed()(intercept[CycleException](r.db.repo)).getMessage, "cyclic dependency: repo -> tx -> repo")
  }

  test("a cycle inside a deep override of a module is detected") {
    val r = Overridable.build(root(), Override[Root]((db = module(Override[DbDeps]((tx = Tx(self.repo.tx.url)))))))
    assertEquals(timed()(intercept[CycleException](r.db.repo)).getMessage, "cyclic dependency: repo -> tx -> repo")
  }

  test("a module's requirement is by-name: it is not read until the module needs it") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[Cyc]((a = "a", db = module(dbBase({ n.incrementAndGet(); "u" })))))
    assertEquals(n.get, 0)
    assertEquals(r.db.repo, Repo(Tx("u")))
    assertEquals(n.get, 1)
  }
}
