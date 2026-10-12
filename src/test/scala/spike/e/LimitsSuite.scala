package spike.e

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import Fx.*

// Pins the limits of the explicit path that Cell documents. Deterministic ones are asserted as they
// are today; the ones that hang are ignored (a hang cannot be asserted without leaking threads).
object LimitsFx {
  import ModFx.*
  trait Me { def me: Me; def x: Int; def all: List[Me] }
}

class LimitsSuite extends munit.FunSuite {
  import LimitsFx.*
  import ModFx.*

  // --- self copies have no stable identity

  test("self has no stable identity: self eq self is false") {
    val r = Overridable.build(Overridable[Me]((me = self, x = 1, all = List(self, self))))
    assert(!(r.me eq r))
    assert(!(r.me eq r.me))
    assert(r.me != r)
    assertEquals(r.all.head == r.all(1), false)
  }

  test("a copy still reads the same values as the root") {
    val r = Overridable.build(Overridable[Me]((me = self, x = 1, all = Nil)))
    assertEquals(r.me.x, r.x)
  }

  test("a module field has no stable identity either: r.db eq r.db is false") {
    val r = Overridable.build(root())
    assert(!(r.db eq r.db))
    assert(r.mid.inner != r.mid.inner)
  }

  // --- self kept after the computation

  test("a self kept from a finished computation can be used from another thread") {
    val saved = new AtomicReference[AB | Null]
    val r = Overridable.build(Overridable[AB]((a = { saved.set(self); D("a") }, b = D("b"))))
    r.a
    val res = new AtomicReference[Any]
    val t = new Thread(() => res.set(scala.util.Try(saved.get.nn.b)))
    t.start(); t.join()
    assertEquals(res.get, scala.util.Success(D("b")))
  }

  // --- a thread started inside a computation

  test("a thread started inside a field and reading it gets a false cycle (documented)") {
    val done = new CountDownLatch(1)
    val bg = new AtomicReference[Any]("not run")
    val r = Overridable.build(Overridable[AB]((
      a = {
        val t = new Thread(() => { bg.set(scala.util.Try(self.a)); done.countDown() })
        t.setDaemon(true); t.start()
        assert(done.await(10, TimeUnit.SECONDS))
        D("a")
      },
      b = D("b"),
    )))
    assertEquals(r.a, D("a"))
    assertEquals(bg.get.asInstanceOf[scala.util.Try[D]].failed.get.getMessage, "cyclic dependency: a -> a")
  }

  test("a thread started inside a field and reading another field works") {
    val bg = new AtomicReference[Any]("not run")
    val r = Overridable.build(Overridable[AB]((
      a = {
        val t = new Thread(() => bg.set(scala.util.Try(self.b)))
        t.setDaemon(true); t.start(); t.join(10000)
        D("a")
      },
      b = D("b"),
    )))
    r.a
    assertEquals(bg.get, scala.util.Success(D("b")))
  }

  // --- a cycle through a module requirement

  test("cross-module cycle through a by-name requirement is detected when read from the top first") {
    val c = Overridable[Cyc]((a = self.db.repo.tx.url, db = module(dbBase(self.a))))
    val r = Overridable.build(c)
    assertEquals(timed()(intercept[CycleException](r.a)).getMessage, "cyclic dependency: a -> db -> a")
  }

  test("cross-module cycle, db read first: hangs (documented: the captured self lost the path)".ignore) {
    val c = Overridable[Cyc]((a = self.db.repo.tx.url, db = module(dbBase(self.a))))
    val r = Overridable.build(c)
    r.db
    r.a
  }

  test("cross-module cycle, db.tx read first: hangs (documented: the captured self lost the path)".ignore) {
    val c = Overridable[Cyc]((a = self.db.repo.tx.url, db = module(dbBase(self.a))))
    Overridable.build(c).db.tx
  }

  // --- complete instances

  test("a complete instance is not cached and knows no path") {
    var k = 0
    val base = Overridable[AB](new AB { def a = { k += 1; D("a") }; def b = self.a })
    val r = Overridable.build(base)
    r.b; r.b; r.a
    assertEquals(k, 3)
  }

  test("a cycle through a complete-instance override hangs (documented)".ignore) {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val top = Override[AB](new AB { def a = self.b; def b = upper.b })
    Overridable.build(base, top).a
  }

  test("a self-cycle in a complete-instance base overflows the stack (documented: no path, no cells)") {
    val r = Overridable.build(Overridable[AB](new AB { def a = self.b; def b = self.a }))
    val res = timed()(try { r.a; null } catch { case e: StackOverflowError => e })
    assert(res != null, "expected a StackOverflowError")
  }

  // --- very long chains

  test("a chain of thousands of overrides can overflow the stack (documented)".ignore) {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val o = Override[AB]((b = upper.b))
    Overridable.build(base, List.fill(10000)(o)*).b
  }
}
