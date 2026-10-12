package spike.b

import cats.Monoid
import cats.syntax.all.*

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import Fx.*

class BehaviorSpec extends munit.FunSuite {
  private def build(overrides: Override[Deps]*): Deps = Overridable.build(base, overrides*)

  // --- Laziness and compute-once ---

  test("building evaluates no member") {
    val computed = new AtomicInteger
    val b = Overridable[Single](new { def a = { computed.incrementAndGet(); 1 } })
    val o = Override[Single](new { def a = { computed.incrementAndGet(); 2 } })
    Overridable.build(b, o)
    assertEquals(computed.get, 0)
  }

  test("only the requested members are computed") {
    val computed = new AtomicInteger
    val d = build(Override[Deps](new { def dep1 = { computed.incrementAndGet(); Dep("o1") } }))
    assertEquals(d.dep2, Dep("base2"))
    assertEquals(computed.get, 0)
  }

  test("a def member is computed once") {
    val computed = new AtomicInteger
    val d = build(Override[Deps](new { def dep2 = { computed.incrementAndGet(); Dep("c") } }))
    d.dep2; d.dep2
    assertEquals(computed.get, 1)
  }

  test("a lazy val member is computed once") {
    val computed = new AtomicInteger
    val d = build(Override[Deps](new { lazy val dep2 = { computed.incrementAndGet(); Dep("c") } }))
    d.dep2; d.dep2
    assertEquals(computed.get, 1)
  }

  test("a base member is computed once even when several members use it") {
    val computed = new AtomicInteger
    val b = Overridable[Deps](new {
      def dep1 = { computed.incrementAndGet(); Dep("1") }
      def dep2 = Dep("2" + self.dep1.name)
      def dep3 = Dep("3" + self.dep1.name)
    })
    val d = Overridable.build(b)
    d.dep2; d.dep3; d.dep1
    assertEquals(computed.get, 1)
  }

  test("a member is computed once under 1000 virtual threads") {
    for _ <- 1 to 5 do {
      val inits = new AtomicInteger
      val d = build(Override[Deps](new {
        lazy val dep2 = { inits.incrementAndGet(); Thread.sleep(5); Dep("d2(" + self.dep1.name + ")") }
        lazy val dep1 = { inits.incrementAndGet(); Thread.sleep(5); Dep("d1") }
      }))
      val start = new CountDownLatch(1)
      val results = withVirtualExecutor { ex =>
        val fs = (1 to 1000).map(_ => ex.submit(() => { start.await(); (d.dep2, d.dep1) }))
        start.countDown()
        fs.map(_.get)
      }
      assertEquals(inits.get, 2)
      assertEquals(results.distinct, Seq((Dep("d2(d1)"), Dep("d1"))))
    }
  }

  test("a member that threw is retried by the next caller") {
    val attempts = new AtomicInteger
    val d = build(Override[Deps](new {
      def dep2 = { if attempts.incrementAndGet() == 1 then throw new RuntimeException("boom"); Dep("ok") }
    }))
    val e = intercept[RuntimeException](d.dep2)
    assertEquals(e.getMessage, "boom")
    assertEquals(d.dep2, Dep("ok"))
    assertEquals(d.dep2, Dep("ok"))
    assertEquals(attempts.get, 2)
  }

  // --- Late binding and upper ---

  test("a base alone gives its own values") {
    val d = Overridable.build(base)
    assertEquals((d.dep1, d.dep2, d.dep3), (Dep("base1"), Dep("base2"), Dep("base3(base2)")))
  }

  test("a base member sees an overridden member through self (late binding)") {
    val d = build(Override[Deps](new { def dep2 = Dep("over2") }))
    assertEquals(d.dep3, Dep("base3(over2)"))
  }

  test("late binding reaches through a later layer of a chain") {
    val d = build(Override[Deps](new { def dep2 = Dep("o2") }), Override[Deps](new { def dep2 = Dep("o2b") }))
    assertEquals(d.dep3, Dep("base3(o2b)"))
  }

  test("upper is the layer below") {
    val d = build(Override[Deps](new { def dep2 = Dep("w(" + upper.dep2.name + ")") }))
    assertEquals(d.dep2, Dep("w(base2)"))
  }

  test("upper chains through several layers") {
    val wrap = Override[Deps](new { def dep2 = Dep("w(" + upper.dep2.name + ")") })
    assertEquals(build(wrap, wrap, wrap).dep2, Dep("w(w(w(base2)))"))
  }

  test("a member not overridden is forwarded to upper") {
    val d = build(Override[Deps](new { def dep1 = Dep("o1") }), Override[Deps](new { def dep2 = Dep("o2") }))
    assertEquals((d.dep1, d.dep2, d.dep3), (Dep("o1"), Dep("o2"), Dep("base3(o2)")))
  }

  test("an override may use self.x for another member it overrides") {
    val d = build(Override[Deps](new {
      def dep1 = Dep("o1")
      def dep2 = Dep("o2 sees " + self.dep1.name)
    }))
    assertEquals(d.dep2, Dep("o2 sees o1"))
  }

  // --- Monoid and |+| ---

  private val oa = Override[Deps](new { def dep1 = Dep("a1"); def dep2 = Dep("a2(" + upper.dep2.name + ")") })
  private val ob = Override[Deps](new { def dep2 = Dep("b2(" + upper.dep2.name + ")"); def dep3 = Dep("b3(" + upper.dep3.name + ")") })
  private val oc = Override[Deps](new { def dep1 = Dep("c1(" + upper.dep1.name + ")"); def dep2 = Dep("c2(" + self.dep1.name + ")") })
  private val M = summon[Monoid[Override[Deps]]]

  private def observe(d: Deps) = (d.dep1, d.dep2, d.dep3)

  test("|+| is associative") {
    val left = Overridable.build(base, (oa |+| ob) |+| oc)
    val right = Overridable.build(base, oa |+| (ob |+| oc))
    assertEquals(observe(left), observe(right))
  }

  test("empty is an identity on both sides") {
    val plain = observe(Overridable.build(base, oa))
    assertEquals(observe(Overridable.build(base, M.empty |+| oa)), plain)
    assertEquals(observe(Overridable.build(base, oa |+| M.empty)), plain)
    assertEquals(observe(Overridable.build(base, M.empty)), observe(Overridable.build(base)))
  }

  test("x |+| y is y extending x") {
    val d = Overridable.build(base, oa |+| ob)
    assertEquals(d.dep2, Dep("b2(a2(base2))"))
  }

  test("flat, layered and composed builds agree") {
    val flat = observe(Overridable.build(base, oa, ob, oc))
    assertEquals(observe(Overridable.build(base |+| oa, ob, oc)), flat)
    assertEquals(observe(Overridable.build(base |+| oa |+| ob, oc)), flat)
    assertEquals(observe(Overridable.build(base, oa |+| ob, oc)), flat)
    assertEquals(observe(Overridable.build(base, oa, ob |+| oc)), flat)
    assertEquals(observe(Overridable.build(base, M.combineAll(List(oa, ob, oc)))), flat)
  }

  test("a base layered with an override stays a base") {
    val b: Overridable[Deps] = base |+| oa
    assertEquals(Overridable.build(b).dep1, Dep("a1"))
  }

  test("a base given as an override replaces everything below") {
    val other = Overridable[Deps](new { def dep1 = Dep("n1"); def dep2 = Dep("n2"); def dep3 = Dep("n3") })
    val d = Overridable.build(base, oa, other)
    assertEquals(observe(d), (Dep("n1"), Dep("n2"), Dep("n3")))
  }

  test("a base replacing a lower layer still lets later overrides win") {
    val other = Overridable[Deps](new { def dep1 = Dep("n1"); def dep2 = Dep("n2"); def dep3 = Dep("n3(" + self.dep2.name + ")") })
    val d = Overridable.build(base, other, ob)
    assertEquals(observe(d), (Dep("n1"), Dep("b2(n2)"), Dep("b3(n3(b2(n2)))")))
  }

  // --- Inputs ---

  test("a named class supplies the members it declares") {
    val d = build(Override[Deps](new WrapsDep2))
    assertEquals(d.dep2, Dep("wrapped(base2)"))
    assertEquals(d.dep3, Dep("base3(wrapped(base2))"))
  }

  test("a case class held in a variable supplies its fields") {
    val held = OnlyDep1(Dep("held"))
    val d = build(Override[Deps](held))
    assertEquals(d.dep1, Dep("held"))
    assertEquals(d.dep2, Dep("base2"))
  }

  test("a named class can be a complete base") {
    val d = Overridable.build(Overridable[Deps](new FullBase))
    assertEquals(observe(d), (Dep("n1"), Dep("n2"), Dep("n3")))
  }

  test("a complete instance in a variable is used as is") {
    val existing: Deps = Overridable.build(base)
    val d = build(Override[Deps](existing))
    assertEquals(observe(d), observe(existing))
  }

  test("a complete instance written inline is used as is") {
    val d = build(Override[Deps](new Deps {
      def dep1 = Dep("i1")
      def dep2 = Dep("i2")
      def dep3 = Dep("i3")
    }))
    assertEquals(observe(d), (Dep("i1"), Dep("i2"), Dep("i3")))
  }

  test("a complete instance can be a base") {
    val inst: Deps = new FullBase with Deps
    assertEquals(observe(Overridable.build(Overridable[Deps](inst))), (Dep("n1"), Dep("n2"), Dep("n3")))
  }

  test("an object literal may keep private helpers") {
    val d = build(Override[Deps](new {
      private val helper = "h"
      def dep2 = Dep(helper + upper.dep2.name + self.dep1.name)
    }))
    assertEquals(d.dep2, Dep("hbase2base1"))
  }

  test("the built value is a plain instance of the trait, not a proxy") {
    assert(!java.lang.reflect.Proxy.isProxyClass(build(oa).getClass))
  }

  test("members can be methods with parameters") {
    trait Calc { def add(x: Int, y: Int): Int; def twice(x: Int): Int }
    val b = Overridable[Calc](new { def add(x: Int, y: Int) = x + y; def twice(x: Int) = self.add(x, x) })
    val d = Overridable.build(b, Override[Calc](new { def add(x: Int, y: Int) = x * y }))
    assertEquals(d.twice(5), 25)
  }
}
