package spike.e

import cats.Monoid
import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger

import Fx.*

object BehaviorFx {
  trait Deps { def dep1: D; def dep2: D; def dep3: D }

  type DepsView = View[(dep1: D, dep2: D, dep3: D)]

  trait Optional[F[_]] { def x: F[Int]; def y: F[Int] }
  trait Cov[+T] { def t: T; def ts: List[T] }
  trait Contra[-T] { def f: T => Int }
  class Outer { trait Inner { def x: Int; def y: Int } }

  trait Unitful { def init: Unit; def boom: Nothing; def x: Int }
  trait Base0 { def a: D }
  trait AB2 extends Base0 { def b: D }
  trait Tuples { def a: (x: Int, y: Int); def b: (Int, Int); def c: Int }

  // Names of the members clash with locals the macro generates.
  trait Hygiene { def value: Int; def instance: Int; def path: Int; def cells: Int; def ctx: Int; def root: Int; def get: Int; def s: Int; def name: Int }
  class HygieneImpl(using Self[Hygiene]) {
    def value = 1; def instance = self.value + 1; def path = 3; def cells = 4; def ctx = 5; def root = 6; def get = 7; def s = 8; def name = 9
  }
  type HygieneView = View[(value: Int, instance: Int, path: Int, cells: Int, ctx: Int, name: Int, Fields: Int, toTuple: Int)]

  trait Big {
    def f1: Int; def f2: Int; def f3: Int; def f4: Int; def f5: Int; def f6: Int; def f7: Int; def f8: Int
    def f9: Int; def f10: Int; def f11: Int; def f12: Int; def f13: Int; def f14: Int; def f15: Int; def f16: Int
    def f17: Int; def f18: Int; def f19: Int; def f20: Int; def f21: Int; def f22: Int; def f23: Int
  }
}

class BehaviorSuite extends munit.FunSuite {
  import BehaviorFx.*

  // --- laziness and compute-once

  test("building computes nothing") {
    val n = new AtomicInteger
    Overridable.build(Overridable[AB]((a = { n.incrementAndGet(); D("a") }, b = { n.incrementAndGet(); D("b") })))
    assertEquals(n.get, 0)
  }

  test("reading a field computes only it and what it needs") {
    val log = Vector.newBuilder[String]
    val r = Overridable.build(Overridable[ABC]((
      a = { log += "a"; D("a") },
      b = { log += "b"; D("b" + self.a.n) },
      c = { log += "c"; D("c") },
    )))
    assertEquals(r.b, D("ba"))
    assertEquals(log.result(), Vector("b", "a"))
  }

  test("a field is computed once however often it is read") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[AB]((a = { n.incrementAndGet(); D("a") }, b = D("b" + self.a.n + self.a.n))))
    r.a; r.a; r.b; r.b
    assertEquals(n.get, 1)
  }

  test("a layer's field is computed once even when upper is read by several fields") {
    val n = new AtomicInteger
    val base = Overridable[AB]((a = { n.incrementAndGet(); D("a") }, b = D("b")))
    val o = Override[AB]((a = D("o" + upper.a.n), b = D("o" + upper.a.n)))
    val r = Overridable.build(base, o)
    assertEquals((r.a, r.b), (D("oa"), D("oa")))
    assertEquals(n.get, 1)
  }

  // --- late binding and upper

  test("a base field sees the overridden field (late binding)") {
    val base = Overridable[AB]((a = D("a"), b = D("b:" + self.a.n)))
    assertEquals(Overridable.build(base).b, D("b:a"))
    assertEquals(Overridable.build(base, Override[AB]((a = D("A")))).b, D("b:A"))
  }

  test("upper reads the layer below") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val o = Override[AB]((a = D("wrapped(" + upper.a.n + ")")))
    assertEquals(Overridable.build(base, o).a, D("wrapped(a)"))
    assertEquals(Overridable.build(base, o).b, D("b"))
  }

  test("fields an override does not define come from upper") {
    val base = Overridable[ABC]((a = D("a"), b = D("b"), c = D("c")))
    val r = Overridable.build(base, Override[ABC]((b = D("B"))))
    assertEquals((r.a, r.b, r.c), (D("a"), D("B"), D("c")))
  }

  test("flat, layered and composed overrides agree") {
    val base = Overridable[AB]((a = D("a"), b = D("b" + self.a.n)))
    val o1 = Override[AB]((a = D("1" + upper.a.n)))
    val o2 = Override[AB](new { def a = D("2" + upper.a.n) })
    val expected = D("b21a")
    assertEquals(Overridable.build(base, o1, o2).b, expected)
    assertEquals(Overridable.build(base |+| o1, o2).b, expected)
    assertEquals(Overridable.build(base, o1 |+| o2).b, expected)
    assertEquals(Overridable.build(base |+| o1 |+| o2).b, expected)
  }

  test("empty is the identity of |+|") {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val o = Override[AB]((a = D("o" + upper.a.n)))
    val e = Monoid[Override[AB]].empty
    val results = List(
      Overridable.build(base, o), Overridable.build(base, e |+| o), Overridable.build(base, o |+| e), Overridable.build(base |+| e, o),
    ).map(_.b)
    assertEquals(results.distinct, List(D("oa")))
  }

  test("|+| is associative") {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val o = Override[AB]((a = D("o" + upper.a.n)))
    val results = List(
      Overridable.build(base, (o |+| o) |+| o), Overridable.build(base, o |+| (o |+| o)), Overridable.build(base, o, o, o),
    ).map(_.b)
    assertEquals(results.distinct, List(D("oooa")))
  }

  test("a base used as an override replaces what is below it") {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val base2 = Overridable[AB]((a = D("A2"), b = D("B2")))
    assertEquals(Overridable.build(base, base2).b, D("B2"))
    assertEquals(Overridable.build(base |+| base2).a, D("A2"))
  }

  test("a chain of 200 overrides") {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val o = Override[AB]((b = upper.b))
    assertEquals(Overridable.build(base, List.fill(200)(o)*).b, D("a"))
  }

  test("a target that inherits abstract members") {
    val r = Overridable.build(Overridable[AB2]((a = D("a"), b = D("b:" + self.a.n))), Override[AB2]((a = D("A"))))
    assertEquals((r.a, r.b), (D("A"), D("b:A")))
  }

  test("Unit and Nothing members") {
    var inits = 0
    val r = Overridable.build(Overridable[Unitful]((init = { inits += 1 }, boom = throw new RuntimeException("boom!"), x = { self.init; self.init; 1 })))
    assertEquals(r.x, 1)
    assertEquals(inits, 1)
    assertEquals(intercept[RuntimeException](r.boom).getMessage, "boom!")
  }

  test("members named like the generated locals, and user locals with the same names") {
    val value = 100; val instance = 200; val s = 300; val slot = 400; val ctx = 500
    val r = Overridable.build(Overridable[Hygiene]((value = value, instance = instance + self.value, path = s, cells = slot, ctx = ctx, root = 1, get = 2, s = 3, name = 4)))
    assertEquals((r.value, r.instance, r.path, r.cells, r.ctx, r.root, r.get, r.s, r.name), (100, 300, 300, 400, 500, 1, 2, 3, 4))
  }

  test("a class using the context under hygiene-prone names") {
    val r = Overridable.build(Overridable[Hygiene](new HygieneImpl))
    assertEquals((r.value, r.instance), (1, 2))
  }

  test("local names self and upper can be shadowed") {
    val r = Overridable.build(
      Overridable[AB2]((a = D("a"), b = { val self = 41; D((self + 1).toString) })),
      Override[AB2]((a = { def upper = 7; D(upper.toString) })),
    )
    assertEquals((r.a, r.b), (D("7"), D("42")))
  }

  test("fields of (named) tuple type") {
    val r = Overridable.build(Overridable[Tuples]((a = (x = 1, y = 2), b = (3, 4), c = self.a.x + self.b._2)))
    assertEquals((r.a.toTuple, r.b, r.c), ((1, 2), (3, 4), 5))
  }

  test("a literal of arity 3 with named-tuple fields evaluates each field once") {
    val k = new AtomicInteger
    val r = Overridable.build(Overridable[Tuples]((a = { k.incrementAndGet(); (x = 1, y = 2) }, b = (3, 4), c = 4)))
    r.a; r.b; r.c; r.a
    assertEquals(k.get, 1)
  }

  test("23 fields in a literal; reading one computes only it and its needs") {
    val n = new AtomicInteger
    def tick(i: Int): Int = { n.incrementAndGet(); i }
    val r = Overridable.build(Overridable[Big]((
      f1 = tick(1), f2 = tick(self.f1), f3 = tick(3), f4 = tick(4), f5 = tick(5), f6 = tick(6), f7 = tick(7), f8 = tick(8),
      f9 = tick(9), f10 = tick(10), f11 = tick(11), f12 = tick(12), f13 = tick(13), f14 = tick(14), f15 = tick(15), f16 = tick(16),
      f17 = tick(17), f18 = tick(18), f19 = tick(19), f20 = tick(20), f21 = tick(21), f22 = tick(22), f23 = tick(23),
    )))
    assertEquals(r.f1, 1)
    assertEquals(n.get, 1)
    assertEquals(r.f2, 1)
    assertEquals(n.get, 2)
  }

  test("23 fields supplied by a named-tuple variable") {
    val t = (f1 = 1, f2 = 2, f3 = 3, f4 = 4, f5 = 5, f6 = 6, f7 = 7, f8 = 8, f9 = 9, f10 = 10, f11 = 11, f12 = 12, f13 = 13, f14 = 14, f15 = 15, f16 = 16,
      f17 = 17, f18 = 18, f19 = 19, f20 = 20, f21 = 21, f22 = 22, f23 = 23)
    val r = Overridable.build(Overridable[Big](t))
    assertEquals((r.f1, r.f23), (1, 23))
  }

  // --- input kinds

  test("input: named-tuple literal") {
    assertEquals(Overridable.build(Overridable[AB]((a = D("a"), b = D("b")))).a, D("a"))
  }

  test("input: object literal with self and upper") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB](new { def a = D("L" + upper.a.n); def b = D("L" + self.a.n) }))
    assertEquals((r.a, r.b), (D("La"), D("LLa")))
  }

  test("input: object literal val is computed once") {
    val n = new AtomicInteger
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB](new { val a = { n.incrementAndGet(); D("v:" + upper.a.n) } }))
    r.a; r.a; r.b
    assertEquals((r.a, n.get), (D("v:a"), 1))
  }

  test("input: object literal lazy val and inline def") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB](new { inline def a = D("inl:" + upper.a.n); lazy val b = D("lz:" + self.a.n) }))
    assertEquals((r.a, r.b), (D("inl:a"), D("lz:inl:a")))
  }

  test("input: named-tuple variable") {
    val base = Overridable[AB]((a = D("a"), b = D("b:" + self.a.n)))
    val t = (a = D("x"))
    assertEquals(Overridable.build(base, Override[AB](t)).b, D("b:x"))
  }

  test("input: named tuples joined with ++") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB]((a = D("x")) ++ (b = D("y"))))
    assertEquals((r.a, r.b), (D("x"), D("y")))
  }

  test("input: case class instance") {
    case class CC(a: D, b: D)
    val base = Overridable[ABC]((a = D("a"), b = D("b"), c = D("c")))
    val r = Overridable.build(base, Override[ABC](CC(D("cca"), D("ccb"))))
    assertEquals((r.a, r.b, r.c), (D("cca"), D("ccb"), D("c")))
  }

  // Finding: a member the class inherits (rather than declares) is silently not used; `a` stays the
  // base's. Expected D("inherited-a") if inherited members count, or a compile error if not.
  test("input: a class's inherited member overrides the field".fail) {
    abstract class Helper { def a: D = D("inherited-a") }
    class Partial extends Helper { def b = D("declared-b") }
    val base = Overridable[ABC]((a = D("a"), b = D("b"), c = D("c")))
    val r = Overridable.build(base, Override[ABC](new Partial))
    assertEquals((r.a, r.b), (D("inherited-a"), D("declared-b")))
  }

  test("input: generic class instance") {
    class Gen[T](t: T) { def a: T = t }
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    assertEquals(Overridable.build(base, Override[AB](new Gen(D("gen")))).a, D("gen"))
  }

  test("input: an empty tuple-free override of nothing is rejected, an empty layer is Monoid.empty") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    assertEquals(Overridable.build(base, Monoid[Override[AB]].empty).a, D("a"))
  }

  test("input: class using the context is constructed once per field computed, and sees self and upper") {
    val ctor = new AtomicInteger
    class UsesCtx(using Upper[ABC]) {
      ctor.incrementAndGet()
      def a = D("ua:" + upper.a.n); def b = D("ub:" + upper.b.n); def c = D("uc:" + self.a.n)
    }
    val base = Overridable[ABC]((a = D("a"), b = D("b"), c = D("c")))
    val r = Overridable.build(base, Override[ABC](new UsesCtx))
    assertEquals((r.a, r.b, r.c), (D("ua:a"), D("ub:b"), D("uc:ua:a")))
    r.a; r.b; r.c
    assertEquals(ctor.get, 3)
  }

  test("input: context-free value is evaluated once for all its fields") {
    val created = new AtomicInteger
    def makeTuple() = { created.incrementAndGet(); (a = D("t1"), b = D("t2")) }
    val base = Overridable[ABC]((a = D("a"), b = D("b"), c = D("c")))
    val r = Overridable.build(base, Override[ABC](makeTuple()))
    r.a; r.b; r.a
    assertEquals((r.a, r.b, r.c), (D("t1"), D("t2"), D("c")))
    assertEquals(created.get, 1)
  }

  test("input: a class whose lambda argument needs no context is built once") {
    class TwoPlain(f: String => String) { def a = D(f("1")); def b = D(f("2")) }
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB](new TwoPlain(s => s"[$s]")))
    assertEquals((r.a, r.b), (D("[1]"), D("[2]")))
  }

  test("input: instance of the target is used as is (a complete instance)") {
    val base = Overridable[AB](new AB { def a = D("a"); def b = D("b") })
    assertEquals(Overridable.build(base).b, D("b"))
  }

  test("input: complete instance as an override replaces the layers below") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val full: AB = new AB { def a = D("full-a"); def b = D("full-b") }
    val r = Overridable.build(base, Override[AB](full))
    assertEquals((r.a, r.b), (D("full-a"), D("full-b")))
  }

  test("input: members forwarded from a named class: varargs, by-name, polymorphic, using") {
    trait V { def sum(xs: Int*): Int; def twice(x: => Int): Int; def id[T](t: T): T; def ctx(using n: Int): Int; def g: Int }
    class Impl { def sum(xs: Int*) = xs.sum; def twice(x: => Int) = x + x; def id[T](t: T) = t; def ctx(using n: Int) = n * 2; def g = 0 }
    val r = Overridable.build(Overridable[V](new Impl), Override[V]((g = 2)))
    var n = 0
    given Int = 21
    assertEquals(r.sum(1, 2, 3), 6)
    assertEquals(r.twice({ n += 1; n }), 3)
    assertEquals(r.id("s"), "s")
    assertEquals(r.ctx, 42)
    assertEquals(r.g, 2)
  }

  test("input: Java interface target via a named class") {
    var ran = 0
    class R { def run(): Unit = ran += 1 }
    val r = Overridable.build(Overridable[Runnable](new R))
    r.run(); r.run()
    assertEquals(ran, 2)
  }

  // --- View targets

  test("view: build, read, toTuple") {
    val base = Overridable[DepsView]((dep1 = D("1"), dep2 = D("2"), dep3 = D("3:" + self.dep2.n)))
    val r = Overridable.build(base, Override[DepsView]((dep2 = D("o" + upper.dep2.n))))
    assertEquals(r.dep3, D("3:o2"))
    assertEquals(r.toTuple, (dep1 = D("1"), dep2 = D("o2"), dep3 = D("3:o2")))
  }

  test("view: layers via object literal and a named class") {
    class Wrap(using Upper[DepsView]) { lazy val dep2 = D("class(" + upper.dep2.n + ")") }
    val base = Overridable[DepsView]((dep1 = D("1"), dep2 = D("2"), dep3 = D("3")))
    assertEquals(Overridable.build(base, Override[DepsView](new { def dep2 = D("lit(" + upper.dep2.n + ")") })).dep2, D("lit(2)"))
    assertEquals(Overridable.build(base, Override[DepsView](new Wrap)).dep2, D("class(2)"))
  }

  test("view: named-tuple variable and an existing instance") {
    val base = Overridable[DepsView]((dep1 = D("1"), dep2 = D("2"), dep3 = D("3")))
    val t = (dep2 = D("var"))
    assertEquals(Overridable.build(base, Override[DepsView](t)).dep2, D("var"))
    val existing: DepsView = Overridable.build(base)
    assertEquals(Overridable.build(base, Override[DepsView](existing)).dep1, D("1"))
  }

  test("view: selectDynamic of an unknown name fails at runtime") {
    val r = Overridable.build(Overridable[View[(a: Int, b: Int)]]((a = 1, b = 2)))
    intercept[NoSuchElementException](r.selectDynamic("zzz"))
  }

  test("view: fields named after the generated members of the macro") {
    val r = Overridable.build(Overridable[HygieneView]((value = 1, instance = 2, path = 3, cells = 4, ctx = 5, name = 6, Fields = 7, toTuple = 8)))
    assertEquals((r.value, r.name), (1, 6))
    assertEquals(r.toTuple, (value = 1, instance = 2, path = 3, cells = 4, ctx = 5, name = 6, Fields = 7, toTuple = 8))
  }

  // --- parameterized traits

  def optBase[F[_]](pure: Int => F[Int], map: (F[Int], Int => Int) => F[Int]): Overridable[Optional[F]] =
    Overridable[Optional[F]]((x = pure(1), y = map(self.x, _ + 1)))
  def optOver[F[_]](map: (F[Int], Int => Int) => F[Int]): Override[Optional[F]] =
    Override[Optional[F]]((x = map(upper.x, _ * 10)))

  test("trait with an abstract type constructor, instantiated with Option and List") {
    assertEquals(Overridable.build(optBase[Option](Some(_), _.map(_)), optOver[Option](_.map(_))).y, Some(11))
    assertEquals(Overridable.build(optBase[List](List(_), _.map(_)), optOver[List](_.map(_))).y, List(11))
  }

  test("trait with a concrete type constructor") {
    trait Deps[F[_]] { def depA: F[D]; def depB: F[D] }
    val base = Overridable[Deps[Option]]((depA = Some(D("a")), depB = self.depA.map(d => D(d.n + "b"))))
    assertEquals(Overridable.build(base).depB, Some(D("ab")))
  }

  test("covariant and contravariant traits") {
    val c: Cov[Any] = Overridable.build(Overridable[Cov[String]]((t = "s", ts = List(self.t))))
    assertEquals(c.ts, List("s"))
    assertEquals(Overridable.build(Overridable[Contra[String]]((f = (s: String) => s.length))).f("abc"), 3)
  }

  test("path-dependent and local traits") {
    val o = new Outer
    assertEquals(Overridable.build(Overridable[o.Inner]((x = 1, y = self.x + 1))).y, 2)
    trait L { def a: Int; def b: Int }
    assertEquals(Overridable.build(Overridable[L]((a = 1, b = self.a + 1)), Override[L]((a = 5))).b, 6)
  }
}
