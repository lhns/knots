package spike.d

import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer

class OverridableSuite extends munit.FunSuite {
  import OverridableSuite.*

  test("a base defines every member") {
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("members are computed once") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Deps]((a = { n.incrementAndGet(); 1 }, b = self.a + 1, c = self.a + self.b)))
    assertEquals(n.get, 0)
    assertEquals((d.c, d.c, d.b, d.a), (3, 3, 2, 1))
    assertEquals(n.get, 1)
  }

  test("members are lazy: unused ones are never computed") {
    val log = ListBuffer.empty[String]
    val d = Overridable.build(Overridable[Deps]((
      a = { log += "a"; 1 },
      b = { log += "b"; 2 },
      c = { log += "c"; self.a },
    )))
    assertEquals(d.c, 1)
    assertEquals(log.toList, List("c", "a"))
  }

  test("a literal with more than 22 fields is lazy and computed once") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Big]((
      f1 = { n.incrementAndGet(); 1 }, f2 = self.f1 + 1, f3 = 3, f4 = 4, f5 = 5, f6 = 6, f7 = 7, f8 = 8, f9 = 9, f10 = 10,
      f11 = 11, f12 = 12, f13 = 13, f14 = 14, f15 = 15, f16 = 16, f17 = 17, f18 = 18, f19 = 19, f20 = 20,
      f21 = 21, f22 = 22, f23 = sys.error("never computed"), f24 = self.f2 + self.f1,
    )))
    assertEquals(n.get, 0)
    assertEquals((d.f24, d.f24, d.f2, d.f1), (3, 3, 2, 1))
    assertEquals(n.get, 1)
  }

  test("late binding: an override changes what the base sees") {
    val base = Overridable[Deps]((a = 1, b = self.a * 10, c = 0))
    val d = Overridable.build(base, Override[Deps]((a = 5)))
    assertEquals((d.a, d.b), (5, 50))
  }

  test("upper is the member of the layer below") {
    val base = Overridable[Deps]((a = 1, b = 2, c = 3))
    val d = Overridable.build(base, Override[Deps]((a = upper.a + 10)), Override[Deps]((a = upper.a * 2)))
    assertEquals(d.a, 22)
    assertEquals((d.b, d.c), (2, 3))
  }

  test("members an override does not supply come from upper") {
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps]((b = 20)))
    assertEquals((d.a, d.b, d.c), (1, 20, 3))
  }

  test("an override computes its member once, and lazily") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps]((b = { n.incrementAndGet(); upper.b + 1 })))
    assertEquals(n.get, 0)
    assertEquals((d.b, d.b), (3, 3))
    assertEquals(n.get, 1)
  }

  test("|+| on overrides: the right one extends the left one") {
    val o = Override[Deps]((a = upper.a + 1)) |+| Override[Deps]((a = upper.a * 10))
    assertEquals(Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), o).a, 20)
  }

  test("|+| on a base and an override gives a base") {
    val base: Overridable[Deps] = Overridable[Deps]((a = 1, b = 2, c = 3)) |+| Override[Deps]((a = 7))
    assertEquals(Overridable.build(base).a, 7)
    assertEquals(Overridable.build(base, Override[Deps]((a = upper.a + 1))).a, 8)
  }

  test("layers combine in order however they are grouped") {
    val base = Overridable[Deps]((a = 1, b = 2, c = 3))
    val o1 = Override[Deps]((a = upper.a + 1))
    val o2 = Override[Deps]((a = upper.a * 10))
    assertEquals(Overridable.build(base, o1, o2).a, 20)
    assertEquals(Overridable.build(base |+| o1, o2).a, 20)
    assertEquals(Overridable.build(base, o1 |+| o2).a, 20)
  }

  test("Override monoid: empty passes upper through") {
    val base = Overridable[Deps]((a = 1, b = 2, c = 3))
    assertEquals(Overridable.build(base, Override.monoid[Deps].empty).a, 1)
    assertEquals(Overridable.build(base).a, 1)
  }

  test("a base as an override replaces the layers below completely") {
    val base = Overridable[Deps]((a = 1, b = 2, c = 3))
    val replacement = Overridable[Deps]((a = 100, b = 200, c = self.a + self.b))
    val d = Overridable.build(base, Override[Deps]((a = 5)), replacement)
    assertEquals((d.a, d.b, d.c), (100, 200, 300))
  }

  test("a replacement base still sees later overrides through self") {
    val base = Overridable[Deps]((a = 1, b = 2, c = 3))
    val replacement = Overridable[Deps]((a = 100, b = 200, c = self.a + self.b))
    val d = Overridable.build(base, replacement, Override[Deps]((a = 1)))
    assertEquals(d.c, 201)
  }

  test("one layer built twice gives independent results") {
    val n = new AtomicInteger
    val base = Overridable[Deps]((a = n.incrementAndGet(), b = 0, c = 0))
    assertEquals((Overridable.build(base).a, Overridable.build(base).a), (1, 2))
  }

  // --- the input kinds ---

  test("input: object literal") {
    val d = Overridable.build(Overridable[Deps](new { def a = 1; def b = self.a + 1; lazy val c = self.b + 1 }))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: object literal with a private helper") {
    val d = Overridable.build(Overridable[Deps](new {
      private def helper = 5
      def a = helper
      def b = 2
      def c = 3
    }))
    assertEquals(d.a, 5)
  }

  test("input: object literal members are computed once") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Deps](new { def a = { n.incrementAndGet(); 1 }; def b = 2; def c = 3 }))
    assertEquals((d.a, d.a), (1, 1))
    assertEquals(n.get, 1)
  }

  test("input: object literal override with upper") {
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps](new { def a = upper.a + 41 }))
    assertEquals((d.a, d.b), (42, 2))
  }

  test("input: class instance") {
    val d = Overridable.build(Overridable[Deps](new ABC(1, 2, 3)))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: class instance as an override, only its own members") {
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps](new OnlyA(9)))
    assertEquals((d.a, d.b, d.c), (9, 2, 3))
  }

  test("input: named-tuple value") {
    val t: (a: Int, b: Int, c: Int) = (a = 1, b = 2, c = 3)
    val d = Overridable.build(Overridable[Deps](t))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: named-tuple value as an override") {
    val t = (b = 20)
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps](t))
    assertEquals((d.a, d.b, d.c), (1, 20, 3))
  }

  test("input: ++ of named tuples") {
    val d = Overridable.build(Overridable[Deps]((a = 1) ++ (b = 2, c = 3)))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: ++ with a tuple variable") {
    val rest = (b = 2, c = 3)
    val d = Overridable.build(Overridable[Deps]((a = 1) ++ rest))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: a complete instance is used as is") {
    val inst: Deps = new Deps { def a = 1; def b = 2; def c = 3 }
    val d = Overridable.build(Overridable[Deps](inst))
    assertEquals((d.a, d.b, d.c), (1, 2, 3))
  }

  test("input: a complete instance as an override replaces everything") {
    val inst: Deps = new Deps { def a = 7; def b = 8; def c = 9 }
    val d = Overridable.build(Overridable[Deps]((a = 1, b = 2, c = 3)), Override[Deps](inst))
    assertEquals((d.a, d.b, d.c), (7, 8, 9))
  }

  test("input: complete anonymous instance literal") {
    val d = Overridable.build(Overridable[Deps](new Deps { def a = 1; def b = 2; def c = 3 }))
    assertEquals(d.c, 3)
  }

  // --- trait shapes ---

  test("parameterized trait Deps[F[_]]") {
    val d = Overridable.build(Overridable[HK[Option]]((x = Some(1), y = self.x.map(_ + 1))))
    assertEquals((d.x, d.y), (Some(1), Some(2)))
    val l = Overridable.build(Overridable[HK[List]]((x = List(1, 2), y = self.x.map(_ + 1))), Override[HK[List]]((x = upper.x :+ 3)))
    assertEquals((l.x, l.y), (List(1, 2, 3), List(2, 3, 4)))
  }

  test("members with parameters are forwarded, not cached") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[WithMethod]((a = 2, f = (x: Int) => { n.incrementAndGet(); x + 1 })))
    assertEquals((d.f(1), d.f(1)), (2, 2))
    assertEquals(n.get, 2)
  }

  test("inherited abstract members are included") {
    val d = Overridable.build(Overridable[Child]((a = 1, b = 2, c = self.a + self.b, d = 4)))
    assertEquals(d.c, 3)
  }
}

object OverridableSuite {
  trait Deps { def a: Int; def b: Int; def c: Int }
  class ABC(val a: Int, val b: Int, val c: Int)
  class OnlyA(val a: Int)
  trait HK[F[_]] { def x: F[Int]; def y: F[Int] }
  trait WithMethod { def a: Int; def f: Int => Int }
  trait Parent { def a: Int; def b: Int }
  trait Child extends Parent { def c: Int; def d: Int }

  trait Big {
    def f1: Int; def f2: Int; def f3: Int; def f4: Int; def f5: Int; def f6: Int; def f7: Int; def f8: Int
    def f9: Int; def f10: Int; def f11: Int; def f12: Int; def f13: Int; def f14: Int; def f15: Int; def f16: Int
    def f17: Int; def f18: Int; def f19: Int; def f20: Int; def f21: Int; def f22: Int; def f23: Int; def f24: Int
  }
}
