package spike.d

import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger

class ViewSuite extends munit.FunSuite {
  import ViewSuite.*

  test("a View target builds and reads fields") {
    val d = Overridable.build(Overridable[Fields]((a = 1, b = "x", c = self.a + 1)))
    assertEquals((d.a, d.b, d.c), (1, "x", 2))
  }

  test("toTuple gives the plain named tuple") {
    val d = Overridable.build(Overridable[Fields]((a = 1, b = "x", c = self.a + 1)))
    val t: (a: Int, b: String, c: Int) = d.toTuple
    assertEquals(t, (a = 1, b = "x", c = 2))
  }

  test("View fields are lazy and computed once") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Fields]((a = { n.incrementAndGet(); 1 }, b = sys.error("never"), c = self.a)))
    assertEquals(n.get, 0)
    assertEquals((d.c, d.c, d.a), (1, 1, 1))
    assertEquals(n.get, 1)
  }

  test("override with upper") {
    val base = Overridable[Fields]((a = 1, b = "x", c = self.a + 1))
    val d = Overridable.build(base, Override[Fields]((a = upper.a + 10)), Override[Fields](new { def b = upper.b + "!" }))
    assertEquals(d.toTuple, (a = 11, b = "x!", c = 12))
  }

  test("a named-tuple value as input") {
    val t: (a: Int, b: String, c: Int) = (a = 1, b = "x", c = 3)
    assertEquals(Overridable.build(Overridable[Fields](t)).toTuple, t)
  }

  test("++ as input") {
    val d = Overridable.build(Overridable[Fields]((a = 1) ++ (b = "x", c = 3)))
    assertEquals(d.toTuple, (a = 1, b = "x", c = 3))
  }

  test("an object literal as input") {
    val d = Overridable.build(Overridable[Fields](new { def a = 1; def b = "x"; def c = self.a + 2 }))
    assertEquals(d.toTuple, (a = 1, b = "x", c = 3))
  }

  test("a replacement base for a View") {
    val base = Overridable[Fields]((a = 1, b = "x", c = 3))
    val d = Overridable.build(base, Override[Fields]((a = 2)), Overridable[Fields]((a = 9, b = "y", c = self.a)))
    assertEquals(d.toTuple, (a = 9, b = "y", c = 9))
  }

  test("a cycle through View fields") {
    val d = Overridable.build(Overridable[Fields]((a = self.c, b = "x", c = self.a)))
    intercept[CycleException](d.a)
  }

  test("an unknown name fails in selectDynamic") {
    val d = Overridable.build(Overridable[Fields]((a = 1, b = "x", c = 3)))
    intercept[NoSuchElementException](d.selectDynamic("zzz"))
  }

  test("a View with more than 22 fields") {
    val d = Overridable.build(Overridable[BigView]((
      f1 = 1, f2 = self.f1 + 1, f3 = 3, f4 = 4, f5 = 5, f6 = 6, f7 = 7, f8 = 8, f9 = 9, f10 = 10, f11 = 11, f12 = 12,
      f13 = 13, f14 = 14, f15 = 15, f16 = 16, f17 = 17, f18 = 18, f19 = 19, f20 = 20, f21 = 21, f22 = 22, f23 = 23,
      f24 = self.f2 + 1,
    )))
    assertEquals((d.f24, d.f23), (3, 23))
    assertEquals(d.toTuple.f24, 3)
  }

  test("cats syntax on layers of a View") {
    val o = Override[Fields]((a = upper.a + 1)) |+| Override[Fields]((a = upper.a * 2))
    assertEquals(Overridable.build(Overridable[Fields]((a = 1, b = "x", c = 0)), o).a, 4)
  }
}

object ViewSuite {
  type Fields = View[(a: Int, b: String, c: Int)]
  type BigView = View[(
    f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int, f11: Int, f12: Int,
    f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int, f21: Int, f22: Int, f23: Int, f24: Int,
  )]
}
