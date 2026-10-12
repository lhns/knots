package spike.a

import java.util.concurrent.atomic.AtomicInteger

class LazinessSuite extends munit.FunSuite {
  test("build computes nothing") {
    val effects = AtomicInteger()
    Overridable.build(Overridable[XY]((x = { effects.incrementAndGet(); 1 }, y = self.x + 1)))
    assertEquals(effects.get, 0)
  }

  test("a field is computed once, on access") {
    val effects = AtomicInteger()
    val v = Overridable.build(Overridable[XY]((x = { effects.incrementAndGet(); 1 }, y = self.x + 1)))
    assertEquals(v.y, 2)
    assertEquals(v.x, 1)
    assertEquals(v.y, 2)
    assertEquals(effects.get, 1)
  }

  test("an overridden base field is never computed") {
    val effects = AtomicInteger()
    val v = Overridable.build(
      Overridable[XY]((x = { effects.incrementAndGet(); 1 }, y = 2)),
      Override[XY]((x = 5)))
    assertEquals(v.x, 5)
    assertEquals(effects.get, 0)
  }

  test("reading one field of an override computes only that field") {
    val effects = AtomicInteger()
    val v = Overridable.build(
      Overridable[XY]((x = 1, y = 2)),
      Override[XY]((x = { effects.incrementAndGet(); 10 }, y = 20)))
    assertEquals(v.y, 20)
    assertEquals(effects.get, 0)
    assertEquals((v.x, v.x), (10, 10))
    assertEquals(effects.get, 1)
  }

  test("each field of an override has its own side effect, run once") {
    val log = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val v = Overridable.build(
      Overridable[XY]((x = 1, y = 2)),
      Override[XY]((x = { log.add("x"); 10 }, y = { log.add("y"); 20 })))
    assertEquals((v.y, v.x, v.y, v.x), (20, 10, 20, 10))
    assertEquals(log.toArray.toList, List("y", "x"))
  }

  test("fields of one override use each other via self") {
    val v = Overridable.build(
      deps,
      Override[Deps]((dep1 = Dep("x"), dep2 = Dep(self.dep1.name + "!"))))
    assertEquals(v.dep2, Dep("x!"))
    assertEquals(v.dep3, Dep("base3(x!)"))
  }

  test("a fallback (variable) override is computed as a whole, once") {
    val effects = AtomicInteger()
    val partial: XY = (x = { effects.incrementAndGet(); 10 }, y = 20)
    val v = Overridable.build(Overridable[XY]((x = 1, y = 2)), Override[XY](partial))
    assertEquals((v.y, v.x, v.x), (20, 10, 10))
    assertEquals(effects.get, 1)
  }

  test("a `++` override is correct, with upper") {
    val v = Overridable.build(
      Overridable[XY]((x = 1, y = 2)),
      Override[XY]((x = upper.x + 1) ++ (y = upper.y + 1)))
    assertEquals(v.toTuple, (x = 2, y = 3))
  }

  test("an element that itself contains a named-tuple literal stays per field") {
    type Nested = (pair: (Int, Int), n: Int)
    val effects = AtomicInteger()
    val v = Overridable.build(
      Overridable[Nested]((pair = (0, 0), n = 0)),
      Override[Nested]((pair = (a = 10, b = { effects.incrementAndGet(); 20 }).toTuple, n = self.pair._1 + 1)))
    assertEquals(v.n, 11)
    assertEquals(effects.get, 1)
    assertEquals(v.pair, (10, 20))
    assertEquals(effects.get, 1)
  }

  test("an override whose field is a named-tuple value (inner proxy) works") {
    type Nested = (pair: (Int, Int), n: Int)
    val v = Overridable.build(
      Overridable[Nested]((pair = (0, 0), n = 0)),
      Override[Nested]((n = 5)))
    assertEquals(v.toTuple, (pair = (0, 0), n = 5))
  }

  test("a 23-field override (TupleXXL) stays per field lazy") {
    type W23 = (f0: Int, f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int, f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int, f21: Int, f22: Int)
    val effects = AtomicInteger()
    val base = Overridable[W23]((f0 = 0, f1 = 0, f2 = 0, f3 = 0, f4 = 0, f5 = 0, f6 = 0, f7 = 0, f8 = 0, f9 = 0, f10 = 0, f11 = 0, f12 = 0, f13 = 0, f14 = 0, f15 = 0, f16 = 0, f17 = 0, f18 = 0, f19 = 0, f20 = 0, f21 = 0, f22 = 0))
    val v = Overridable.build(
      base,
      Override[W23]((f0 = 0, f1 = 1, f2 = 2, f3 = 3, f4 = 4, f5 = { effects.incrementAndGet(); 5 }, f6 = self.f5 + 1, f7 = 7, f8 = 8, f9 = 9, f10 = 10, f11 = 11, f12 = 12, f13 = 13, f14 = 14, f15 = 15, f16 = 16, f17 = 17, f18 = 18, f19 = 19, f20 = 20, f21 = 21, f22 = 22)))
    assertEquals(v.f22, 22)
    assertEquals(effects.get, 0)
    assertEquals(v.f6, 6)
    assertEquals(effects.get, 1)
  }

  test("a 23-field base (TupleXXL) with self is per field lazy") {
    type W23 = (f0: Int, f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int, f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int, f21: Int, f22: Int)
    val effects = AtomicInteger()
    val v = Overridable.build(Overridable[W23]((f0 = 0, f1 = 1, f2 = 2, f3 = 3, f4 = 4, f5 = { effects.incrementAndGet(); 5 }, f6 = self.f5 + 1, f7 = 7, f8 = 8, f9 = 9, f10 = 10, f11 = 11, f12 = 12, f13 = 13, f14 = 14, f15 = 15, f16 = 16, f17 = 17, f18 = 18, f19 = 19, f20 = 20, f21 = 21, f22 = self.f6 + 16)))
    assertEquals(v.f21, 21)
    assertEquals(effects.get, 0)
    assertEquals(v.f22, 22)
    assertEquals(effects.get, 1)
    assertEquals(v.f6, 6)
    assertEquals(effects.get, 1)
  }

  test("a base field whose type is a named tuple") {
    type Nested = (pair: (a: Int, b: Int), n: Int)
    val v = Overridable.build(
      Overridable[Nested]((pair = (a = 1, b = 2), n = self.pair.a + 1)),
      Override[Nested]((pair = (a = 5, b = 6))))
    assertEquals(v.toTuple, (pair = (a = 5, b = 6), n = 6))
  }

  test("a field that failed is retried by the next caller") {
    val attempts = AtomicInteger()
    val v = Overridable.build(Overridable[XY]((
      x = { if attempts.incrementAndGet() == 1 then throw IllegalArgumentException("boom"); 1 },
      y = 2)))
    intercept[IllegalArgumentException](v.x)
    assertEquals(v.x, 1)
    assertEquals(attempts.get, 2)
  }

  test("toTuple computes every field") {
    assertEquals(Overridable.build(deps).toTuple, (Dep("base1"), Dep("base2"), Dep("base3(base2)")))
  }

  test("field names are not hidden by View members") {
    val v = Overridable.build(Overridable[(name: String, value: Int)]((name = "n", value = 1)))
    assertEquals(v.toTuple, (name = "n", value = 1))
  }
}
