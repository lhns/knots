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

  test("an override layer is computed once for all its fields") {
    val effects = AtomicInteger()
    val v = Overridable.build(
      Overridable[XY]((x = 1, y = 2)),
      Override[XY]((x = { effects.incrementAndGet(); 10 }, y = 20)))
    assertEquals((v.x, v.y, v.x), (10, 20, 10))
    assertEquals(effects.get, 1)
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
