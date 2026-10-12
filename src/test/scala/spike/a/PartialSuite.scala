package spike.a

// SubsetOf and Partial know nothing about Overridable or modules.
class PartialSuite extends munit.FunSuite {
  test("Partial fills the missing fields with None") {
    val p = Partial[Deps]((dep2 = Dep("x")))
    assertEquals(p.toTuple, (None, Some(Dep("x")), None))
  }

  test("Partial of the whole set") {
    val p = Partial[XY]((x = 1, y = 2))
    assertEquals(p.toTuple, (Some(1), Some(2)))
  }

  test("Partial keeps field order of F, not of the literal") {
    val p = Partial[XY]((y = 2, x = 1))
    assertEquals(p.toTuple, (Some(1), Some(2)))
  }

  test("SubsetOf is summoned for a subset") {
    summon[(dep2: Dep) SubsetOf Deps]
    summon[(dep3: Dep, dep1: Dep) SubsetOf Deps]
  }

  test("SubsetOf rejects an unknown field") {
    assert(compileErrors("summon[(dep9: Dep) SubsetOf Deps]").contains("unknown field: dep9"))
  }

  test("SubsetOf rejects the wrong type") {
    assert(compileErrors("summon[(dep1: Int) SubsetOf Deps]").contains("wrong type for field dep1"))
  }

  test("SubsetOf accepts a subtype") {
    summon[(x: Int) SubsetOf (x: Any)]
  }

  test("SubsetOf has no module rules") {
    val errors = compileErrors("summon[(db: Override[DbDeps]) SubsetOf AppDeps]")
    assert(errors.contains("wrong type for field db"), clue(errors))
  }

  test("fieldNames lists names in order") {
    assertEquals(fieldNames[Deps], List("dep1", "dep2", "dep3"))
  }
}
