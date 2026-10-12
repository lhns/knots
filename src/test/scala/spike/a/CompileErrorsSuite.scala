package spike.a

class CompileErrorsSuite extends munit.FunSuite {
  private def assertMentions(errors: String, needle: String): Unit =
    assert(errors.contains(needle), clue(errors))

  private def assertRejected(errors: String): Unit = assert(errors.nonEmpty, "expected a compile error")

  test("override with an unknown field") {
    assertMentions(compileErrors("""Override[Deps]((dep9 = Dep("x")))"""), "unknown field: dep9")
  }

  test("override with the wrong type") {
    assertMentions(compileErrors("""Override[Deps]((dep2 = "x"))"""), "wrong type for field dep2")
  }

  test("base missing a field") {
    assertMentions(compileErrors("""Overridable[Deps]((dep1 = Dep("1"), dep2 = Dep("2")))"""), "missing fields: dep3")
  }

  test("base missing several fields names them all") {
    assertMentions(compileErrors("""Overridable[Deps]((dep2 = Dep("2")))"""), "missing fields: dep1, dep3")
  }

  test("base with an unknown field or the wrong type") {
    assertMentions(compileErrors("""Overridable[Deps]((dep1 = Dep("1"), dep2 = Dep("2"), dep3 = Dep("3"), dep9 = Dep("9")))"""), "unknown field: dep9")
    assertMentions(compileErrors("""Overridable[Deps]((dep1 = Dep("1"), dep2 = "x", dep3 = Dep("3")))"""), "wrong type for field dep2")
  }

  test("bare field name instead of self.name") {
    assertMentions(compileErrors("""Override[Deps]((dep2 = dep1))"""), "dep1")
  }

  test("self outside a literal") {
    assertMentions(compileErrors("""self[Deps]"""), "only available inside")
  }

  test("upper in a base") {
    assertRejected(compileErrors("""Overridable[Deps]((dep1 = upper.dep1, dep2 = Dep("2"), dep3 = Dep("3")))"""))
  }

  test("build accepts only a base") {
    assertRejected(compileErrors("""Overridable.build(Override[Deps]((dep1 = Dep("x"))))"""))
  }

  test("raw layer in an override needs module(...)") {
    assertMentions(
      compileErrors("""Override[AppDeps]((db = Override[DbDeps]((transactor = Transactor("t")))))"""),
      "wrap it in module(...)")
  }

  test("raw base layer in an override needs module(...)") {
    assertMentions(compileErrors("""Override[AppDeps]((db = new App().dbBase("u")))"""), "wrap it in module(...)")
  }

  test("raw layer in a base needs module(...)") {
    assertMentions(compileErrors(
      """Overridable[AppDeps]((config = AppConfig("x", 1), db = new App().dbBase("u"), server = module(new App().serverBase(???, 1))))"""),
      "wrap it in module(...)")
  }

  test("override module in a base") {
    assertRejected(compileErrors(
      """Overridable[AppDeps]((config = AppConfig("x", 1), db = module(Override[DbDeps]((transactor = Transactor("t")))), server = module(new App().serverBase(???, 1))))"""))
  }

  test("module of the wrong set") {
    assertRejected(compileErrors("""Override[AppDeps]((db = module(Override[ServerDeps]((port = 1)))))"""))
  }

  test("module on a plain field") {
    assertRejected(compileErrors("""Override[AppDeps]((config = module(Override[DbDeps]((transactor = Transactor("t"))))))"""))
  }

  test("module outside a literal") {
    assertRejected(compileErrors("""module(new App().dbBase("u"))"""))
  }

  test("reserved name in a base") {
    assertMentions(compileErrors("""Overridable[(toString: Int)]((toString = 1))"""), "reserved field name: toString")
  }

  test("reserved name in an override") {
    assertMentions(compileErrors("""Override[(toTuple: Int)]((toTuple = 1))"""), "reserved field name: toTuple")
  }

  test("every listed reserved name is rejected") {
    assertMentions(compileErrors("""Overridable[(selectDynamic: Int)]((selectDynamic = 1))"""), "reserved field name: selectDynamic")
    assertMentions(compileErrors("""Overridable[(Fields: Int)]((Fields = 1))"""), "reserved field name: Fields")
    assertMentions(compileErrors("""Overridable[(hashCode: Int)]((hashCode = 1))"""), "reserved field name: hashCode")
    assertMentions(compileErrors("""Overridable[(equals: Int)]((equals = 1))"""), "reserved field name: equals")
    assertMentions(compileErrors("""Overridable[(getClass: Int)]((getClass = 1))"""), "reserved field name: getClass")
  }

  test("ordinary names are accepted") {
    assertEquals(compileErrors("""Overridable[(name: String, value: Int)]((name = "n", value = 1))"""), "")
  }

  test("statements before the override literal are rejected") {
    assertMentions(
      compileErrors("""Override[Deps]({ println("hi"); (dep2 = Dep("x")) })"""),
      "statements before the named tuple are not supported")
  }
}
