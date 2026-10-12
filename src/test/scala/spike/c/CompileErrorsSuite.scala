package spike.c.tests

import spike.c.*

object ErrFixtures {
  case class Dependency(name: String)

  trait Deps(using Upper[Deps]) {
    def dep1: Dependency = inherited
    def dep2: Dependency = inherited
    def dep3: Dependency = inherited
  }

  trait Ok(using Upper[Ok]) { def a: Int = inherited }
  trait Helper2(using Upper[Helper2]) { def a: Int = inherited; def h: Int = a + 1 }
  trait ValM(using Upper[ValM]) { def a: Int = inherited; val v: Int = 1 }
  trait VarM(using Upper[VarM]) { def a: Int = inherited; var v: Int = 1 }
  trait Deflt(using Upper[Deflt]) { def a: Int = inherited; def f(x: Int = 1): Int = x }
  trait AbsVar(using Upper[AbsVar]) { def a: Int = inherited; var v: Int }
  trait AbsVal(using Upper[AbsVal]) { def a: Int = inherited; val v: Int }
  trait Foo { def foo: Int = 1 }
  trait SelfT(using Upper[SelfT]) { this: Foo => def a: Int }
  trait Extra(x: Int)(using Upper[Extra]) { def a: Int = inherited }
  trait NoUpper { def a: Int }
  trait Ov(using Upper[Ov]) { def a: Int; def a(i: Int): Int }
  trait Mod(using Upper[Mod]) { def inner: Inner = inherited }
  trait Inner(using Upper[Inner]) { def v: Int = inherited }
  trait Other(using Upper[Other]) { def w: Int = inherited }
}

class CompileErrorsSuite extends munit.FunSuite {
  import ErrFixtures.*

  test("a base must override every member") {
    val e = compileErrors("""Overridable[Deps](new Deps { override lazy val dep1 = Dependency("1"); override lazy val dep2 = Dependency("2") })""")
    assert(e.contains("the base must override every member of Deps; missing: dep3"), e)
  }

  test("a bare member name is rejected") {
    val e = compileErrors("""Override[Deps](new Deps { override lazy val dep3 = dep2 })""")
    assert(e.contains("use self.dep2: a bare dep2 skips overrides"), e)
  }

  test("this.name is rejected") {
    val e = compileErrors("""Override[Deps](new Deps { override lazy val dep3 = this.dep2 })""")
    assert(e.contains("use self.dep2"), e)
  }

  test("upper is rejected in a base") {
    val e = compileErrors("""Overridable[Deps](new Deps {
      override lazy val dep1 = upper.dep1; override lazy val dep2 = Dependency("2"); override lazy val dep3 = Dependency("3") })""")
    assert(e.contains("the base has no upper"), e)
  }

  test("a misspelled override does not override anything") {
    val e = compileErrors("""Override[Deps](new Deps { override lazy val dep9 = Dependency("x") })""")
    assert(e.contains("dep9"), e)
  }

  test("self and upper are accepted in an override") {
    assertEquals(compileErrors("""Override[Deps](new Deps { override lazy val dep3 = Dependency(self.dep2.name + upper.dep3.name) })"""), "")
  }

  test("self and upper are not available outside a literal") {
    assert(compileErrors("""self[Ok]""").contains("self is only available"))
    assert(compileErrors("""upper[Ok]""").contains("upper is only available"))
    assert(compileErrors("""new Ok {}""").nonEmpty)
  }

  test("a concrete trait member that is not `= inherited` is rejected") {
    val e1 = compileErrors("""Override[Helper2](new Helper2 { override lazy val a = 1 })""")
    assert(e1.contains("concrete member h in Helper2 must be exactly"), e1)
    val e2 = compileErrors("""Override[ValM](new ValM { override lazy val a = 1 })""")
    assert(e2.contains("concrete member v in ValM must be exactly"), e2)
    val e3 = compileErrors("""Override[VarM](new VarM { override lazy val a = 1 })""")
    assert(e3.contains("concrete member v"), e3)
  }

  test("default arguments are rejected") {
    val e = compileErrors("""Override[Deflt](new Deflt { override lazy val a = 1 })""")
    assert(e.contains("f has default arguments"), e)
  }

  test("abstract var and val are rejected") {
    val e1 = compileErrors("""Override[AbsVar](new AbsVar { override lazy val a = 1; var v = 1 })""")
    assert(e1.contains("abstract var v"), e1)
    val e2 = compileErrors("""Override[AbsVal](new AbsVal { override lazy val a = 1; val v = 1 })""")
    assert(e2.contains("abstract val v"), e2)
  }

  test("a self-type is rejected") {
    val e = compileErrors("""Override[SelfT](new SelfT with Foo { def a = 1 })""")
    assert(e.contains("has a self-type"), e)
  }

  test("extra constructor parameters are rejected") {
    val e = compileErrors("""Override[Extra](new Extra(1) { override lazy val a = 1 })""")
    assert(e.contains("extra constructor parameters are not supported"), e)
  }

  test("a trait without the Upper parameter is rejected") {
    val e = compileErrors("""Override[NoUpper](new NoUpper { def a = 1 })""")
    assert(e.contains("must be declared with exactly one constructor parameter"), e)
  }

  test("overloaded members are rejected") {
    val e = compileErrors("""Override[Ov](new Ov { def a = 1; def a(i: Int) = i })""")
    assert(e.contains("overloaded member a"), e)
  }

  test("an argument that is not an object literal is rejected") {
    val e1 = compileErrors("""val x: Ok = null; Override[Ok](x)""")
    assert(e1.contains("expected an object literal"), e1)
    val e2 = compileErrors("""Override[Ok](new Ok {}.asInstanceOf[Ok])""")
    assert(e2.contains("expected an object literal"), e2)
  }

  test("null is rejected") {
    val e = compileErrors("""Override[Ok](null)""")
    assert(e.contains("null is not supported"), e)
  }

  test("a var in the literal is rejected") {
    val e = compileErrors("""Override[Ok](new Ok { override var a = 1 })""")
    assert(e.nonEmpty, e)
  }

  test("module(...) that is not the whole body of a member is rejected") {
    val e = compileErrors("""Override[Mod](new Mod { override lazy val inner = { println(); module(Override[Inner](new Inner {})) } })""")
    assert(e.contains("module(...) must be the whole body"), e)
  }

  test("module(...) in a nested call, a non-member and a new member is rejected") {
    val nested = compileErrors("""Override[Mod](new Mod { override lazy val inner = identity(module(Override[Inner](new Inner {}))) })""")
    assert(nested.contains("module(...) must be the whole body"), nested)
    val extra = compileErrors("""Override[Mod](new Mod { lazy val extra = module(Override[Inner](new Inner {})) })""")
    assert(extra.contains("module(...) must be the whole body"), extra)
  }

  test("an Override module in a base is rejected") {
    val e = compileErrors("""Overridable[Mod](new Mod { override lazy val inner = module(Override[Inner](new Inner { override lazy val v = 1 })) })""")
    assert(e.contains("a base needs a complete module"), e)
  }

  test("a module of the wrong type is rejected") {
    val e = compileErrors("""Override[Mod](new Mod { override lazy val inner = module(Override[Other](new Other { override lazy val w = 1 })) })""")
    assert(e.nonEmpty, e)
  }
}
