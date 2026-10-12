package spike.b

import Fx.*

class CompileErrorsSpec extends munit.FunSuite {
  test("an unknown member of an object literal is rejected") {
    val e = compileErrors("""Override[Deps](new { lazy val dep9 = Dep("x") })""")
    assert(e.contains("dep9 is not a member of Deps"), e)
  }

  test("an unknown member of a named class is rejected") {
    assert(compileErrors("Override[Deps](new UnknownMember)").contains("dep9 is not a member of Deps"))
  }

  test("a member of the wrong type is rejected") {
    assert(compileErrors("""Override[Deps](new { lazy val dep2 = "x" })""").contains("dep2 has type String, expected Dep"))
    assert(compileErrors("Override[Deps](new WrongType)").contains("dep2 has type String, expected Dep"))
  }

  test("a base missing a member is rejected") {
    val e = compileErrors("""Overridable[Deps](new { lazy val dep1 = Dep("1"); lazy val dep2 = Dep("2") })""")
    assert(e.contains("missing members of Deps: dep3"), e)
  }

  test("a bare member name without self does not compile") {
    assert(compileErrors("""Override[Deps](new { lazy val dep2 = dep1 })""").contains("Not found: dep1"))
  }

  test("a target that is not a trait is rejected") {
    assert(compileErrors("""Override[Dep](new { lazy val name = "x" })""").contains("must be a trait"))
  }

  test("an overloaded member is rejected") {
    assert(compileErrors("Override[Overloaded](new { def f(x: Int) = 1 })").contains("overloaded member f"))
  }

  test("an abstract val is rejected") {
    assert(compileErrors("Override[AbsVal](new { def x = 1 })").contains("abstract val x"))
  }

  test("an abstract var is rejected") {
    assert(compileErrors("Overridable[AbsVar](new { def v = 1; def a = 2 })").contains("abstract var v"))
  }

  test("a concrete trait member is rejected") {
    assert(compileErrors("Overridable[ConcreteDef](new { def a = 1 })").contains("concrete member b"))
    assert(compileErrors("Overridable[ConcreteVal](new { def a = 1 })").contains("concrete member v"))
  }

  test("default arguments are rejected") {
    assert(compileErrors("Overridable[WithDefault](new { def f(x: Int) = x; def a = 1 })").contains("f has default arguments"))
  }

  test("a strict val in an object literal is rejected, lazy val and def are fine") {
    assert(compileErrors("""Override[Deps](new { val dep2 = Dep("x") })""").contains("use def (or lazy val)"))
    assertEquals(compileErrors("""Override[Deps](new { lazy val dep2 = Dep("x") })"""), "")
    assertEquals(compileErrors("""Override[Deps](new { def dep2 = Dep("x") })"""), "")
  }

  test("a private helper in an object literal is fine, a public one is not") {
    assertEquals(compileErrors("""Override[Deps](new { private val h = 1; def dep2 = Dep(h.toString) })"""), "")
    assert(compileErrors("""Override[Deps](new { def helper = 1; def dep1 = Dep("x") })""").contains("helper is not a member of Deps"))
    assert(compileErrors("Override[Deps](new Helper)").contains("helper is not a member of Deps"))
  }

  test("upper is not available in a base") {
    assert(compileErrors("Overridable[Single](new { def a = upper.a })").contains("upper is only available in an Override"))
  }

  test("self outside a literal does not compile") {
    assert(compileErrors("self[Single].a").contains("self (and module(...)) is only available"))
  }

  test("wrong kinds of argument are rejected") {
    val expected = "expected new { ... }, an instance of a class, or a Deps"
    assert(compileErrors("Override[Deps](42)").contains(expected))
    assert(compileErrors("""Override[Deps]("str")""").contains(expected))
    assert(compileErrors("Override[Deps](Some(1))").contains(expected))
    assert(compileErrors("Override[Deps](new Object)").contains(expected))
  }

  test("a layer as the argument is rejected, combine layers with |+|") {
    val e = compileErrors("""{ val o = Override[Single](new { def a = 1 }); Override[Single](o) }""")
    assert(e.contains("combine layers with |+|"), e)
  }

  test("null is rejected") {
    assert(compileErrors("Override[Deps](null)").contains("null is not supported"))
  }

  test("an empty argument is rejected") {
    assert(compileErrors("Override[Deps](new {})").contains("supplies no member"))
    assert(compileErrors("Override[Deps](())").nonEmpty)
  }

  test("overriding members of Object is rejected") {
    assert(compileErrors("""Override[Deps](new { override def toString = "x"; def dep1 = Dep("x") })""").contains("overriding members of Object is not supported"))
  }

  // Modules.
  test("a raw layer for a module member is rejected, it needs module(...)") {
    assert(compileErrors("""Override[AppDeps](new { def db = Override[DbDeps](new { def transactor = Transactor("t") }) })""").contains("wrap it in module(...)"))
    assert(compileErrors("""Overridable[AppDeps](new { def config = AppConfig("x", 1); def db = Overridable[DbDeps](new { def transactor = Transactor("t"); def userRepo = UserRepo(self.transactor) }); def server = module(serverModule(???, 1)) })""").contains("wrap it in module(...)"))
  }

  test("an Override module in a base is rejected, a base needs a complete module") {
    val e = compileErrors("""Overridable[AppDeps](new {
      def config = AppConfig("x", 1)
      def db = module(Override[DbDeps](new { def transactor = Transactor("t") }))
      def server = module(serverModule(???, 1)) })""")
    assert(e.contains("a base needs a complete module"), e)
  }

  test("a module of the wrong trait is rejected") {
    val e = compileErrors("""Override[AppDeps](new { def db = module(Override[ServerDeps](new { def routes = Routes(???, 1) })) })""")
    assert(e.contains("db has type"), e)
  }

  test("module(...) with an Override or a complete module is accepted in an override") {
    assertEquals(compileErrors("""Override[AppDeps](new { def db = module(Override[DbDeps](new { def transactor = Transactor("t") })) })"""), "")
    assertEquals(compileErrors("""Override[AppDeps](new { def db = module(dbModule("u", new java.util.concurrent.atomic.AtomicInteger)) })"""), "")
  }

  test("module outside a literal does not compile") {
    assert(compileErrors("""module(dbModule("u", new java.util.concurrent.atomic.AtomicInteger))""").contains("only available inside"))
  }
}
