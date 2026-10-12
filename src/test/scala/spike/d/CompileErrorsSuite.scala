package spike.d

import cats.syntax.all.*

class CompileErrorsSuite extends munit.FunSuite {
  import CompileErrorsSuite.*

  private def assertErrors(errors: String, expected: String*)(using munit.Location): Unit = {
    assert(errors.nonEmpty, "expected a compile error, but it compiled")
    expected.foreach(e => assert(errors.contains(e), s"missing '$e' in:\n$errors"))
  }

  test("unknown field") {
    assertErrors(compileErrors("Overridable[AB]((a = 1, b = 2, c = 3))"), "c is not a member of AB")
  }

  test("wrong type") {
    assertErrors(compileErrors("""Overridable[AB]((a = 1, b = "x"))"""), "b has type", "expected Int")
  }

  test("missing base field") {
    assertErrors(compileErrors("Overridable[AB]((a = 1))"), "missing members of AB: b")
  }

  test("bare name instead of a literal") {
    assertErrors(compileErrors("val x = 1; Overridable[AB](x)"))
  }

  test("target is not a trait") {
    assertErrors(compileErrors("Overridable[Cls]((a = 1))"), "must be a trait")
    assertErrors(compileErrors("Overridable[Int](1)"), "must be a trait")
  }

  test("overloaded member") {
    assertErrors(compileErrors("Overridable[Over]((a = 1))"), "overloaded member")
  }

  test("abstract val") {
    assertErrors(compileErrors("Overridable[AbsVal]((a = 1))"), "abstract val a", "declare it as def a")
  }

  test("abstract var") {
    assertErrors(compileErrors("Overridable[AbsVar]((a = 1))"), "abstract var a", "declare it as def a")
  }

  test("concrete trait member") {
    assertErrors(compileErrors("Overridable[Concrete]((a = 1))"), "concrete member b")
  }

  test("default arguments") {
    assertErrors(compileErrors("Overridable[Defaults]((f = (x: Int) => x))"), "default arguments")
  }

  test("strict val in an object literal") {
    assertErrors(compileErrors("Overridable[AB](new { val a = 1; def b = 2 })"), "use def (or lazy val)")
  }

  test("statements before a named-tuple literal") {
    assertErrors(
      compileErrors("Overridable[AB]({ println(\"x\"); (a = 1, b = 2) })"),
      "statements before the named tuple",
    )
  }

  test("a layer where a plain layer literal is expected") {
    assertErrors(compileErrors("Override[AB](Override[AB]((a = 1)))"), "this is a layer")
  }

  test("wrong argument kind: a plain value") {
    assertErrors(compileErrors("""Overridable[AB]("text")"""), "expected (name = ...)")
    assertErrors(compileErrors("Overridable[AB](List(1))"), "expected (name = ...)")
  }

  test("null") {
    assertErrors(compileErrors("Overridable[AB](null)"), "null is not supported")
  }

  test("empty argument") {
    assertErrors(compileErrors("Overridable[AB](new {})"), "supplies no field")
    assertErrors(compileErrors("Override[AB](new {})"), "supplies no field")
  }

  test("reserved View field names") {
    assertErrors(
      compileErrors("Overridable[View[(toString: Int, a: Int)]]((toString = 1, a = 2))"),
      "field name toString is reserved",
    )
  }

  test("a base ignores nothing: fields of an override still need to exist") {
    assertErrors(compileErrors("Override[AB]((z = 1))"), "z is not a member of AB")
  }

  test("module: raw layer instead of module(...)") {
    assertErrors(
      compileErrors("Overridable[Outer]((n = 1, inner = Overridable[Inner]((x = 1))))"),
      "wrap it in module(...)",
    )
  }

  test("module: an Override in a base") {
    assertErrors(
      compileErrors("Overridable[Outer]((n = 1, inner = module(Override[Inner]((x = 1)))))"),
      "a base needs a complete module",
    )
  }

  test("module: layer of the wrong type") {
    assertErrors(
      compileErrors("Overridable[Outer]((n = 1, inner = module(Overridable[AB]((a = 1, b = 2)))))"),
      "inner",
    )
  }

  test("self outside a layer literal") {
    assertErrors(compileErrors("self[AB]"), "only available inside")
  }

  test("upper in a base") {
    assertErrors(compileErrors("Overridable[AB]((a = 1, b = upper.a))"), "only available in an Override")
  }
}

object CompileErrorsSuite {
  trait AB { def a: Int; def b: Int }
  class Cls { def a: Int = 1 }
  trait Over { def a: Int; def a(x: Int): Int }
  trait AbsVal { val a: Int }
  trait AbsVar { var a: Int }
  trait Concrete { def a: Int; def b: Int = 2 }
  trait Defaults { def f(x: Int = 1): Int }
  trait Inner { def x: Int }
  trait Outer { def n: Int; def inner: Inner }
}
