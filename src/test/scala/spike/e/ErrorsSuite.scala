package spike.e

import Fx.*

// Compile-time rejections. The target types live here so the code strings can refer to them.
object ErrFx {
  trait DbDeps { def tx: String }
  trait SrvDeps { def r: String }
  trait Root { def db: DbDeps; def n: Int }
  trait HasLayer { def l: Override[AB]; def n: Int }
  trait Helper { def h: String = "helper" }
  trait WithDefault { def a: Int; def b: Int = a + 1 }
  trait WithVal { def a: Int; val b: Int = a + 1 }
  trait WithLazy { def a: Int; lazy val b: Int = a + 1 }
  trait WithVar { def a: Int; var b: Int = 0 }
  trait Parent { def p: Int = 1 }
  trait Inherits extends Parent { def a: Int }
  trait DefaultArgs { def f(x: Int = 1): Int; def a: Int }
  trait AbsVar { var v: Int; def a: Int }
  trait AbsVal { val v: Int }
  trait Logger { def log(s: String): Unit = () }
  trait SelfTyped { this: Logger => def x: Int }
  trait CtorParam(val n: Int) { def x: Int }
  class SomeClass
  trait ExtendsClass extends SomeClass { def x: Int }
  trait ProtAbs { protected def secret: Int; def x: Int }
  trait ThisType { def me: this.type; def x: Int }
  trait TypeMember { type X; def x: X; def y: Int }
  trait Slot { def slot: Int; def at: Int; def x: Int }
  trait EmptyParen { def close(): Unit; def g: Int }
  trait Param { def f(x: Int): Int; def g: Int }
  trait Ov { def o(x: Int): Int; def o(x: String): Int }
  class WithHelper { def a = D("h"); def helper = "h" }
}

class ErrorsSuite extends munit.FunSuite {
  import ErrFx.*

  // Every expected snippet must appear in the compile error. (compileErrors must be called directly
  // with the literal: through an inline helper the macro's errors surface as plain type mismatches.)
  def check(errs: String, expected: String*)(using munit.Location): Unit = {
    assert(errs.nonEmpty, "expected a compile error")
    expected.foreach(s => assert(errs.contains(s), clue(errs)))
  }

  // --- the tuple against the trait
  test("unknown field")(check(compileErrors("""Overridable[AB]((a = D("a"), b = D("b"), c = 1))"""), "c is not a member of AB"))
  test("wrong field type")(check(compileErrors("""Overridable[AB]((a = D("a"), b = 1))"""), "b has type Int, expected D"))
  test("missing field in a base")(check(compileErrors("""Overridable[AB]((a = D("a")))"""), "missing members of AB: b"))
  test("override with unknown field")(check(compileErrors("""Override[AB]((zzz = D("a")))"""), "zzz is not a member of AB"))
  test("bare name of another field")(check(compileErrors("""Override[AB]((a = b))""") ))
  test("self of an unknown field")(check(compileErrors("""Override[AB]((a = self.zzz))""") ))
  test("view: unknown field")(check(compileErrors("""Override[View[(a: D, b: D)]]((zzz = D("x")))"""), "zzz"))
  test("view: wrong type")(check(compileErrors("""Override[View[(a: D, b: D)]]((a = "x"))""") ))
  test("view: missing field in a base")(check(compileErrors("""Overridable[View[(a: D, b: D)]]((a = D("1")))"""), "missing members"))
  test("not a trait")(check(compileErrors("""Overridable[SomeClass]((a = 1))"""), "SomeClass must be a trait"))
  test("members named slot and at are fine")(assertEquals(compileErrors("""Overridable[Slot]((slot = 1, at = self.slot + 1, x = self.at))"""), ""))

  // --- object-literal members may only use self/upper
  test("literal members using each other")(check(compileErrors("""Override[AB](new { def a = b; def b = D("b") })"""), "a uses b of the object literal"))
  test("literal member using this")(check(compileErrors("""Override[AB](new { def a = this.b; def b = D("b") })"""), "a uses b of the object literal"))
  test("literal private helper")(check(compileErrors("""Override[AB](new { private def h = "h"; def a = D(h) })"""), "a uses h of the object literal"))
  test("literal inheriting a helper")(check(compileErrors("""Override[AB](new Helper { def a = D(h) })"""), "inherited by the object literal from Helper"))
  test("literal with parameters")(check(compileErrors("""Override[AB](new { def a(x: Int) = D("a") })"""), "object literal members can't take parameters"))
  test("literal with empty parameter list")(check(compileErrors("""Override[AB](new { def a() = D("a") })"""), "object literal members can't take parameters"))
  test("literal nested class")(check(compileErrors("""Override[AB](new { class X; def a = D("a") })"""), "nested class X"))
  test("literal type member")(check(compileErrors("""Override[AB](new { type T = Int; def a = D("a") })"""), "type member T"))
  test("literal var")(check(compileErrors("""Override[AB](new { var a = D("a") })"""), "var a in an object literal"))

  // --- trait targets must be fully abstract
  test("overloaded member")(check(compileErrors("""Override[Ov]((o = 1))"""), "overloaded member o"))
  test("abstract val")(check(compileErrors("""Overridable[AbsVal]((v = 1))"""), "abstract val v"))
  test("abstract var")(check(compileErrors("""Overridable[AbsVar]((v = 1, a = 1))"""), "abstract var v"))
  test("concrete def")(check(compileErrors("""Overridable[WithDefault]((a = 1))"""), "concrete member b in WithDefault"))
  test("concrete val")(check(compileErrors("""Overridable[WithVal]((a = 1))"""), "concrete member b in WithVal"))
  test("concrete lazy val")(check(compileErrors("""Overridable[WithLazy]((a = 1))"""), "concrete member b in WithLazy"))
  test("concrete var")(check(compileErrors("""Overridable[WithVar]((a = 1))"""), "concrete member b in WithVar"))
  test("concrete inherited member")(check(compileErrors("""Overridable[Inherits]((a = 1))"""), "concrete member p in Inherits"))
  test("concrete member in an override too")(check(compileErrors("""Override[WithDefault]((a = 1))"""), "concrete member b"))
  test("default arguments")(check(compileErrors("""Overridable[DefaultArgs]((f = 1, a = 1))"""), "f has default arguments"))
  test("self type")(check(compileErrors("""Overridable[SelfTyped]((x = 1))"""), "has a self type"))
  test("constructor parameters")(check(compileErrors("""Overridable[CtorParam]((x = 1))"""), "has constructor parameters"))
  test("refinement type")(check(compileErrors("""Overridable[AB { def c: Int }]((a = D("a"), b = D("b")))"""), "refinement types are not supported"))
  test("trait extending a class")(check(compileErrors("""Overridable[ExtendsClass]((x = 1))"""), "extends the class SomeClass"))
  test("abstract protected member")(check(compileErrors("""Overridable[ProtAbs]((x = 1))"""), "abstract protected member secret"))
  test("this.type member")(check(compileErrors("""Overridable[ThisType]((me = ???, x = 1))"""), "mentions this.type"))
  test("abstract type member")(check(compileErrors("""Overridable[TypeMember]((x = 1, y = 2))"""), "abstract type member X"))
  test("intersection type")(check(compileErrors("""Overridable[AB & DbDeps]((a = D("a"), b = D("b"), tx = "x"))"""), "must be a trait"))

  // --- members with parameter lists need an instance of a class
  test("empty-paren member via tuple")(check(compileErrors("""Overridable[EmptyParen]((close = println("closed"), g = 2))"""), "close has a parameter list"))
  test("parameter member via tuple")(check(compileErrors("""Overridable[Param]((f = (x: Int) => x, g = 1))"""), "f has a parameter list"))

  // --- reserved View names
  test("view field selectDynamic")(check(compileErrors("""Overridable[View[(selectDynamic: Int, x: Int)]]((selectDynamic = 5, x = 1))"""), "View field selectDynamic is reserved"))
  test("view field toString")(check(compileErrors("""Overridable[View[(toString: String, x: Int)]]((toString = "t", x = 1))"""), "View field toString is reserved"))
  test("view fields hashCode, equals, getClass")(check(compileErrors("""Overridable[View[(hashCode: Int, equals: Int, getClass: Int)]]((hashCode = 1, equals = 2, getClass = 3))"""), "hashCode is reserved", "equals is reserved", "getClass is reserved"))

  // --- statements and argument kinds
  test("statements before the literal")(check(compileErrors("""Overridable[AB]({ println("side"); (a = D("a"), b = D("b")) })"""), "statements before the named tuple"))
  test("statements before the literal in an override")(check(compileErrors("""Override[AB]({ println("side"); (a = D("o")) })"""), "statements before the named tuple"))
  test("null")(check(compileErrors("""Override[AB](null)"""), "null is not a valid argument"))
  test("empty object literal")(check(compileErrors("""Override[AB](new {})"""), "supplies no field of AB"))
  test("unit")(check(compileErrors("""Override[AB](())"""), "expected (name = ...)"))
  test("Int")(check(compileErrors("""Override[AB](42)"""), "expected (name = ...)"))
  test("String")(check(compileErrors("""Override[AB]("s")"""), "expected (name = ...)"))
  test("Option")(check(compileErrors("""Override[AB](Some(D("x")))"""), "expected (name = ...)"))
  test("function")(check(compileErrors("""Override[AB]((x: Int) => x)"""), "expected (name = ...)"))
  test("plain tuple")(check(compileErrors("""Override[AB]((D("a"), D("b")))"""), "expected (name = ...)"))
  test("Any-typed value")(check(compileErrors("""{ val x: Any = (a = D("x")); Override[AB](x) }"""), "expected (name = ...)"))
  test("union from if")(check(compileErrors("""Override[AB](if scala.util.Random.nextBoolean() then (a = D("x")) else (b = D("y")))"""), "expected (name = ...)"))
  test("object literal kept in a val")(check(compileErrors("""{ val lit = new { def a = D("lit") }; Override[AB](lit) }"""), "expected (name = ...)"))
  test("class supplying no field")(check(compileErrors("""{ class Other { def zzz = 1 }; Override[AB](new Other) }"""), "supplies no field of AB"))
  test("class with an extra public member")(check(compileErrors("""Override[AB](new WithHelper)"""), "helper is not a member of AB"))
  test("a layer passed as the argument")(check(compileErrors("""{ val base = Overridable[AB]((a = D("a"), b = D("b"))); Override[AB](base) }"""), "combine layers with |+|"))
  test("an Override passed to Overridable")(check(compileErrors("""{ val o = Override[AB]((a = D("x"))); Overridable[AB](o) }"""), "combine layers with |+|"))

  // --- self / upper availability
  test("upper in a base")(check(compileErrors("""Overridable[AB]((a = D("a"), b = upper.a))"""), "upper is only available in an Override"))
  test("self outside a layer")(check(compileErrors("""self[AB]"""), "only available inside Overridable"))

  // --- modules
  test("module in a base")(assertEquals(compileErrors("""Overridable[Root]((db = module(Overridable[DbDeps]((tx = "x"))), n = 1))"""), ""))
  test("module(Override) in an override")(assertEquals(compileErrors("""Override[Root]((db = module(Override[DbDeps]((tx = "x")))))"""), ""))
  test("module(Overridable) in an override")(assertEquals(compileErrors("""Override[Root]((db = module(Overridable[DbDeps]((tx = "x")))))"""), ""))
  test("a layer-typed field takes a layer")(assertEquals(compileErrors("""Overridable[HasLayer]((l = Override[AB]((a = D("a"))), n = 1))"""), ""))
  test("raw Overridable for a module field")(check(compileErrors("""Overridable[Root]((db = Overridable[DbDeps]((tx = "x")), n = 1))"""), "wrap it in module(...)"))
  test("raw Override for a module field")(check(compileErrors("""Override[Root]((db = Override[DbDeps]((tx = "x"))))"""), "wrap it in module(...)"))
  test("module(Override) in a base")(check(compileErrors("""Overridable[Root]((db = module(Override[DbDeps]((tx = "x"))), n = 1))"""), "a base needs a complete module"))
  test("module of the wrong trait")(check(compileErrors("""Override[Root]((db = module(Override[SrvDeps]((r = "x")))))"""), "expected DbDeps or a module(...) of it"))
  test("module outside a layer literal")(check(compileErrors("""module(Overridable[DbDeps]((tx = "x")))"""), "only available inside Overridable"))
  test("module for a plain field")(check(compileErrors("""Overridable[Root]((db = module(Overridable[DbDeps]((tx = "x"))), n = module(Overridable[DbDeps]((tx = "x")))))"""), "n has type BaseModule[DbDeps], expected Int"))
}
