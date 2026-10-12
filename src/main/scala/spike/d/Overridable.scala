package spike.d

import cats.Monoid

import scala.NamedTuple.{AnyNamedTuple, Names}
import scala.compiletime.constValueTuple

trait Self[A] {
  def self: A
}

trait Upper[A] extends Self[A] {
  def upper: A
}

inline def self[A](using s: Self[A]): A = s.self

inline def upper[A](using s: Upper[A]): A = s.upper

// Opaque (over plain functions: context function types can't be opaque) so a value isn't
// auto-applied like a bare context function: `val o = Override(...)` needs no type ascription.
opaque type Override[A] = Upper[A] => A

// A base is an override that doesn't need `upper`, so it can stand wherever an Override can,
// e.g. as a child of another base, replacing it completely.
opaque type Overridable[A] <: Override[A] = Self[A] => A

object Overridable {
  // `o` defines every member of A (`(dep1 = ..., ...)`, `new { def dep1 = ... }`, see Macros).
  inline def apply[A](inline o: Self[A] ?=> Any): Overridable[A] =
    s => Macros.derive[A, Self[A]](o, s, isBase = true)

  // A module: a field whose type is a dependency trait of its own (`db: DbDeps`), supplied with that
  // trait's layer (`db = DbModule.base(...)`). The layer runs focused through the field: its `self`
  // is the root's final `db`, so late binding works through the whole tree. In a base there is no
  // layer below, so the module must be complete (see Override.focus for overrides).
  def focus[A, B](ctx: Self[A], get: A => B, module: Overridable[B]): B =
    module(new Self[B] { def self: B = get(ctx.self) })

  // Layering on a base keeps it a base (it still needs no upper); Override's monoid alone
  // would widen the result to Override.
  extension [A](base: Overridable[A]) def |+|(ovr: Override[A]): Overridable[A] = layer(base, ovr)

  def build[A](base: Overridable[A], overrides: Override[A]*): A = {
    val knot = layer(base, Override.monoid[A].combineAll(overrides))
    var r: A | Null = null
    r = knot(new Self[A] {
      def self: A = r match {
        case null => throw new IllegalStateException("cannot evaluate untied knot")
        case a: A @unchecked => a
      }
    })
    r.asInstanceOf[A]
  }
}

object Override {
  // `o` defines only the overridden members (`new { ... }`, `(dep2 = ...)`); the rest come from `upper`.
  inline def apply[A](inline o: Upper[A] ?=> Any): Override[A] =
    s => Macros.derive[A, Upper[A]](o, s, isBase = false)

  // A module in an override (see Overridable.focus): its `upper` is the `db` of the layer below, so
  // `db = Override[DbDeps](...)` is a deep override and `db = DbModule.base(...)` replaces the module.
  def focus[A, B](ctx: Upper[A], get: A => B, module: Override[B]): B =
    module(new Upper[B] {
      def self: B = get(ctx.self)
      def upper: B = get(ctx.upper)
    })

  // `x |+| y` is y extending x: y's `upper` (its super) is x's result. `empty` passes `upper` through.
  // Named so this file can pass it explicitly: here Override[A] is transparent, so implicit
  // search would find cats' instance for plain functions instead.
  given monoid[A]: Monoid[Override[A]] with {
    def empty: Override[A] = _.upper
    def combine(x: Override[A], y: Override[A]): Override[A] =
      layer(x, y)
  }
}

// Like `class Child extends Parent`: `upper` is `super`, `self` is the final `this`. The child's
// `upper` is the parent's result; both see the same `s`, i.e. the same final `self` and, if this
// pair is itself an override in a bigger chain, the same outer `upper`. The parent is a base
// (S = Self) or another override (S = Upper).
private def layer[A, S <: Self[A]](parent: S => A, child: Override[A]): S => A =
  s => child(new Upper[A] {
    lazy val upper: A = parent(s)
    def self: A = s.self
  })

// The members the current computation is in the middle of, outermost first. A ScopedValue is bound
// for the extent of a call and seen by everything it calls, without depending on thread identity.
private val computing: ScopedValue[List[Cell]] = ScopedValue.newInstance()

// Caches one generated member and detects dependency cycles. The value is a lazy val: computed once,
// other callers wait, retried after an exception. It is computed with `computing` extended by this
// cell, so asking for a member the current computation is already in the middle of fails with the
// path before reaching the lazy val, which would otherwise wait on itself forever.
final class Cell(val name: String, compute: () => Any) {
  private lazy val value: Any = ScopedValue.where(computing, current :+ this).call(() => compute())

  def get: Any = {
    val path = current
    if path.contains(this) then throw new CycleException(path.dropWhile(_ ne this).map(_.name) :+ name)
    value
  }
}

private def current: List[Cell] = if computing.isBound then computing.get else Nil

final class CycleException(path: List[String]) extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")

// A named tuple's shape as an object whose fields are computed on first access, so `self.dep2`
// works while the knot is being tied (a tuple can't exist before its fields). As a target:
//   type MainDeps = View[(dep1: Dependency, dep2: Dependency)]; Overridable[MainDeps](...)
trait View[F <: AnyNamedTuple] extends Selectable {
  type Fields = F
  def selectDynamic(name: String): Any
}

extension [F <: AnyNamedTuple](view: View[F])
  // Computes every field.
  inline def toTuple: F =
    Tuple.fromArray(constValueTuple[Names[F]].toList.map(n => view.selectDynamic(n.asInstanceOf[String])).toArray[Any]).asInstanceOf[F]
