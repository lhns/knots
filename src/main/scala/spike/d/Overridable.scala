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
  // `o` defines every member of A (`new { lazy val dep1 = ... }`, `(dep1 = ..., ...)`, see Macros).
  inline def apply[A](inline o: Self[A] ?=> Any): Overridable[A] =
    s => Macros.derive[A, Self[A]](o, s, isBase = true)

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

// Generated members run through `guard`, so a dependency cycle (a member asked for again while it is
// still being computed on this thread) fails with the path instead of a lazy val waiting on itself.
object Cycles {
  private val inProgress = ThreadLocal.withInitial(() => scala.collection.mutable.ArrayBuffer.empty[(AnyRef, String)])

  def guard[T](layer: AnyRef, member: String)(compute: => T): T = {
    val stack = inProgress.get
    val start = stack.indexWhere((l, m) => (l eq layer) && m == member)
    if start >= 0 then {
      // A member forwarded through several layers shows up once per layer; show it once.
      val path = (stack.drop(start).map(_._2) :+ member).foldRight(List.empty[String]) {
        case (m, next :: rest) if m == next => next :: rest
        case (m, rest) => m :: rest
      }
      throw new IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")
    }
    stack += ((layer, member))
    try compute
    finally stack.remove(stack.length - 1)
  }
}

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
