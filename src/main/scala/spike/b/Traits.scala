package spike.b

import cats.Monoid

trait Self[A] {
  def self: A
}

trait Super[A] extends Self[A] {
  def upper: A
}

inline def self[A](using s: Self[A]): A = s.self

inline def upper[A](using s: Super[A]): A = s.upper

// Opaque (over plain functions: context function types can't be opaque) so a value isn't
// auto-applied like a bare context function: `val o = Override(...)` needs no type ascription.
opaque type Overridable[A] = Self[A] => A

opaque type Override[A] = Super[A] => A

object Overridable {
  // `o` defines every abstract member of A: `new { lazy val dep1 = ... }`, see Derive.
  inline def apply[A](inline o: Self[A] ?=> Any): Overridable[A] =
    s => Derive.derive[A, Self[A]](o, s, isBase = true)

  // Needs at least one override so a single argument always means "create" above;
  // overload resolution can't tell a context-function literal from a value otherwise.
  def apply[A](base: Overridable[A], ovr: Override[A], more: Override[A]*): Overridable[A] =
    stack(base, Override.monoid[A].combineAll(ovr +: more))

  def build[A](base: Overridable[A], overrides: Override[A]*): A = {
    val knot = Overridable(base, Override.monoid[A].combineAll(overrides))
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
  // `o` defines only the overridden members; the rest come from `upper`, see Derive.
  inline def apply[A](inline o: Super[A] ?=> Any): Override[A] =
    s => Derive.derive[A, Super[A]](o, s, isBase = false)

  // `x |+| y` stacks y on top of x: y's `upper` is x's result. `empty` passes `upper` through.
  // Named so this file can pass it explicitly: here Override[A] is transparent, so implicit
  // search would find cats' instance for plain functions instead.
  given monoid[A]: Monoid[Override[A]] with {
    def empty: Override[A] = _.upper
    def combine(x: Override[A], y: Override[A]): Override[A] =
      stack(x, y)
  }
}

// Puts `ovr` on top of `lower`, which is a base (S = Self) or another override (S = Super).
private def stack[A, S <: Self[A]](lower: S => A, ovr: Override[A]): S => A =
  s => ovr(new Super[A] {
    lazy val upper: A = lower(s)
    def self: A = s.self
  })
