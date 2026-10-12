package spike.c

import cats.Monoid

trait Self[A] {
  def self: A
}

trait Super[A] extends Self[A] {
  def upper: A
}

// Plain defs, not inline: the literal check recognizes calls to them in the tree.
def self[A](using s: Self[A]): A = s.self

def upper[A](using s: Super[A]): A = s.upper

// Opaque (over plain functions: context function types can't be opaque) so a value isn't
// auto-applied like a bare context function: `val o = Override(...)` needs no type ascription.
opaque type Override[A] = Super[A] => A

// A base is an override that doesn't need `upper`, so it can stand wherever an Override can,
// e.g. on top of another base to replace it completely.
opaque type Overridable[A] <: Override[A] = Self[A] => A

object Overridable {
  // `o` is `new A { ... }` overriding every member of A (checked at compile time). A's
  // constructor needs a Super, so the base gets one whose `upper` it can't use.
  inline def apply[A](inline o: Super[A] ?=> A): Overridable[A] = {
    Macros.checkLiteral[A](o, base = true)
    s => o(using baseSuper(s))
  }

  private def baseSuper[A](s: Self[A]): Super[A] = new Super[A] {
    def self: A = s.self
    def upper: A = throw new IllegalStateException("the base has no upper")
  }

  // Layering on a base keeps it a base (it still needs no upper); Override's monoid alone
  // would widen the result to Override.
  extension [A](base: Overridable[A]) def |+|(ovr: Override[A]): Overridable[A] = stack(base, ovr)

  def build[A](base: Overridable[A], overrides: Override[A]*): A = {
    val knot = stack(base, Override.monoid[A].combineAll(overrides))
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
  // `o` is `new A { override ... }`; members it doesn't override fall back to A's `inherited` defaults.
  inline def apply[A](inline o: Super[A] ?=> A): Override[A] = {
    Macros.checkLiteral[A](o, base = false)
    s => o(using s)
  }

  // `x |+| y` stacks y on top of x: y's `upper` is x's result. `empty` passes `upper` through.
  // Named so this file can pass it explicitly: here Override[A] is transparent, so implicit
  // search would find cats' instance for plain functions instead.
  given monoid[A]: Monoid[Override[A]] with {
    def empty: Override[A] = _.upper
    def combine(x: Override[A], y: Override[A]): Override[A] = stack(x, y)
  }
}

// Puts `ovr` on top of `lower`, which is a base (S = Self) or another override (S = Super).
private def stack[A, S <: Self[A]](lower: S => A, ovr: Override[A]): S => A =
  s => ovr(new Super[A] {
    lazy val upper: A = lower(s)
    def self: A = s.self
  })
