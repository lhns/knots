package spike.c

import cats.Monoid

trait Self[A] {
  def self: A
}

trait Upper[A] extends Self[A] {
  def upper: A
}

// Plain defs, not inline: the literal check recognizes calls to them in the tree.
def self[A](using s: Self[A]): A = s.self

def upper[A](using s: Upper[A]): A = s.upper

// Opaque (over plain functions: context function types can't be opaque) so a value isn't
// auto-applied like a bare context function: `val o = Override(...)` needs no type ascription.
opaque type Override[A] = Upper[A] => A

// A base is an override that doesn't need `upper`, so it can stand wherever an Override can,
// e.g. as a child of another base, replacing it completely.
opaque type Overridable[A] <: Override[A] = Self[A] => A

object Overridable {
  // `o` is `new A { ... }` overriding every member of A (checked at compile time). A's
  // constructor needs a Upper, so the base gets one whose `upper` it can't use.
  inline def apply[A](inline o: Upper[A] ?=> A): Overridable[A] = {
    Macros.checkLiteral[A](o, base = true)
    s => o(using baseUpper(s))
  }

  private def baseUpper[A](s: Self[A]): Upper[A] = new Upper[A] {
    def self: A = s.self
    def upper: A = throw new IllegalStateException("the base has no upper")
  }

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
  // `o` is `new A { override ... }`; members it doesn't override fall back to A's `inherited` defaults.
  inline def apply[A](inline o: Upper[A] ?=> A): Override[A] = {
    Macros.checkLiteral[A](o, base = false)
    s => o(using s)
  }

  // `x |+| y` is y extending x: y's `upper` (its super) is x's result. `empty` passes `upper` through.
  // Named so this file can pass it explicitly: here Override[A] is transparent, so implicit
  // search would find cats' instance for plain functions instead.
  given monoid[A]: Monoid[Override[A]] with {
    def empty: Override[A] = _.upper
    def combine(x: Override[A], y: Override[A]): Override[A] = layer(x, y)
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
