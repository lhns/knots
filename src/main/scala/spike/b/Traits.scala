package spike.b

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
  def apply[A](f: Self[A] ?=> A): Overridable[A] = s => f(using s)

  // Needs at least one override so a single argument always means "create" above;
  // overload resolution can't tell a context-function literal from a value otherwise.
  def apply[A](base: Overridable[A], ovr: Override[A], more: Override[A]*): Overridable[A] =
    (ovr +: more).foldLeft(base)(layer)

  private def layer[A](lower: Overridable[A], ovr: Override[A]): Overridable[A] =
    s => ovr(new Super[A] {
      lazy val upper: A = lower(s)
      def self: A = s.self
    })

  def build[A](base: Overridable[A], overrides: Override[A]*): A = {
    var r: A | Null = null
    r = overrides.foldLeft(base)(layer)(new Self[A] {
      def self: A = r match {
        case null => throw new IllegalStateException("cannot evaluate untied knot")
        case a: A @unchecked => a
      }
    })
    r.asInstanceOf[A]
  }
}

object Override {
  def apply[A](f: Super[A] ?=> A): Override[A] = s => f(using s)

  // Stacked in order: each override's `upper` is the result of the ones before it.
  def apply[A](first: Override[A], second: Override[A], more: Override[A]*): Override[A] =
    (first +: second +: more).foldLeft[Override[A]](_.upper)((lower, ovr) =>
      s => ovr(new Super[A] {
        lazy val upper: A = lower(s)
        def self: A = s.self
      })
    )
}
