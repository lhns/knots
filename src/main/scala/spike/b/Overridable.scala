package spike.b

import cats.Monoid

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

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
  // `o` defines every abstract member of A: `new { def dep1 = ... }`, see Macros.
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
  // `o` defines only the overridden members; the rest come from `upper`, see Macros.
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

// The members the current computation is in the middle of, outermost first. A ScopedValue is bound
// for the extent of a call and seen by everything it calls, without depending on thread identity.
private val computing: ScopedValue[List[Cell]] = ScopedValue.newInstance()

// Caches one generated member and detects dependency cycles: asking for a member that the current
// computation is already in the middle of fails with the path, where a lazy val would wait on itself
// forever. Other callers wait for the value.
final class Cell(val name: String) {
  private enum State {
    case Empty
    case Pending(done: CountDownLatch)
    case Done(value: Any)
  }
  private val state = new AtomicReference[State](State.Empty)

  def get(compute: => Any): Any = {
    val path = if computing.isBound then computing.get else Nil
    if path.contains(this) then throw new CycleException(path.dropWhile(_ ne this).map(_.name) :+ name)
    state.get match {
      case State.Done(value) => value
      case State.Pending(done) =>
        done.await()
        get(compute)
      case State.Empty =>
        val done = new CountDownLatch(1)
        if !state.compareAndSet(State.Empty, State.Pending(done)) then get(compute)
        else
          try {
            val value = ScopedValue.where(computing, path :+ this).call(() => compute)
            state.set(State.Done(value))
            value
          } catch {
            case e: Throwable =>
              state.set(State.Empty)
              throw e
          } finally done.countDown()
    }
  }
}

final class CycleException(path: List[String]) extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")
