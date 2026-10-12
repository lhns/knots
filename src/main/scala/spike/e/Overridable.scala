package spike.e

import cats.Monoid

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

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

// Caches one generated member and detects dependency cycles. `path` is the chain of cells the caller
// is in the middle of computing, passed explicitly (see Tagged): asking for a cell already on it is
// a cycle and fails with the path, where a lazy val would wait on itself forever. Other callers
// wait for the value.
final class Cell(val name: String) {
  private enum State {
    case Empty
    case Pending(done: CountDownLatch)
    case Done(value: Any)
  }
  private val state = new AtomicReference[State](State.Empty)

  // Done is checked before the path, so a `self` kept from a finished computation is no cycle. One
  // rare false positive remains: such a kept `self` used while the member is being recomputed after
  // a failure.
  def get(path: List[Cell])(compute: List[Cell] => Any): Any = state.get match {
    case State.Done(value) => value
    case _ if path.contains(this) => throw new CycleException(path.dropWhile(_ ne this).map(_.name) :+ name)
    case State.Pending(done) =>
      done.await()
      get(path)(compute)
    case State.Empty =>
      val done = new CountDownLatch(1)
      if !state.compareAndSet(State.Empty, State.Pending(done)) then get(path)(compute)
      else
        try {
          val value = compute(path :+ this)
          state.set(State.Done(value))
          value
        } catch {
          case e: Throwable =>
            state.set(State.Empty)
            throw e
        } finally done.countDown()
  }
}

final class CycleException(path: List[String]) extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")

// Generated classes implement this: `at(path)` is a copy carrying the path of the computation it is
// used from, sharing the cells. User code computing a member gets `self`/`upper` tagged that way, so
// the path travels along every `self.x` / `upper.x` without any thread- or scope-bound state.
trait Tagged[A] {
  def at(path: List[Cell]): A
}

object Tag {
  // The state of one generated instance, set right after creation. Kept here: the compiler can't
  // generate setters for vars declared by a macro.
  final class Slot {
    var path: List[Cell] = Nil
    var cells: Array[Cell] = null
  }

  def apply[A](a: A, path: List[Cell]): A = a match {
    case t: Tagged[?] => t.at(path).asInstanceOf[A]
    case a => a // a complete instance passed in as is
  }

  // The context user code computing a member on behalf of `path` sees.
  def self[A](s: Self[A], path: List[Cell]): Self[A] = new Self[A] {
    def self: A = Tag(s.self, path)
  }

  def upper[A](s: Upper[A], path: List[Cell]): Upper[A] = new Upper[A] {
    def self: A = Tag(s.self, path)
    def upper: A = Tag(s.upper, path)
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
