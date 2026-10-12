package spike.e

import cats.Monoid

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import scala.NamedTuple.{AnyNamedTuple, Names}
import scala.annotation.{implicitNotFound, targetName}
import scala.compiletime.constValueTuple

@implicitNotFound("self (and module(...)) is only available inside Overridable[...](...) or Override[...](...)")
trait Self[A] {
  def self: A
}

@implicitNotFound("upper is only available in an Override; a base (Overridable) has no layer below")
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

// A module: a field whose type is a dependency trait of its own (`db: DbDeps`), supplied with that
// trait's layer wrapped in `module(...)`: `db = module(DbModule.base(self.url))`. The wrapper makes
// the intent explicit (a raw layer for such a field is an error) and keeps a layer that is itself the
// field's value (a field of type `Override[X]`) distinct from a module. In a base only a complete
// module (BaseModule) makes sense; an override may also give a layer to override the module below
// (Module). `using Self[?]` only scopes the marker to layer literals.
opaque type Module[B] = Override[B]
opaque type BaseModule[B] <: Module[B] = Overridable[B]

// Overload resolution picks the most specific: an Overridable is a BaseModule, an Override a Module.
def module[B](layer: Overridable[B])(using Self[?]): BaseModule[B] = layer
@targetName("moduleOverride")
def module[B](layer: Override[B])(using Self[?]): Module[B] = layer

object Overridable {
  // `o` defines every member of A (`(dep1 = ..., ...)`, `new { def dep1 = ... }`, see Macros).
  inline def apply[A](inline o: Self[A] ?=> Any): Overridable[A] =
    s => Macros.derive[A, Self[A]](o, s, isBase = true)

  // A module's layer runs focused through its field: its `self` is the root's final `db`, so late
  // binding works through the whole tree. In a base there is no layer below, so the module must be
  // complete (see Override.focus for overrides). Cycles that cross the module boundary can hang, see
  // the limits on Cell.
  def focus[A, B](ctx: Self[A], get: A => B, layer: BaseModule[B]): B =
    layer(new Self[B] { def self: B = get(ctx.self) })

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
  // `db = module(Override[DbDeps](...))` is a deep override and `db = module(DbModule.base(...))`
  // replaces the module.
  def focus[A, B](ctx: Upper[A], get: A => B, layer: Module[B]): B =
    layer(new Upper[B] {
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

// Like `class Child extends Parent`: `upper` is `super`, `self` is the final `this`. A chain of n
// overrides nests n deep, so a very long chain (thousands) can overflow the stack. The child's
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
//
// Cycles between threads: when a computation with path P is about to wait on a cell Y that another
// thread is computing, every cell of P is recorded as blocked on Y (`Cell.blockedOn`, under one
// global lock) and the chain of blocked cells is followed from Y. If it leads back to a cell of P,
// the two threads wait on each other: the thread that closes the cycle throws CycleException and the
// waiters of the cells it was computing rethrow it instead of recomputing.
//
// What E does not catch (the path is plain data handed around explicitly, with no hidden state):
// - A cycle through a closure that captured `self` or `upper` in an earlier computation, or through
//   a complete instance (`Overridable[A](new A { ... })`: used as is, not cached, no path), can hang.
//   The main case is a module: `db = module(dbBase(self.a))` captures `self` (the by-name argument)
//   while the `db` cell is computed, with the path at that time. A later `self.db.x` that reaches
//   that argument through `a` is a cycle `a -> db -> a`, but the path it carries knows nothing of
//   `a`, so the thread waits on its own cell. Which one happens depends on the access order: reading
//   `a` first throws `a -> db -> a`, reading `db` first and then `a` hangs.
// - A thread started inside a computation and reading its own computation's cells inherits that
//   computation's path through the captured `self`, so it gets a CycleException instead of waiting.
// - `self` copies have no stable identity: `self eq self` is false and a root's `r.db eq r.db` too.
final class Cell(val name: String) {
  private final class Wait {
    val done = new CountDownLatch(1)
    @volatile var cycle: CycleException | Null = null
  }
  private enum State {
    case Empty
    case Pending(w: Wait)
    case Done(value: Any)
  }
  private val state = new AtomicReference[State](State.Empty)

  // Done is checked before the path, so a `self` kept from a finished computation is no cycle. One
  // rare false positive remains: such a kept `self` used while the member is being recomputed after
  // a failure.
  def get(path: List[Cell])(compute: List[Cell] => Any): Any = state.get match {
    case State.Done(value) => value
    case _ if path.contains(this) => throw new CycleException(path.dropWhile(_ ne this).map(_.name) :+ name)
    case State.Pending(w) =>
      Cell.await(path, this, w.done)
      val cycle = w.cycle
      if cycle != null then throw cycle
      get(path)(compute)
    case State.Empty =>
      val w = new Wait
      if !state.compareAndSet(State.Empty, State.Pending(w)) then get(path)(compute)
      else
        try {
          val value = compute(path :+ this)
          state.set(State.Done(value))
          value
        } catch {
          case e: Throwable =>
            e match {
              case c: CycleException => w.cycle = c // a cycle is no transient failure: waiters would hit it too
              case _ =>
            }
            state.set(State.Empty)
            throw e
        } finally w.done.countDown()
  }
}

object Cell {
  private val lock = new Object
  // Cells whose computation is blocked, and the pending cell each one waits for.
  private val blockedOn = new java.util.HashMap[Cell, Cell]

  // Waits until `target`'s computation is over, unless that would wait on a cycle.
  private def await(path: List[Cell], target: Cell, done: CountDownLatch): Unit = {
    lock.synchronized {
      val seen = new java.util.HashSet[Cell]
      var chain = List.empty[Cell]
      var next: Cell | Null = target
      while next != null && !path.exists(_ eq next) && seen.add(next) do {
        chain = next :: chain
        next = blockedOn.get(next)
      }
      next match {
        case hit: Cell if path.exists(_ eq hit) =>
          throw new CycleException(path.dropWhile(_ ne hit).map(_.name) ++ chain.reverse.map(_.name) :+ hit.name)
        case _ =>
      }
      path.foreach(blockedOn.put(_, target))
    }
    try done.await()
    finally lock.synchronized { path.foreach(blockedOn.remove(_, target)) }
  }
}

final class CycleException(path: List[String]) extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")

// Generated classes implement this: `__knots_at(path)` is a copy carrying the path of the computation it
// is used from, sharing the cells. User code computing a member gets `self`/`upper` tagged that way,
// so the path travels along every `self.x` / `upper.x` without any thread- or scope-bound state.
// (`__knots_` names: they can't collide with the members of the target trait.)
trait Tagged[A] {
  def __knots_at(path: List[Cell]): A
}

object Tag {
  // The state of one generated instance, set right after creation. Kept here: the compiler can't
  // generate setters for vars declared by a macro.
  final class Slot {
    var path: List[Cell] = Nil
    var cells: Array[Cell] = null
  }

  def apply[A](a: A, path: List[Cell]): A = a match {
    case t: Tagged[?] => t.__knots_at(path).asInstanceOf[A]
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
