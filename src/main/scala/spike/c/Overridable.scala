package spike.c

import cats.Monoid

import scala.annotation.{implicitNotFound, targetName}
import scala.collection.mutable

@implicitNotFound("self is only available inside an Overridable[...](new ...) or Override[...](new ...) literal")
trait Self[A] {
  def self: A
}

@implicitNotFound("upper is only available in an Override; a base (Overridable) has no layer below. A trait with a `using Upper` parameter is instantiated inside an Overridable[...](new ...) or Override[...](new ...) literal")
trait Upper[A] extends Self[A] {
  def upper: A
}

// Plain defs, not inline: the literal check recognizes calls to `upper` in the tree.
def self[A](using s: Self[A]): A = s.self

def upper[A](using s: Upper[A]): A = s.upper

// Opaque (over plain functions: context function types can't be opaque) so a value isn't
// auto-applied like a bare context function: `val o = Override(...)` needs no type ascription.
opaque type Override[A] = Upper[A] => A

// A base is an override that doesn't need `upper`, so it can stand wherever an Override can,
// e.g. as a child of another base, replacing it completely.
opaque type Overridable[A] <: Override[A] = Self[A] => A

// The user's object literal `new A { override lazy val x = ... }` is kept as it is, but it never
// serves as the layer: a macro generates a subclass of A around it whose overridden members read
// the literal's members through a Cell (computed once, cycles detected, see Cell and Macros.derive).
// The literal's own lazy vals are therefore only ever entered by one thread at a time.
// This needs JDK 25+ (ScopedValue), like spike D.
// Marks a member as a module: `override lazy val db = module(DbModule.base(self.config.dbUrl))` in a
// literal. It has the member's type so that the literal type-checks; the macro replaces the member
// with the layer run focused through the field (see Overridable.focus), so these bodies only run if
// the macro didn't see the call. A base needs a complete module (Overridable); an override may give
// either: an Override is a deep override, an Overridable replaces the module. Both overloads erase to
// the same signature, hence the @targetName.
private val moduleMisuse = "module(...) must be the whole body of an overriding member in an Overridable/Override literal"

@targetName("baseModule")
def module[B](layer: Overridable[B])(using Self[?]): B = throw new IllegalStateException(moduleMisuse)

def module[B](layer: Override[B])(using Self[?]): B = throw new IllegalStateException(moduleMisuse)

object Overridable {
  // The module `get(self)` of the enclosing layer, defined by `layer`: its `self` is the root's final
  // `get(self)`, so late binding works through the whole tree. A base has no layer below, so its
  // module must be complete (see Override.focus for overrides).
  def focus[A, B](ctx: Self[A], get: A => B, layer: Overridable[B]): B =
    layer(new Self[B] { def self: B = get(ctx.self) })

  // `o` is `new A { ... }` overriding every member of A (checked at compile time). A's
  // constructor needs a Upper, so the base gets one whose `upper` it can't use.
  inline def apply[A](inline o: Upper[A] ?=> A): Overridable[A] =
    s => {
      val u = baseUpper(s)
      Macros.derive[A](o, u, true)
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
  // A module in an override (see Overridable.focus): its `upper` is the module of the layer below, so
  // an Override is a deep override and an Overridable replaces the module.
  def focus[A, B](ctx: Upper[A], get: A => B, layer: Override[B]): B =
    layer(new Upper[B] {
      def self: B = get(ctx.self)
      def upper: B = get(ctx.upper)
    })

  // `o` is `new A { override ... }`; members it doesn't override fall back to A's `inherited` defaults.
  inline def apply[A](inline o: Upper[A] ?=> A): Override[A] =
    s => Macros.derive[A](o, s, false)

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

// The members the current computation is in the middle of, outermost first. A ScopedValue is bound
// for the extent of a call and seen by everything it calls, without depending on thread identity.
private val computing: ScopedValue[List[Cell]] = ScopedValue.newInstance()

private def current: List[Cell] = if computing.isBound then computing.get else Nil

// Caches one generated member and detects dependency cycles, within a computation and across threads.
// The value is computed once; other callers wait; after an exception the next caller retries.
//
// A cell is computed with `computing` extended by itself, so asking for a member the current
// computation is already in the middle of fails with the path (the chain check in `get`). Across
// threads that is not enough: thread 1 computing a waits for b, which thread 2 is computing while
// waiting for a. So a thread about to wait for a pending cell first records, for every cell of its
// chain, that the chain is blocked on it, then follows the records from that cell; reaching its own
// chain means that waiting would deadlock, and it throws instead. Recording and checking happen under
// one lock, so when two threads close a cycle at the same time one of them sees the other's records.
// A waiter whose cell failed with a CycleException throws it too instead of retrying: the cycle runs
// through the waiter's own chain, so a retry would only find it again (after repeating its effects).
//
// Not covered: blocking on something that is not a cell inside a computation
// (`executor.submit(..).get()`, `IO(..).unsafeRunSync()`) while the other side needs the computing
// cell can still deadlock, like a lazy val. StructuredTaskScope forks inherit the ScopedValue, so
// their waits are part of the chain and are detected.
final class Cell(val name: String, compute: () => Any) {
  @volatile private var value: Any = Cell.Unset
  // The running computation, if any; guarded by Cell.lock.
  private var running: Cell.Attempt | Null = null

  def get: Any = {
    val v = value
    if v.asInstanceOf[AnyRef] ne Cell.Unset then v
    else {
      val path = current
      if path.contains(this) then throw new CycleException(path.dropWhile(_ ne this).map(_.name) :+ name)
      val mine = claim(path)
      if mine == null then value
      else
        try {
          val v = ScopedValue.where(computing, path :+ this).call(() => compute())
          Cell.lock.synchronized { value = v; running = null; Cell.lock.notifyAll() }
          v
        } catch {
          case e: Throwable =>
            Cell.lock.synchronized { mine.failure = e; running = null; Cell.lock.notifyAll() }
            throw e
        }
    }
  }

  // Returns the attempt this caller must run, or null when the value is there by now.
  private def claim(path: List[Cell]): Cell.Attempt | Null = Cell.lock.synchronized {
    var claimed: Cell.Attempt | Null = null
    var finished = false
    while !finished do {
      val other = running
      if value.asInstanceOf[AnyRef] ne Cell.Unset then finished = true
      else if other == null then { claimed = new Cell.Attempt; running = claimed; finished = true }
      else {
        Cell.await(path, this)
        other.failure match {
          case c: CycleException => throw new CycleException(c.path)
          case _ =>
        }
      }
    }
    claimed
  }
}

private object Cell {
  val Unset: AnyRef = new Object

  final class Attempt { @volatile var failure: Throwable | Null = null }

  val lock = new Object
  // chain cell -> the cells its chain is waiting for (a list: forks of one chain wait separately).
  private val blockedOn = mutable.HashMap.empty[Cell, List[Cell]]

  // Waits (under `lock`) for the pending `target` to finish; throws if that would be a deadlock.
  def await(path: List[Cell], target: Cell): Unit = {
    if path.nonEmpty then {
      path.foreach(c => blockedOn.update(c, target :: blockedOn.getOrElse(c, Nil)))
      cycleThrough(path, target) match {
        case Some(cycle) => unblock(path, target); throw new CycleException(cycle.map(_.name))
        case None =>
      }
    }
    try lock.wait() finally unblock(path, target)
  }

  private def unblock(path: List[Cell], target: Cell): Unit =
    path.foreach { c =>
      val rest = blockedOn.getOrElse(c, Nil).diff(List(target))
      if rest.isEmpty then blockedOn.remove(c) else blockedOn.update(c, rest)
    }

  // The cycle that waiting for `target` would close, from the first repeated cell of `path`.
  private def cycleThrough(path: List[Cell], target: Cell): Option[List[Cell]] = {
    val seen = mutable.HashSet.empty[Cell]
    def walk(cell: Cell, route: List[Cell]): Option[List[Cell]] =
      if !seen.add(cell) then None
      else {
        val route2 = cell :: route
        blockedOn.getOrElse(cell, Nil).iterator.flatMap { next =>
          path.find(_ eq next) match {
            case Some(hit) => Some(path.dropWhile(_ ne hit) ++ route2.reverse :+ hit)
            case None => walk(next, route2)
          }
        }.nextOption()
      }
    walk(target, Nil)
  }
}

final class CycleException(val path: List[String]) extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")
