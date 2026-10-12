package spike.a

import scala.collection.mutable

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
      if path.contains(this) then {
        val cycle = path.dropWhile(_ ne this)
        throw new CycleException(cycle.map(_.name) :+ name)
      }
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

final class CycleException(val path: List[String])
    extends IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")
