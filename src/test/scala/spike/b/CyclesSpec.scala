package spike.b

import java.util.concurrent.CyclicBarrier
import java.util.concurrent.atomic.AtomicInteger

import Fx.*

class CyclesSpec extends munit.FunSuite {
  trait AB { def a: Int; def b: Int }
  trait ABC { def a: Int; def b: Int; def c: Int }
  trait Wide { def f0: Int; def f1: Int; def f2: Int; def f3: Int; def f4: Int; def f5: Int; def f6: Int; def f7: Int }

  test("a cycle within one layer throws a CycleException with the path") {
    val d = Overridable.build(base, Override[Deps](new {
      lazy val dep1 = Dep(self.dep2.name)
      lazy val dep2 = Dep(self.dep1.name)
    }))
    val e = intercept[CycleException](d.dep1)
    assertEquals(e.path, List("dep1", "dep2", "dep1"))
  }

  test("a member depending on itself is a cycle") {
    val d = Overridable.build(base, Override[Deps](new { def dep1 = Dep(self.dep1.name) }))
    assertEquals(intercept[CycleException](d.dep1).path, List("dep1", "dep1"))
  }

  test("a cycle across layers throws") {
    val d = Overridable.build(base,
      Override[Deps](new { lazy val dep1 = Dep(self.dep3.name) }),
      Override[Deps](new { lazy val dep3 = Dep(self.dep1.name) }))
    assertEquals(intercept[CycleException](d.dep1).path, List("dep1", "dep3", "dep1"))
  }

  test("a cycle is reported again by the next caller") {
    val d = Overridable.build(base, Override[Deps](new { def dep1 = Dep(self.dep1.name) }))
    intercept[CycleException](d.dep1)
    intercept[CycleException](d.dep1)
  }

  test("upper is not a cycle") {
    val d = Overridable.build(base, Override[Deps](new { lazy val dep2 = Dep("w(" + upper.dep2.name + ")") }))
    assertEquals(d.dep2, Dep("w(base2)"))
  }

  test("the same member used twice in a computation is not a cycle") {
    val d = Overridable.build(base, Override[Deps](new { def dep3 = Dep(self.dep2.name + self.dep2.name) }))
    assertEquals(d.dep3, Dep("base2base2"))
  }

  test("a two-thread cycle throws in both threads, over several rounds") {
    for round <- 1 to 30 do {
      val barrier = new CyclicBarrier(2)
      val gate = new AtomicInteger
      val x = Overridable.build(Overridable[AB](new {
        def a = { if gate.getAndIncrement() < 2 then barrier.await(); self.b }
        def b = { if gate.getAndIncrement() < 2 then barrier.await(); self.a }
      }))
      val results = inThreads(List(() => x.a, () => x.b), virtual = round % 2 == 0)
      assert(results.forall(_.left.exists(_.isInstanceOf[CycleException])), s"round $round: $results")
    }
  }

  test("a three-thread cycle throws in every thread, over several rounds") {
    for round <- 1 to 30 do {
      val barrier = new CyclicBarrier(3)
      val gate = new AtomicInteger
      val x = Overridable.build(Overridable[ABC](new {
        def a = { if gate.getAndIncrement() < 3 then barrier.await(); self.b }
        def b = { if gate.getAndIncrement() < 3 then barrier.await(); self.c }
        def c = { if gate.getAndIncrement() < 3 then barrier.await(); self.a }
      }))
      val results = inThreads(List(() => x.a, () => x.b, () => x.c), virtual = round % 2 == 0)
      assert(results.forall(_.left.exists(_.isInstanceOf[CycleException])), s"round $round: $results")
    }
  }

  test("threads waiting on each other without a cycle just wait") {
    val barrier = new CyclicBarrier(2)
    val x = Overridable.build(Overridable[AB](new {
      def a = { barrier.await(); 1 }
      def b = { barrier.await(); self.a + 1 }
    }))
    // Thread 2 computes b and waits for a, which thread 1 computes: no cycle.
    val results = inThreads(List(() => x.a, () => x.b))
    assertEquals(results, List(Right(1), Right(2)))
  }

  test("1000 virtual threads reading different fields see no false cycles") {
    val falseCycles = new AtomicInteger
    val computes = new AtomicInteger
    for _ <- 1 to 10 do {
      val x = Overridable.build(Overridable[Wide](new {
        def f0 = { computes.incrementAndGet(); Thread.sleep(1); 0 }
        def f1 = self.f0 + 1
        def f2 = self.f1 + 1
        def f3 = self.f2 + 1
        def f4 = self.f0 + 10
        def f5 = self.f4 + self.f3
        def f6 = self.f5 + 1
        def f7 = self.f6 + self.f2
      }))
      val values = withVirtualExecutor { ex =>
        val fs = (0 until 1000).map { i =>
          ex.submit(() =>
            try i % 8 match {
              case 0 => x.f0
              case 1 => x.f1
              case 2 => x.f2
              case 3 => x.f3
              case 4 => x.f4
              case 5 => x.f5
              case 6 => x.f6
              case _ => x.f7
            } catch { case _: CycleException => falseCycles.incrementAndGet(); -1 })
        }
        fs.map(_.get).toSet
      }
      assertEquals(values, Set(0, 1, 2, 3, 10, 13, 14, 16))
    }
    assertEquals(falseCycles.get, 0)
    assertEquals(computes.get, 10)
  }
}
