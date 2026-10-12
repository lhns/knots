package spike.a

import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class CyclesSuite extends munit.FunSuite {
  override val munitTimeout: Duration = 2.minutes

  type AB = (a: Int, b: Int)
  type ABC = (a: Int, b: Int, c: Int)
  type Wide = (f0: Int, f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int)

  test("a field that needs itself names the cycle") {
    val v = Overridable.build(Overridable[AB]((a = self.b, b = self.a)))
    val e = intercept[CycleException](v.a)
    assertEquals(e.getMessage, "cyclic dependency: a (base) -> b (base) -> a (base)")
  }

  test("a cycle between fields of one override names the fields") {
    val v = Overridable.build(
      deps,
      Override[Deps]((dep1 = Dep(self.dep2.name), dep2 = Dep(self.dep1.name))))
    val e = intercept[CycleException](v.dep1)
    assertEquals(e.getMessage, "cyclic dependency: dep1 (layer 1) -> dep2 (layer 1) -> dep1 (layer 1)")
  }

  test("a field of an override asking for itself names it") {
    val v = Overridable.build(deps, Override[Deps]((dep2 = Dep(self.dep2.name))))
    assertEquals(intercept[CycleException](v.dep2).getMessage, "cyclic dependency: dep2 (layer 1) -> dep2 (layer 1)")
  }

  test("a cycle across layers shows the layer labels") {
    val v = Overridable.build(
      deps,
      Override[Deps]((dep1 = Dep(self.dep3.name))),
      Override[Deps]((dep3 = Dep(upper.dep3.name + self.dep2.name))),
      Override[Deps]((dep2 = Dep(self.dep1.name))))
    val e = intercept[CycleException](v.dep1)
    assert(e.getMessage.contains("dep1 (layer 1)") && e.getMessage.contains("dep2 (layer 3)"), clue(e.getMessage))
  }

  test("a cycle is reported again on the next access") {
    val v = Overridable.build(Overridable[AB]((a = self.b, b = self.a)))
    intercept[CycleException](v.a)
    intercept[CycleException](v.a)
  }

  // Both threads hold one field and ask for the other: without detection they would deadlock.
  private def twoThreadRound(round: Int): List[String] = {
    val barrier = CyclicBarrier(2)
    val gate = AtomicInteger()
    def meet(): Unit = if gate.getAndIncrement() < 2 then barrier.await(): Unit
    val v = Overridable.build(Overridable[AB]((a = { meet(); self.b }, b = { meet(); self.a })))
    val outcomes = ConcurrentLinkedQueue[String]()
    val threads = List[() => Int](() => v.a, () => v.b).map { read =>
      val body: Runnable = () => outcomes.add(try s"ok ${read()}" catch case _: CycleException => "cycle"): Unit
      if round % 2 == 0 then Thread.ofVirtual().unstarted(body)
      else Thread.ofPlatform().daemon().unstarted(body)
    }
    threads.foreach(_.start())
    threads.foreach(_.join(5000))
    if threads.exists(_.isAlive) then List("hang") else outcomes.toArray.toList.map(_.toString)
  }

  test("a two-thread cycle throws in both threads, platform and virtual, over many rounds") {
    for round <- 1 to 50 do assertEquals(twoThreadRound(round), List("cycle", "cycle"), s"round $round")
  }

  test("a three-thread cycle throws in all threads over many rounds") {
    for round <- 1 to 50 do {
      val barrier = CyclicBarrier(3)
      val gate = AtomicInteger()
      def meet(): Unit = if gate.getAndIncrement() < 3 then barrier.await(): Unit
      val v = Overridable.build(Overridable[ABC]((
        a = { meet(); self.b },
        b = { meet(); self.c },
        c = { meet(); self.a })))
      val outcomes = ConcurrentLinkedQueue[String]()
      val threads = List[() => Int](() => v.a, () => v.b, () => v.c).map { read =>
        Thread.ofPlatform().daemon().unstarted(() => outcomes.add(try s"ok ${read()}" catch case _: CycleException => "cycle"): Unit)
      }
      threads.foreach(_.start())
      threads.foreach(_.join(5000))
      assert(!threads.exists(_.isAlive), s"round $round hangs")
      assertEquals(outcomes.toArray.toList, List("cycle", "cycle", "cycle"), s"round $round")
    }
  }

  test("1000 virtual threads reading a dependency chain see no false cycle and compute each field once") {
    val f0Computed = AtomicInteger()
    val falseCycles = AtomicInteger()
    for _ <- 1 to 5 do {
      f0Computed.set(0)
      val v = Overridable.build(Overridable[Wide]((
        f0 = { f0Computed.incrementAndGet(); Thread.sleep(1); 0 },
        f1 = self.f0 + 1, f2 = self.f1 + 1, f3 = self.f2 + 1,
        f4 = self.f0 + 10, f5 = self.f4 + self.f3, f6 = self.f5 + 1, f7 = self.f6 + self.f2)))
      val executor = Executors.newVirtualThreadPerTaskExecutor()
      try {
        val reads = (0 until 1000).map { i =>
          executor.submit { () =>
            try i % 8 match {
              case 0 => v.f0 case 1 => v.f1 case 2 => v.f2 case 3 => v.f3
              case 4 => v.f4 case 5 => v.f5 case 6 => v.f6 case _ => v.f7
            } catch case _: CycleException => { falseCycles.incrementAndGet(); -1 }
          }
        }
        assertEquals(reads.map(_.get(20, TimeUnit.SECONDS)).toSet, Set(0, 1, 2, 3, 10, 13, 14, 16))
      } finally executor.close()
      assertEquals(f0Computed.get, 1)
    }
    assertEquals(falseCycles.get, 0)
  }
}
