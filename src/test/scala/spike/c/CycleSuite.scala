package spike.c.tests

import spike.c.*

import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

object CycFixtures {
  trait AB(using Upper[AB]) { def a: Int = inherited; def b: Int = inherited }
  trait ABC(using Upper[ABC]) { def a: Int = inherited; def b: Int = inherited; def c: Int = inherited }
  trait Wide(using Upper[Wide]) {
    def f0: Int = inherited; def f1: Int = inherited; def f2: Int = inherited; def f3: Int = inherited
    def f4: Int = inherited; def f5: Int = inherited; def f6: Int = inherited; def f7: Int = inherited
  }
}

class CycleSuite extends munit.FunSuite {
  import CycFixtures.*

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val rounds = 30

  test("a 2-cycle on one thread throws CycleException with the path") {
    val x = Overridable.build(Overridable[ABC](new ABC {
      override lazy val a = self.b + 1
      override lazy val b = self.a + 1
      override lazy val c = 0
    }))
    val e = intercept[CycleException](x.a)
    assertEquals(e.path, List("a", "b", "a"))
  }

  test("a 3-cycle through defs throws CycleException") {
    val x = Overridable.build(Overridable[ABC](new ABC {
      override def a = self.b
      override def b = self.c
      override def c = self.a
    }))
    val e = intercept[CycleException](x.b)
    assertEquals(e.path, List("b", "c", "a", "b"))
  }

  test("a cycle across layers throws CycleException") {
    val base = Overridable[ABC](new ABC {
      override lazy val a = self.b
      override lazy val b = 1
      override lazy val c = 2
    })
    val o = Override[ABC](new ABC { override lazy val b = self.a })
    intercept[CycleException](Overridable.build(base, o).a)
  }

  test("a cycle between strict vals throws CycleException") {
    val x = Overridable.build(Overridable[ABC](new ABC {
      override val a = self.b + 1
      override val b = self.a
      override val c = 0
    }))
    intercept[CycleException](x.a)
  }

  test("a CycleException is an IllegalStateException with a readable message") {
    val x = Overridable.build(Overridable[AB](new AB { override lazy val a = self.a; override lazy val b = 0 }))
    val e = intercept[IllegalStateException](x.a)
    assert(e.getMessage.startsWith("cyclic dependency"), e.getMessage)
  }

  // Runs one thread per task; each result is "cycle" or "ok n". Fails on a hang.
  private def runAll(virtual: Boolean, fs: List[() => Int]): List[String] = {
    val res = new ConcurrentLinkedQueue[String]
    val ts = fs.map { f =>
      val body: Runnable = () => { res.add(try s"ok ${f()}" catch { case _: CycleException => "cycle" }); () }
      val t = if virtual then Thread.ofVirtual().unstarted(body) else { val t = new Thread(body); t.setDaemon(true); t }
      t.start(); t
    }
    ts.foreach(_.join(5000))
    assert(!ts.exists(_.isAlive), s"hang, results so far: $res")
    res.asScala.toList
  }

  test("a 2-thread cycle throws in both threads, over several rounds, platform and virtual threads") {
    for round <- 1 to rounds do {
      val barrier = new CyclicBarrier(2)
      val gate = new AtomicInteger
      val x = Overridable.build(Overridable[AB](new AB {
        override lazy val a = { if gate.getAndIncrement() < 2 then barrier.await(); self.b }
        override lazy val b = { if gate.getAndIncrement() < 2 then barrier.await(); self.a }
      }))
      assertEquals(runAll(round % 2 == 0, List(() => x.a, () => x.b)), List("cycle", "cycle"), s"round $round")
    }
  }

  test("a 3-thread cycle throws in all threads, over several rounds") {
    for round <- 1 to rounds do {
      val barrier = new CyclicBarrier(3)
      val gate = new AtomicInteger
      val x = Overridable.build(Overridable[ABC](new ABC {
        override lazy val a = { if gate.getAndIncrement() < 3 then barrier.await(); self.b }
        override lazy val b = { if gate.getAndIncrement() < 3 then barrier.await(); self.c }
        override lazy val c = { if gate.getAndIncrement() < 3 then barrier.await(); self.a }
      }))
      assertEquals(runAll(false, List(() => x.a, () => x.b, () => x.c)), List("cycle", "cycle", "cycle"), s"round $round")
    }
  }

  test("a 2-thread cycle across layers throws in both threads, over several rounds") {
    for round <- 1 to rounds do {
      val barrier = new CyclicBarrier(2)
      val gate = new AtomicInteger
      val base = Overridable[AB](new AB {
        override lazy val a = { if gate.getAndIncrement() < 2 then barrier.await(); self.b }
        override lazy val b = 1
      })
      val o = Override[AB](new AB {
        override lazy val b = { if gate.getAndIncrement() < 2 then barrier.await(); self.a }
      })
      val x = Overridable.build(base, o)
      assertEquals(runAll(false, List(() => x.a, () => x.b)), List("cycle", "cycle"), s"round $round")
    }
  }

  test("1000 virtual threads on a diamond-shaped graph see no false cycles and compute each member once") {
    for round <- 1 to 10 do {
      val computes = new AtomicInteger
      val x = Overridable.build(Overridable[Wide](new Wide {
        override lazy val f0 = { computes.incrementAndGet(); Thread.sleep(1); 0 }
        override lazy val f1 = self.f0 + 1
        override lazy val f2 = self.f1 + 1
        override lazy val f3 = self.f2 + 1
        override lazy val f4 = self.f0 + 10
        override lazy val f5 = self.f4 + self.f3
        override lazy val f6 = self.f5 + 1
        override lazy val f7 = self.f6 + self.f2
      }))
      val ex = Executors.newVirtualThreadPerTaskExecutor()
      try {
        val fs = (0 until 1000).map { i =>
          ex.submit[Int](() => i % 8 match {
            case 0 => x.f0 case 1 => x.f1 case 2 => x.f2 case 3 => x.f3
            case 4 => x.f4 case 5 => x.f5 case 6 => x.f6 case _ => x.f7
          })
        }
        val vs = fs.map(_.get(20, TimeUnit.SECONDS)).toSet
        assertEquals(vs, Set(0, 1, 2, 3, 10, 13, 14, 16), s"round $round")
        assertEquals(computes.get, 1, s"round $round")
      } finally ex.shutdownNow()
    }
  }

  test("concurrent callers of one member compute it once") {
    val n = new AtomicInteger
    val x = Overridable.build(Overridable[AB](new AB {
      override lazy val a = { n.incrementAndGet(); Thread.sleep(50); 7 }
      override lazy val b = 0
    }))
    val ex = Executors.newFixedThreadPool(8)
    try {
      val fs = (1 to 8).map(_ => ex.submit[Int](() => x.a))
      assertEquals(fs.map(_.get(10, TimeUnit.SECONDS)).toSet, Set(7))
      assertEquals(n.get, 1)
    } finally ex.shutdownNow()
  }
}
