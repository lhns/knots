package spike.e

import java.util.concurrent.{CyclicBarrier, ExecutionException, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import Fx.*

object CyclesFx {
  trait Many { def f0: Int; def f1: Int; def f2: Int; def f3: Int; def f4: Int; def f5: Int; def f6: Int; def f7: Int; def f8: Int; def f9: Int }
}

class CyclesSuite extends munit.FunSuite {
  import CyclesFx.*

  override val munitTimeout: Duration = 2.minutes

  private def cycleMessage(body: => Any): String = timed()(intercept[CycleException](body)).getMessage

  // --- single thread

  test("a field that reads itself") {
    val r = Overridable.build(Overridable[AB]((a = self.a, b = D("b"))))
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> a")
  }

  test("two fields reading each other") {
    val r = Overridable.build(Overridable[AB]((a = self.b, b = self.a)))
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> b -> a")
  }

  test("three fields in a ring") {
    val r = Overridable.build(Overridable[ABC]((a = self.b, b = self.c, c = self.a)))
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> b -> c -> a")
  }

  test("a cycle is reported again on a second read") {
    val r = Overridable.build(Overridable[AB]((a = self.b, b = self.a)))
    cycleMessage(r.a)
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> b -> a")
  }

  test("a cycle across layers through upper") {
    val base = Overridable[AB]((a = D("a"), b = self.a))
    val r = Overridable.build(base, Override[AB]((a = upper.b)))
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> b -> a")
  }

  test("a cycle across layers between object literals") {
    val r = Overridable.build(
      Overridable[ABC]((a = D("a"), b = D("b"), c = D("c"))),
      Override[ABC](new { def a = self.c }),
      Override[ABC](new { def c = self.a }),
    )
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> c -> a")
  }

  test("the same field overridden twice and reading upper from itself") {
    val base = Overridable[AB]((a = D("a"), b = self.b))
    val r = Overridable.build(base, Override[AB]((b = upper.b)), Override[AB]((b = upper.b)))
    assertEquals(cycleMessage(r.b), "cyclic dependency: b -> b -> b -> b")
  }

  test("a cycle in a view") {
    val r = Overridable.build(Overridable[View[(a: Int, b: Int)]]((a = self.selectDynamic("b").asInstanceOf[Int], b = self.a)))
    assertEquals(cycleMessage(r.toTuple), "cyclic dependency: a -> b -> a")
  }

  test("a cycle through an object-literal override") {
    val base = Overridable[AB]((a = D("a"), b = D("b")))
    val r = Overridable.build(base, Override[AB](new { def a = D(self.b.n); def b = D(self.a.n) }))
    assertEquals(cycleMessage(r.a), "cyclic dependency: a -> b -> a")
  }

  // --- retry

  test("a field that threw is computed again on the next read") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[AB]((a = { if n.incrementAndGet() == 1 then throw new RuntimeException("boom"); D("a") }, b = self.a)))
    assertEquals(intercept[RuntimeException](r.b).getMessage, "boom")
    assertEquals(r.b, D("a"))
    assertEquals(n.get, 2)
  }

  // --- several threads

  private def twoThreadCycle(): (Either[Throwable, D], Either[Throwable, D]) = {
    val bar = new CyclicBarrier(2)
    val r = Overridable.build(Overridable[AB]((a = { bar.await(10, TimeUnit.SECONDS); self.b }, b = { bar.await(10, TimeUnit.SECONDS); self.a })))
    val exec = Executors.newFixedThreadPool(2)
    try {
      val fa = exec.submit(() => scala.util.Try(r.a).toEither)
      val fb = exec.submit(() => scala.util.Try(r.b).toEither)
      (fa.get(20, TimeUnit.SECONDS), fb.get(20, TimeUnit.SECONDS))
    } finally exec.shutdownNow()
  }

  test("a two-thread cycle throws in both threads, over many rounds") {
    for round <- 1 to 30 do {
      val (x, y) = twoThreadCycle()
      assert(x.left.exists(_.isInstanceOf[CycleException]), clue((round, x)))
      assert(y.left.exists(_.isInstanceOf[CycleException]), clue((round, y)))
    }
  }

  test("a three-thread cycle throws in every thread, over several rounds") {
    for round <- 1 to 20 do {
      val bar = new CyclicBarrier(3)
      val r = Overridable.build(Overridable[ABC]((
        a = { bar.await(10, TimeUnit.SECONDS); self.b },
        b = { bar.await(10, TimeUnit.SECONDS); self.c },
        c = { bar.await(10, TimeUnit.SECONDS); self.a },
      )))
      val exec = Executors.newFixedThreadPool(3)
      try {
        val fs = List(exec.submit(() => scala.util.Try(r.a).toEither), exec.submit(() => scala.util.Try(r.b).toEither), exec.submit(() => scala.util.Try(r.c).toEither))
        fs.map(_.get(20, TimeUnit.SECONDS)).foreach(res => assert(res.left.exists(_.isInstanceOf[CycleException]), clue((round, res))))
      } finally exec.shutdownNow()
    }
  }

  test("a failed cycle does not poison the instance: a field outside the cycle still works") {
    val r = Overridable.build(Overridable[ABC]((a = self.b, b = self.a, c = D("c"))))
    cycleMessage(r.a)
    assertEquals(r.c, D("c"))
  }

  test("1000 virtual threads over different fields: no false cycles, each field computed once") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[Many]((
      f0 = { n.incrementAndGet(); Thread.sleep(5); 0 }, f1 = self.f0 + 1, f2 = self.f1 + 1, f3 = { Thread.sleep(2); 3 }, f4 = self.f3 + self.f2,
      f5 = self.f4 + 1, f6 = 6, f7 = self.f6 + self.f0, f8 = self.f5 + self.f7, f9 = self.f8 + 1,
    )))
    val exec = Executors.newVirtualThreadPerTaskExecutor()
    try {
      val futures = (0 until 1000).map { i =>
        exec.submit(() => i % 10 match {
          case 0 => r.f0; case 1 => r.f1; case 2 => r.f2; case 3 => r.f3; case 4 => r.f4
          case 5 => r.f5; case 6 => r.f6; case 7 => r.f7; case 8 => r.f8; case _ => r.f9
        })
      }
      val values = futures.map(_.get(30, TimeUnit.SECONDS))
      assertEquals(values.count(_ < 0), 0)
    } finally exec.shutdown()
    assertEquals(n.get, 1)
    assertEquals(r.f9, 13)
  }

  test("20 rounds of 1000 virtual threads over a dependency diamond: no errors") {
    for _ <- 1 to 20 do {
      val r = Overridable.build(Overridable[ABC]((a = { Thread.sleep(1); D("a") }, b = D("b" + self.a.n), c = D("c" + self.a.n + self.b.n))))
      val exec = Executors.newVirtualThreadPerTaskExecutor()
      try {
        val futures = (0 until 1000).map(i => exec.submit(() => if i % 3 == 0 then r.c else if i % 3 == 1 then r.b else r.a))
        futures.foreach(_.get(30, TimeUnit.SECONDS))
        assertEquals(r.c, D("cab" + "a"))
      } finally exec.shutdown()
    }
  }

  test("1000 virtual threads racing the first read compute the field once") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[AB]((a = { n.incrementAndGet(); Thread.sleep(50); D("a") }, b = self.a)))
    val exec = Executors.newVirtualThreadPerTaskExecutor()
    try (0 until 1000).map(i => exec.submit(() => if i % 2 == 0 then r.a else r.b)).foreach(_.get(30, TimeUnit.SECONDS))
    finally exec.shutdown()
    assertEquals(n.get, 1)
  }

  test("64 platform threads racing the first read compute the field once") {
    val n = new AtomicInteger
    val r = Overridable.build(Overridable[AB]((a = { n.incrementAndGet(); Thread.sleep(50); D("a") }, b = self.a)))
    val exec = Executors.newFixedThreadPool(64)
    try (1 to 64).map(i => exec.submit(() => if i % 2 == 0 then r.a else r.b)).foreach(_.get(30, TimeUnit.SECONDS))
    finally exec.shutdown()
    assertEquals(n.get, 1)
  }

  test("concurrent readers of one field and its dependency, with overrides, compute each once") {
    for _ <- 1 to 20 do {
      val inits = new AtomicInteger
      val base = Overridable[AB]((a = D("a"), b = D("b")))
      val d = Overridable.build(base, Override[AB]((
        b = { inits.incrementAndGet(); Thread.sleep(5); D("b(" + self.a.n + ")") },
        a = { inits.incrementAndGet(); Thread.sleep(5); D("a") },
      )))
      val start = new java.util.concurrent.CountDownLatch(1)
      val exec = Executors.newVirtualThreadPerTaskExecutor()
      try {
        val results = (1 to 1000).map(_ => exec.submit(() => { start.await(); (d.b, d.a) }))
        start.countDown()
        assertEquals(results.map(_.get(30, TimeUnit.SECONDS)).distinct.size, 1)
      } finally exec.shutdown()
      assertEquals(inits.get, 2)
    }
  }
}
