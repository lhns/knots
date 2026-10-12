package spike.d

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, ExecutorService, TimeUnit}
import scala.concurrent.duration.*

class CycleSuite extends munit.FunSuite {
  import CycleSuite.*

  override val munitTimeout: Duration = 60.seconds

  private def cycle[A](body: => A)(using munit.Location): CycleException = intercept[CycleException](body)

  test("a member that depends on itself") {
    val d = Overridable.build(Overridable[Two]((a = self.a, b = 1)))
    val e = cycle(d.a)
    assertEquals(e.path, List("a", "a"))
  }

  test("a cycle in one thread reports the path") {
    val d = Overridable.build(Overridable[Two]((a = self.b + 1, b = self.a + 1)))
    val e = cycle(d.a)
    assertEquals(e.path, List("a", "b", "a"))
    assert(e.getMessage.contains("a -> b -> a"))
  }

  test("a cycle through three members") {
    val d = Overridable.build(Overridable[Three]((a = self.b, b = self.c, c = self.a)))
    assertEquals(cycle(d.b).path, List("b", "c", "a", "b"))
  }

  test("a cycle across layers") {
    // base.a needs self.b; the override's b needs upper.a
    val base = Overridable[Two]((a = self.b, b = 1))
    val d = Overridable.build(base, Override[Two]((b = upper.a)))
    assertEquals(cycle(d.a).path.head, "a")
  }

  test("a cycle through a closure and an object literal") {
    val d = Overridable.build(Overridable[Two](new { def a = List(1).map(_ + self.b).head; def b = self.a }))
    cycle(d.b)
  }

  test("a failed cycle can be asked again and fails again") {
    val d = Overridable.build(Overridable[Two]((a = self.b, b = self.a)))
    cycle(d.a)
    cycle(d.a)
    cycle(d.b)
  }

  test("a member outside the cycle still works") {
    val d = Overridable.build(Overridable[Three]((a = self.b, b = self.a, c = 3)))
    cycle(d.a)
    assertEquals(d.c, 3)
  }

  test("retry after an exception: the member is computed again") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Two]((a = { if n.incrementAndGet() == 1 then throw new RuntimeException("boom"); 42 }, b = self.a)))
    assertEquals(intercept[RuntimeException](d.a).getMessage, "boom")
    assertEquals(d.b, 42)
    assertEquals(d.a, 42)
    assertEquals(n.get, 2)
  }

  test("a value is not computed again after success") {
    val n = new AtomicInteger
    val d = Overridable.build(Overridable[Two]((a = n.incrementAndGet(), b = self.a)))
    (1 to 5).foreach(_ => d.b)
    assertEquals(n.get, 1)
  }

  test("concurrent callers wait for one computation") {
    val n = new AtomicInteger
    val pool = Executors.newFixedThreadPool(8)
    try {
      val d = Overridable.build(Overridable[Two]((a = { n.incrementAndGet(); Thread.sleep(50); 7 }, b = self.a)))
      val results = run(pool, List.fill(8)(() => d.b))
      assertEquals(results.map(_.toOption.get), List.fill(8)(7))
      assertEquals(n.get, 1)
    } finally shutdown(pool)
  }

  test("two threads closing a cycle: both throw, over several rounds") {
    (1 to 20).foreach { _ =>
      val pool = Executors.newFixedThreadPool(2)
      try {
        val latch = new CountDownLatch(2)
        def rendezvous(): Unit = { latch.countDown(); latch.await(10, TimeUnit.SECONDS) }
        val d = Overridable.build(Overridable[Two]((a = { rendezvous(); self.b }, b = { rendezvous(); self.a })))
        val results = run(pool, List(() => d.a, () => d.b))
        results.foreach(r => assert(r.failed.toOption.exists(_.isInstanceOf[CycleException]), s"expected a cycle in every thread, got $results"))
      } finally shutdown(pool)
    }
  }

  test("three threads closing a cycle: all throw, over several rounds") {
    (1 to 20).foreach { _ =>
      val pool = Executors.newFixedThreadPool(3)
      try {
        val latch = new CountDownLatch(3)
        def rendezvous(): Unit = { latch.countDown(); latch.await(10, TimeUnit.SECONDS) }
        val d = Overridable.build(Overridable[Three]((a = { rendezvous(); self.b }, b = { rendezvous(); self.c }, c = { rendezvous(); self.a })))
        val results = run(pool, List(() => d.a, () => d.b, () => d.c))
        results.foreach(r => assert(r.failed.toOption.exists(_.isInstanceOf[CycleException]), s"expected a cycle in every thread, got $results"))
      } finally shutdown(pool)
    }
  }

  test("a cycle across threads through layers") {
    (1 to 10).foreach { _ =>
      val pool = Executors.newFixedThreadPool(2)
      try {
        val latch = new CountDownLatch(2)
        def rendezvous(): Unit = { latch.countDown(); latch.await(10, TimeUnit.SECONDS) }
        val base = Overridable[Two]((a = { rendezvous(); self.b }, b = 0))
        val d = Overridable.build(base, Override[Two]((b = { rendezvous(); upper.a })))
        val results = run(pool, List(() => d.a, () => d.b))
        results.foreach(r => assert(r.failed.toOption.exists(_.isInstanceOf[CycleException]), s"got $results"))
      } finally shutdown(pool)
    }
  }

  test("1000 virtual threads over shared dependencies find no false cycle") {
    val n = new AtomicInteger
    val pool = Executors.newVirtualThreadPerTaskExecutor()
    try {
      val d = Overridable.build(Overridable[Three]((
        a = { n.incrementAndGet(); Thread.sleep(20); 1 },
        b = { Thread.sleep(5); self.a + 1 },
        c = self.a + self.b,
      )))
      val tasks = (0 until 1000).toList.map(i => () => if i % 3 == 0 then d.c else if i % 3 == 1 then d.b else d.a)
      val results = run(pool, tasks)
      assertEquals(results.count(_.isFailure), 0, results.filter(_.isFailure).toString)
      assertEquals(results.map(_.toOption.get).toSet, Set(1, 2, 3))
      assertEquals(n.get, 1)
    } finally shutdown(pool)
  }

  test("1000 virtual threads over many builds, crossed dependencies") {
    val pool = Executors.newVirtualThreadPerTaskExecutor()
    try {
      val base = Overridable[Three]((a = { Thread.sleep(1); self.b + 1 }, b = { Thread.sleep(1); self.c + 1 }, c = 1))
      val builds = Vector.fill(50)(Overridable.build(base))
      val tasks = (0 until 1000).toList.map { i =>
        val d = builds(i % 50)
        () => if i % 2 == 0 then d.a else d.b
      }
      val results = run(pool, tasks)
      assertEquals(results.count(_.isFailure), 0, results.filter(_.isFailure).toString)
    } finally shutdown(pool)
  }
}

object CycleSuite {
  trait Two { def a: Int; def b: Int }
  trait Three { def a: Int; def b: Int; def c: Int }

  def run[A](pool: ExecutorService, tasks: List[() => A]): List[scala.util.Try[A]] = {
    val futures = tasks.map(t => pool.submit(new java.util.concurrent.Callable[A] { def call(): A = t() }))
    futures.map { f =>
      try scala.util.Success(f.get(30, TimeUnit.SECONDS))
      catch {
        case e: java.util.concurrent.ExecutionException => scala.util.Failure(e.getCause)
        case e: java.util.concurrent.TimeoutException => scala.util.Failure(new AssertionError("timed out: deadlock?", e))
      }
    }
  }

  def shutdown(pool: ExecutorService): Unit = {
    pool.shutdownNow()
    pool.awaitTermination(10, TimeUnit.SECONDS)
  }
}
