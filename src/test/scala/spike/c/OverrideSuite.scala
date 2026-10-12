package spike.c.tests

import cats.syntax.all.*
import spike.c.*

import java.util.concurrent.atomic.AtomicInteger

object OvFixtures {
  trait Three(using Upper[Three]) {
    def a: Int = inherited
    def b: Int = inherited
    def c: Int = inherited
  }

  trait Abs(using Upper[Abs]) {
    def a: Int
    def b: Int = inherited
  }

  trait Base0 { def x: Int }
  trait Multi(using Upper[Multi]) extends Base0 { def y: Int = inherited }

  case class Dep(name: String)
  trait HkDeps[F[_]](using Upper[HkDeps[F]]) {
    def depA: F[Dep] = inherited
    def depB: F[Dep] = inherited
  }

}

class OverrideSuite extends munit.FunSuite {
  import OvFixtures.*

  private val base = Overridable[Three](new Three {
    override lazy val a = 1
    override lazy val b = 2
    override lazy val c = 3
  })

  test("a base alone gives its values") {
    val t = Overridable.build(base)
    assertEquals((t.a, t.b, t.c), (1, 2, 3))
  }

  test("an override replaces a member and leaves the others") {
    val o = Override[Three](new Three { override lazy val b = 20 })
    val t = Overridable.build(base, o)
    assertEquals((t.a, t.b, t.c), (1, 20, 3))
  }

  test("late binding: a base member sees an override of what it reads") {
    val b = Overridable[Three](new Three {
      override lazy val a = self.b + 1
      override lazy val b = 5
      override lazy val c = 0
    })
    val o = Override[Three](new Three { override lazy val b = 50 })
    assertEquals(Overridable.build(b).a, 6)
    assertEquals(Overridable.build(b, o).a, 51)
  }

  test("upper chains through several overrides") {
    val o1 = Override[Three](new Three { override lazy val a = upper.a + 10 })
    val o2 = Override[Three](new Three { override lazy val a = upper.a + 100 })
    assertEquals(Overridable.build(base, o1, o2).a, 111)
  }

  test("flat, layered and composed overrides agree") {
    val o1 = Override[Three](new Three { override lazy val a = upper.a + 10 })
    val o2 = Override[Three](new Three { override lazy val a = upper.a * 2 })
    val expected = 22
    assertEquals(Overridable.build(base, o1, o2).a, expected)
    assertEquals(Overridable.build(base |+| o1, o2).a, expected)
    assertEquals(Overridable.build(base, o1 |+| o2).a, expected)
  }

  test("Override has a Monoid with a pass-through empty") {
    val o = Override[Three](new Three { override lazy val b = upper.b * 3 })
    val m = Override.monoid[Three]
    assertEquals(Overridable.build(base, m.empty).b, 2)
    assertEquals(Overridable.build(base, m.empty |+| o).b, 6)
    assertEquals(Overridable.build(base, o |+| m.empty).b, 6)
    assertEquals(Overridable.build(base, List(o, o).combineAll).b, 18)
  }

  test("an override of a member nobody asked for is never computed") {
    val n = new AtomicInteger
    val o = Override[Three](new Three { override lazy val c = { n.incrementAndGet(); 0 } })
    val t = Overridable.build(base, o)
    t.a
    assertEquals(n.get, 0)
  }

  test("lazy val, def and strict val are each computed once") {
    val n = new AtomicInteger
    val t = Overridable.build(Overridable[Three](new Three {
      override lazy val a = n.incrementAndGet()
      override def b = n.incrementAndGet() * 100
      override val c = n.incrementAndGet() * 10000
    }))
    assertEquals(n.get, 0)
    val r = List(t.a, t.a, t.b, t.b, t.c, t.c)
    assertEquals(n.get, 3)
    assertEquals(r.distinct.size, 3)
  }

  test("a strict override val is lazy: not evaluated when the knot is tied") {
    val n = new AtomicInteger
    val t = Overridable.build(Overridable[Three](new Three {
      override val a = n.incrementAndGet()
      override val b = 0
      override val c = 0
    }))
    assertEquals(n.get, 0)
    assertEquals(t.a, 1)
    assertEquals(t.a, 1)
    assertEquals(n.get, 1)
  }

  test("a strict override val can read self") {
    val t = Overridable.build(Overridable[Three](new Three {
      override val a = self.b + 1
      override val b = 5
      override val c = 0
    }))
    assertEquals(t.a, 6)
  }

  test("a private helper in the literal can be used") {
    val t = Overridable.build(Overridable[Three](new Three {
      private def twice(i: Int) = i * 2
      override lazy val a = twice(self.b)
      override lazy val b = 21
      override lazy val c = 0
    }))
    assertEquals(t.a, 42)
  }

  test("after an exception the next caller retries") {
    val n = new AtomicInteger
    val t = Overridable.build(Overridable[Three](new Three {
      override lazy val a = { if n.incrementAndGet() == 1 then throw new RuntimeException("boom") else n.get }
      override lazy val b = 0
      override lazy val c = 0
    }))
    intercept[RuntimeException](t.a)
    assertEquals(t.a, 2)
    assertEquals(t.a, 2)
    assertEquals(n.get, 2)
  }

  test("an abstract trait member must be implemented in each literal and can use upper") {
    val b = Overridable[Abs](new Abs { def a = 1; override lazy val b = self.a + 1 })
    val o = Override[Abs](new Abs { def a = upper.a + 10 })
    val t = Overridable.build(b, o)
    assertEquals(t.a, 11)
    assertEquals(t.b, 12)
  }

  test("a type alias of the trait works as the target") {
    type Alias = Abs
    val t = Overridable.build(Overridable[Alias](new Abs { def a = 5; override def b = 6 }))
    assertEquals((t.a, t.b), (5, 6))
  }

  test("an abstract member inherited from another trait works") {
    val t = Overridable.build(Overridable[Multi](new Multi { def x = 3; override lazy val y = self.x * 2 }))
    assertEquals(t.y, 6)
  }

  test("a base can be replaced by another base as a child (Overridable is an Override)") {
    val replacement: Override[Three] = Overridable[Three](new Three {
      override lazy val a = 100
      override lazy val b = 200
      override lazy val c = 300
    })
    assertEquals(Overridable.build(base, replacement).a, 100)
  }

  test("parameterized trait with a concrete type constructor") {
    val b = Overridable[HkDeps[Option]](new HkDeps[Option] {
      override lazy val depA = Some(Dep("a"))
      override lazy val depB = self.depA.map(d => Dep(d.name + "b"))
    })
    assertEquals(Overridable.build(b).depB, Some(Dep("ab")))
  }

  test("parameterized trait with an abstract type constructor") {
    def generic[F[_]](pure: Dep => F[Dep]): Overridable[HkDeps[F]] =
      Overridable[HkDeps[F]](new HkDeps[F] {
        override lazy val depA = pure(Dep("ga"))
        override lazy val depB = pure(Dep("gb"))
      })
    def over[F[_]](pure: Dep => F[Dep]): Override[HkDeps[F]] =
      Override[HkDeps[F]](new HkDeps[F] { override lazy val depA = pure(Dep("over")) })
    val t = Overridable.build(generic[List](List(_)), over[List](List(_)))
    assertEquals(t.depA, List(Dep("over")))
    assertEquals(t.depB, List(Dep("gb")))
  }
}
