package spike.b

import Fx.*

class HigherKindedSpec extends munit.FunSuite {
  private val concrete = Overridable[HkDeps[Option]](new {
    def depA = Some(Dep("a"))
    def depB = self.depA.map(d => Dep(d.name + "b"))
  })

  private def generic[F[_]](pure: Dep => F[Dep]): Overridable[HkDeps[F]] =
    Overridable[HkDeps[F]](new { def depA = pure(Dep("ga")); def depB = pure(Dep("gb")) })

  private def over[F[_]](pure: Dep => F[Dep]): Override[HkDeps[F]] =
    Override[HkDeps[F]](new { def depA = pure(Dep("over")) })

  test("a parameterized trait with a concrete F") {
    val d = Overridable.build(concrete)
    assertEquals(d.depB, Some(Dep("ab")))
  }

  test("late binding works with a concrete F") {
    val d = Overridable.build(concrete, Override[HkDeps[Option]](new { def depA = Some(Dep("x")) }))
    assertEquals(d.depB, Some(Dep("xb")))
  }

  test("upper works with a concrete F") {
    val d = Overridable.build(concrete, Override[HkDeps[Option]](new { def depA = upper.depA.map(x => Dep(x.name + "!")) }))
    assertEquals(d.depA, Some(Dep("a!")))
  }

  test("a parameterized trait with a generic F") {
    val d = Overridable.build(generic[List](List(_)), over[List](List(_)))
    assertEquals(d.depA, List(Dep("over")))
    assertEquals(d.depB, List(Dep("gb")))
  }

  test("a generic F works with another F") {
    val d = Overridable.build(generic[Option](Some(_)), over[Option](Some(_)))
    assertEquals(d.depA, Some(Dep("over")))
  }
}
