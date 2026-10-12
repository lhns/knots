package spike.a

import cats.Monoid
import cats.syntax.all.*

class OverridingSuite extends munit.FunSuite {
  private val newDep2 = Override[Deps]((dep2 = Dep("new2")))
  private val wrapDep2 = Override[Deps]((dep2 = Dep(s"wrap(${upper.dep2.name})")))
  private val otherBase = Overridable[Deps]((dep1 = Dep("o1"), dep2 = Dep("o2"), dep3 = Dep("o3")))

  private def result(base: Overridable[Deps], overrides: Override[Deps]*) = Overridable.build(base, overrides*).toTuple

  test("late binding: a base field sees an overridden dependency") {
    assertEquals(Overridable.build(deps, newDep2).dep3, Dep("base3(new2)"))
  }

  test("upper is the layer below") {
    assertEquals(Overridable.build(deps, newDep2, wrapDep2).dep2, Dep("wrap(new2)"))
    assertEquals(Overridable.build(deps, wrapDep2).dep2, Dep("wrap(base2)"))
  }

  test("upper is not a cycle") {
    assertEquals(Overridable.build(deps, wrapDep2, wrapDep2).dep2, Dep("wrap(wrap(base2))"))
  }

  test("flat, layered and composed overrides agree") {
    val flat = result(deps, newDep2, wrapDep2)
    assertEquals(result(deps |+| newDep2, wrapDep2), flat)
    assertEquals(result(deps, newDep2 |+| wrapDep2), flat)
    assertEquals(result(deps |+| newDep2 |+| wrapDep2), flat)
  }

  test("a base with layers is still a base") {
    val layered: Overridable[Deps] = deps |+| newDep2
    assertEquals(Overridable.build(layered).dep2, Dep("new2"))
  }

  test("monoid: empty is the identity") {
    val empty = Monoid[Override[Deps]].empty
    assertEquals(result(deps, empty |+| wrapDep2), result(deps, wrapDep2))
    assertEquals(result(deps, wrapDep2 |+| empty), result(deps, wrapDep2))
    assertEquals(result(deps, empty), result(deps))
  }

  test("monoid: |+| is associative") {
    val c = Override[Deps]((dep2 = Dep(s"again(${upper.dep2.name})"), dep1 = Dep("c1")))
    assertEquals(result(deps, (newDep2 |+| wrapDep2) |+| c), result(deps, newDep2 |+| (wrapDep2 |+| c)))
  }

  test("a base given as the only override replaces everything below") {
    assertEquals(result(deps, otherBase), result(otherBase))
  }

  test("a base after other layers replaces them") {
    assertEquals(result(deps, newDep2, otherBase), result(otherBase))
  }

  test("layers after a replacing base apply to it") {
    assertEquals(result(deps, otherBase, wrapDep2).dep2, Dep("wrap(o2)"))
  }

  test("base |+| base is the second base") {
    assertEquals(result(deps |+| otherBase), result(otherBase))
  }

  test("monoid accepts a base on either side") {
    assertEquals(result(deps, newDep2 |+| otherBase), result(otherBase))
    assertEquals(result(deps, otherBase |+| wrapDep2).dep2, Dep("wrap(o2)"))
  }

  test("a replaced base is never computed") {
    var computed = false
    val replaced = Overridable[Deps]((dep1 = { computed = true; Dep("x") }, dep2 = Dep("x"), dep3 = Dep("x")))
    Overridable.build(replaced, otherBase).toTuple
    assert(!computed)
  }
}
