package spike.d

import cats.syntax.all.*

object Demo {
  case class Dependency(name: String)

  // Target as a trait: nominal types everywhere.
  trait MainDependencies {
    def dep1: Dependency
    def dep2: Dependency
    def dep3: Dependency
  }

  object MainDependencies {
    val base = Overridable[MainDependencies]((
      dep1 = Dependency("base1"),
      dep2 = Dependency("base2"),
      dep3 = {
        println(s"dep3 initialized using ${self.dep2}")
        Dependency("base3")
      },
    ))
  }

  val o1 = Override[MainDependencies]((dep2 = {
    println(s"dep2 initialized using ${self.dep1}")
    Dependency("overridden2")
  }))

  val o2 = Override[MainDependencies](new {
    def dep2 = Dependency(s"wrapped(${upper.dep2.name})")
  })

  // Target as a named tuple: `self`/`upper` are Views (Selectable), `toTuple` gives the plain tuple.
  type MainDeps = View[(dep1: Dependency, dep2: Dependency, dep3: Dependency)]

  val tupleBase = Overridable[MainDeps]((
    dep1 = Dependency("base1"),
    dep2 = Dependency("base2"),
    dep3 = {
      println(s"dep3 initialized using ${self.dep2}")
      Dependency("base3")
    },
  ))

  val t1 = Override[MainDeps]((dep2 = Dependency(s"overridden2 using ${self.dep1.name}")))

  val t2 = Override[MainDeps](new {
    def dep2 = Dependency(s"wrapped(${upper.dep2.name})")
  })

  def show(label: String, deps: MainDependencies): Unit = {
    println(s"$label.dep1 ${deps.dep1}")
    println(s"$label.dep2 ${deps.dep2}")
    println(s"$label.dep3 ${deps.dep3}")
    println(s"$label.dep3 ${deps.dep3}")
  }

  def main(args: Array[String]): Unit = {
    show("base", Overridable.build(MainDependencies.base))
    show("flat", Overridable.build(MainDependencies.base, o1, o2))
    show("layered", Overridable.build(MainDependencies.base |+| o1, o2))
    show("composed", Overridable.build(MainDependencies.base, o1 |+| o2))

    val tuple = Overridable.build(tupleBase, t1 |+| t2)
    println(s"tuple.dep2 ${tuple.dep2}")
    println(s"tuple.dep3 ${tuple.dep3}")
    println(s"tuple.toTuple ${tuple.toTuple}")
  }
}
