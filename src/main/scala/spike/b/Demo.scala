package spike.b

import cats.syntax.all.*

object Demo {
  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency
    def dep2: Dependency
    def dep3: Dependency
  }

  object MainDependencies {
    val base = Overridable[MainDependencies](new {
      lazy val dep1 = Dependency("base1")
      lazy val dep2 = Dependency("base2")
      lazy val dep3 = {
        println(s"dep3 initialized using ${self.dep2}")
        Dependency("base3")
      }
    })
  }

  import MainDependencies.base

  val o1 = Override[MainDependencies](new {
    lazy val dep2 = {
      println(s"dep2 initialized using ${self.dep1}")
      Dependency("overridden2")
    }
  })

  val o2 = Override[MainDependencies](new {
    lazy val dep2 = Dependency(s"wrapped(${upper.dep2.name})")
  })

  def show(label: String, deps: MainDependencies): Unit = {
    println(s"$label.dep1 ${deps.dep1}")
    println(s"$label.dep2 ${deps.dep2}")
    println(s"$label.dep3 ${deps.dep3}")
    println(s"$label.dep3 ${deps.dep3}")
  }

  def main(args: Array[String]): Unit = {
    show("base", Overridable.build(base))
    show("flat", Overridable.build(base, o1, o2))
    show("layered", Overridable.build(Overridable(base, o1), o2))
    show("composed", Overridable.build(base, o1 |+| o2))
  }
}
