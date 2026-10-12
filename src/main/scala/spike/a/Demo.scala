package spike.a

object Demo {
  case class Dependency(name: String)

  type MainDeps = (dep1: Dependency, dep2: Dependency, dep3: Dependency)

  val base = Overridable[MainDeps]((
    dep1 = Dependency("base1"),
    dep2 = Dependency("base2"),
    dep3 = {
      println(s"dep3 initialized using ${self.dep2}")
      Dependency("base3")
    },
  ))

  val o1 = Override[MainDeps]((dep2 = {
    println(s"dep2 initialized using ${self.dep1}")
    Dependency("overridden2")
  }))

  val o2 = Override[MainDeps]((dep2 = Dependency(s"wrapped(${upper.dep2.name})")))

  def show(label: String, deps: MainDeps): Unit = {
    println(s"$label.dep1 ${deps.dep1}")
    println(s"$label.dep2 ${deps.dep2}")
    println(s"$label.dep3 ${deps.dep3}")
  }

  def main(args: Array[String]): Unit = {
    show("base", Overridable.build(base))
    show("flat", Overridable.build(base, o1, o2))
    show("layered", Overridable.build(Overridable(base, o1), o2))
    show("composed", Overridable.build(base, Override[MainDeps](o1, o2)))
  }
}
