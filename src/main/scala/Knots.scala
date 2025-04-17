object Knots {
  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency

    def dep2: Dependency

    def dep3: Dependency
  }

  object MainDependencies {
    def apply(): MainDependencies = new MainDependencies {
      def dep1: Dependency = Dependency("base1")

      def dep2: Dependency = Dependency("base2")

      def dep3: Dependency = {
        println(s"dep3 initialized using $dep2")
        new Dependency("base3")
      }
    }
  }

  val base = MainDependencies()
  println("base.dep1 " ++ base.dep1.toString)
  println("base.dep2 " ++ base.dep2.toString)
  println("base.dep3 " ++ base.dep2.toString)

  /*open class MainDependenciesDecorator(delegate: => MainDependencies) extends MainDependencies {
    private lazy val delegate_ = delegate
    export delegate_.*
  }*/

  def overrideDep2(base: => MainDependencies) = new MainDependencies {
    private lazy val _base = base
    export _base.{dep2 as _, *}

    override def dep2: Dependency = {
      println(s"dep2 initialized using $dep1")
      new Dependency("overridden2")
    }
  }

  val decorated = overrideDep2(MainDependencies())
  println("decorated.dep1 " ++ decorated.dep1.toString)
  println("decorated.dep2 " ++ decorated.dep2.toString)
  println("decorated.dep3 " ++ decorated.dep3.toString)
}