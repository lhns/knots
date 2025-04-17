object Knots {
  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency

    def dep2: Dependency

    def dep3: Dependency
  }

  object MainDependencies {
    def apply(self: => MainDependencies): MainDependencies = new MainDependencies {
      def dep1: Dependency = Dependency("base1")

      def dep2: Dependency = Dependency("base2")

      def dep3: Dependency = {
        println(s"dep3 initialized using ${self.dep2}")
        new Dependency("base3")
      }
    }

    /*def apply(): MainDependencies = {
      var self: MainDependencies = null
      self = apply(self)
      self
    }*/

    def apply(): MainDependencies = knot(apply)
  }

  def knot[A](f: (=> A) => A): A = {
    var r: A | Null = null
    r = f(r.asInstanceOf[A])
    r.asInstanceOf[A]
  }

  val base = MainDependencies()
  println("base.dep1 " ++ base.dep1.toString)
  println("base.dep2 " ++ base.dep2.toString)
  println("base.dep3 " ++ base.dep3.toString)

  def overrideDep2(base: (MainDependencies) => MainDependencies)(self: => MainDependencies) = new MainDependencies {
    private lazy val _base = base(this)
    export _base.{dep2 as _, *}

    override def dep2: Dependency = {
      println(s"dep2 initialized using ${self.dep1}")
      new Dependency("overridden2")
    }
  }

  val decorated = knot(overrideDep2(MainDependencies.apply))
  println("decorated.dep1 " ++ decorated.dep1.toString)
  println("decorated.dep2 " ++ decorated.dep2.toString)
  println("decorated.dep3 " ++ decorated.dep3.toString)
}