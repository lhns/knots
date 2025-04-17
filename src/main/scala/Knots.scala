object Knots {
  trait Self[A] {
    def self: A
  }

  inline def self[A](using knotEnd: Self[A]): A = knotEnd.self

  type Overridable[A] = Self[A] ?=> A

  object Overridable {
    inline def apply[A](f: Self[A] ?=> A): Overridable[A] = f

    def build[A](knot: Overridable[A]): A = {
      var r: A | Null = null
      r = knot(using new Self[A] {
        override def self: A = r match {
          case null => throw new IllegalStateException("cannot evaluate untied knot")
          case a: A @unchecked => a
        }
      })
      r.asInstanceOf[A]
    }

    def layer[A](knot: Overridable[A])(lowerKnot: Override[A]): Overridable[A] = Overridable {
      lowerKnot(using new Super[A] {
        override lazy val upper: A = knot

        override def self: A = summon[Self[A]].self
      })
    }
  }

  trait Super[A] extends Self[A] {
    def self: A

    def upper: A
  }

  type Override[A] = Super[A] ?=> A

  inline def upper[A](using doubleKnot: Super[A]): A = doubleKnot.upper

  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency

    def dep2: Dependency

    def dep3: Dependency
  }

  object MainDependencies {
    def knot: Overridable[MainDependencies] = Overridable(new MainDependencies {
      def dep1: Dependency = Dependency("base1")

      def dep2: Dependency = Dependency("base2")

      def dep3: Dependency = {
        println(s"dep3 initialized using ${self.dep2}")
        new Dependency("base3")
      }
    })

    class Delegate(using Super[MainDependencies]) extends MainDependencies {
      override def dep1: Dependency = upper.dep1

      override def dep2: Dependency = upper.dep2

      override def dep3: Dependency = upper.dep3
    }

    def apply(): MainDependencies = Overridable.build(knot)
  }

  def main(args: Array[String]): Unit = {
    val base = MainDependencies()
    println("base.dep1 " ++ base.dep1.toString)
    println("base.dep2 " ++ base.dep2.toString)
    println("base.dep3 " ++ base.dep3.toString)

    val decorated = Overridable.build(Overridable.layer(MainDependencies.knot)(new MainDependencies.Delegate {
      override def dep2: Dependency = {
        println(s"dep2 initialized using ${self.dep1}")
        new Dependency("overridden2")
      }
    }))

    println("decorated.dep1 " ++ decorated.dep1.toString)
    println("decorated.dep2 " ++ decorated.dep2.toString)
    println("decorated.dep3 " ++ decorated.dep3.toString)
  }
}