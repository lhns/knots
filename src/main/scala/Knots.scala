object Knots {
  trait KnotEnd[A] {
    def self: A
  }

  inline def self[A](using knotEnd: KnotEnd[A]): A = knotEnd.self

  type Knot[A] = KnotEnd[A] ?=> A

  object Knot {
    inline def apply[A](f: KnotEnd[A] ?=> A): Knot[A] = f

    def tie[A](knot: Knot[A]): A = {
      var r: A | Null = null
      r = knot(using new KnotEnd[A] {
        override def self: A = r match {
          case null => throw new IllegalStateException("cannot evaluate untied knot")
          case a: A @unchecked => a
        }
      })
      r.asInstanceOf[A]
    }

    def layer[A](knot: Knot[A])(lowerKnot: DoubleKnot[A]): Knot[A] = Knot {
      lowerKnot(using new DoubleKnotEnd[A] {
        override lazy val upper: A = knot

        override def self: A = summon[KnotEnd[A]].self
      })
    }
  }

  trait DoubleKnotEnd[A] extends KnotEnd[A] {
    def self: A

    def upper: A
  }

  type DoubleKnot[A] = DoubleKnotEnd[A] ?=> A

  inline def upper[A](using doubleKnot: DoubleKnotEnd[A]): A = doubleKnot.upper

  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency

    def dep2: Dependency

    def dep3: Dependency
  }

  object MainDependencies {
    def knot: Knot[MainDependencies] = Knot(new MainDependencies {
      def dep1: Dependency = Dependency("base1")

      def dep2: Dependency = Dependency("base2")

      def dep3: Dependency = {
        println(s"dep3 initialized using ${self.dep2}")
        new Dependency("base3")
      }
    })

    class Delegate(using DoubleKnotEnd[MainDependencies]) extends MainDependencies {
      override def dep1: Dependency = upper.dep1

      override def dep2: Dependency = upper.dep2

      override def dep3: Dependency = upper.dep3
    }

    def apply(): MainDependencies = Knot.tie(knot)
  }

  def main(args: Array[String]): Unit = {
    val base = MainDependencies()
    println("base.dep1 " ++ base.dep1.toString)
    println("base.dep2 " ++ base.dep2.toString)
    println("base.dep3 " ++ base.dep3.toString)

    val decorated = Knot.tie(Knot.layer(MainDependencies.knot)(new MainDependencies.Delegate {
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