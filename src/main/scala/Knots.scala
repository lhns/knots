object Knots {
  trait Knot[A] {
    def self: A
  }

  def self[A](using knot: Knot[A]): A = knot.self

  def tieKnot[A](f: Knot[A] ?=> A): A = {
    var r: A | Null = null
    r = f(using new Knot[A] {
      override def self: A = r match {
        case null => throw new IllegalStateException("cannot evaluate untied knot")
        case a: A @unchecked => a
      }
    })
    r.asInstanceOf[A]
  }

  trait DoubleKnot[A] extends Knot[A] {
    def self: A

    def upper: A
  }

  def upper[A](using doubleKnot: DoubleKnot[A]): A = doubleKnot.upper

  def tieKnotWith[A](upperKnot: Knot[A] ?=> A)(lowerKnot: DoubleKnot[A] ?=> A): A =
    tieKnot {
      lowerKnot(using new DoubleKnot[A] {
        override lazy val upper: A = upperKnot

        override def self: A = summon[Knot[A]].self
      })
    }

  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency

    def dep2: Dependency

    def dep3: Dependency
  }

  object MainDependencies {
    def knot(using Knot[MainDependencies]): MainDependencies = new MainDependencies {
      def dep1: Dependency = Dependency("base1")

      def dep2: Dependency = Dependency("base2")

      def dep3: Dependency = {
        println(s"dep3 initialized using ${self.dep2}")
        new Dependency("base3")
      }
    }

    class Delegate(using DoubleKnot[MainDependencies]) extends MainDependencies {
      override def dep1: Dependency = upper.dep1

      override def dep2: Dependency = upper.dep2

      override def dep3: Dependency = upper.dep3
    }

    def apply(): MainDependencies = tieKnot(knot)
  }

  def main(args: Array[String]): Unit = {
    val base = MainDependencies()
    println("base.dep1 " ++ base.dep1.toString)
    println("base.dep2 " ++ base.dep2.toString)
    println("base.dep3 " ++ base.dep3.toString)

    val decorated = tieKnotWith(MainDependencies.knot)(new MainDependencies.Delegate {
      override def dep2: Dependency = {
        println(s"dep2 initialized using ${self.dep1}")
        new Dependency("overridden2")
      }
    })

    println("decorated.dep1 " ++ decorated.dep1.toString)
    println("decorated.dep2 " ++ decorated.dep2.toString)
    println("decorated.dep3 " ++ decorated.dep3.toString)
  }
}
