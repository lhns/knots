package spike.b

trait Self[A] {
  def self: A
}

trait Super[A] extends Self[A] {
  def upper: A
}

inline def self[A](using s: Self[A]): A = s.self

inline def upper[A](using s: Super[A]): A = s.upper

type Overridable[A] = Self[A] ?=> A

type Override[A] = Super[A] ?=> A

object Overridable {
  def build[A](base: Overridable[A], overrides: Override[A]*): A = {
    var r: A | Null = null
    given Self[A] with {
      def self: A = r match {
        case null => throw new IllegalStateException("cannot evaluate untied knot")
        case a: A @unchecked => a
      }
    }
    r = overrides.foldLeft[() => A](() => base) { (lower, ovr) =>
      () => ovr(using new Super[A] {
        lazy val upper: A = lower()
        def self: A = summon[Self[A]].self
      })
    }()
    r.asInstanceOf[A]
  }
}

object Demo {
  case class Dependency(name: String)

  trait MainDependencies {
    def dep1: Dependency
    def dep2: Dependency
    def dep3: Dependency
  }

  object MainDependencies {
    def base: Overridable[MainDependencies] = new MainDependencies {
      lazy val dep1 = Dependency("base1")
      lazy val dep2 = Dependency("base2")
      lazy val dep3 = {
        println(s"dep3 initialized using ${self.dep2}")
        Dependency("base3")
      }
    }

    class Delegate(using Super[MainDependencies]) extends MainDependencies {
      def dep1 = upper.dep1
      def dep2 = upper.dep2
      def dep3 = upper.dep3
    }
  }

  def main(args: Array[String]): Unit = {
    val b = Overridable.build(MainDependencies.base)
    println("base.dep1 " ++ b.dep1.toString)
    println("base.dep2 " ++ b.dep2.toString)
    println("base.dep3 " ++ b.dep3.toString)

    val decorated = Overridable.build(
      MainDependencies.base,
      new MainDependencies.Delegate {
        override lazy val dep2 = {
          println(s"dep2 initialized using ${self.dep1}")
          Dependency("overridden2")
        }
      },
      new MainDependencies.Delegate {
        override lazy val dep2 = Dependency(s"wrapped(${upper.dep2.name})")
      }
    )

    println("decorated.dep1 " ++ decorated.dep1.toString)
    println("decorated.dep2 " ++ decorated.dep2.toString)
    println("decorated.dep3 " ++ decorated.dep3.toString)
    println("decorated.dep3 " ++ decorated.dep3.toString)
  }
}
