import cats.{Eval, Id, ~>}
import cats.tagless.FunctorK

object Knots {
  case class Dependency(name: String)

  case class MainDependencies[F[_]](
                                     dep1: F[Dependency],
                                     dep2: F[Dependency],
                                     dep3: F[Dependency]
                                   )
  given FunctorK[MainDependencies] = new FunctorK[MainDependencies] {
    override def mapK[F[_], G[_]](af: MainDependencies[F])(fk: F ~> G): MainDependencies[G] =
      MainDependencies(
        dep1 = fk(af.dep1),
        dep2 = fk(af.dep2),
        dep3 = fk(af.dep3)
      )
  }

  trait Self[A] {
    def self: A
  }

  inline def self[A](using self: Self[A]): A = self.self
  
  type WithSelf[Alg[_[_]]] = [A] =>> Self[WithSelf[Alg]] ?=> A
  type Dep[Alg[_[_]]] = Alg[WithSelf[Alg]]

  val evalK: Eval ~> Id = new (Eval ~> Id) {
    override def apply[A](fa: Eval[A]): Id[A] = fa.value
  }

  extension [F[_[_]]](dep: Dep[F])(using F: FunctorK[F]) {
    def eval: F[Id] = {
      var r: F[Id] | Null = null
      lazy val lazyR = r
      val self = new Self[F[Id]] {
        override def self: F[Id] = r.asInstanceOf[F[Id]]
        /*match {
          case null => throw new IllegalStateException("cannot evaluate untied knot")
          case a: F[Id] @unchecked => a
        }*/
      }
      r = F.mapK(F.mapK(dep) {
        new (WithSelf[F] ~> Eval) {
          override def apply[A](fa: WithSelf[F][A]): Eval[A] =
            Eval.later(fa(using self))
        }
      })(evalK)
      r.asInstanceOf[F[Id]]
    }
  }

  object MainDependencies {
    def apply(): Dep[MainDependencies] = new Dep[MainDependencies](
      dep1 = Dependency("base1"),
      dep2 = Dependency("base2"),
      dep3 = {
        println(s"dep3 initialized using ${self.dep2}")
        new Dependency("base3")
      }
    )
  }

  def main(args: Array[String]): Unit = {
    val base = MainDependencies().eval
    println("base.dep1 " ++ base.dep1.toString)
    println("base.dep2 " ++ base.dep2.toString)
    println("base.dep3 " ++ base.dep3.toString)

    println("---")
    /*
  val decorated = MainDependencies().copy(
    dep2 = {
      println(s"dep2 initialized using ${self.dep1}")
      new Dependency("overridden2")
    }
  ).eval
  println("decorated.dep1 " ++ decorated.dep1.toString)
  println("decorated.dep2 " ++ decorated.dep2.toString)
  println("decorated.dep3 " ++ decorated.dep3.toString)*/
  }
}