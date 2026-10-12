package spike.a

import scala.NamedTuple.{AnyNamedTuple, Names}
import scala.collection.mutable
import scala.compiletime.constValueTuple

trait Self[F <: AnyNamedTuple] {
  def self: View[F]
}

trait Super[F <: AnyNamedTuple] extends Self[F] {
  def upper: View[F]
}

inline def self[F <: AnyNamedTuple](using s: Self[F]): View[F] = s.self

inline def upper[F <: AnyNamedTuple](using s: Super[F]): View[F] = s.upper

// Every field of F, deferred until `self` exists.
type Defs[F <: AnyNamedTuple] = NamedTuple.Map[F, [X] =>> Self[F] ?=> X]

// Lazy, cached access to the fields of F while the knot is being tied.
final class View[F <: AnyNamedTuple](resolve: String => Any) extends Selectable {
  type Fields = F

  def selectDynamic(name: String): Any = resolve(name)
}

final class Setter[F <: AnyNamedTuple] extends Selectable {
  type Fields = NamedTuple.Map[F, [X] =>> Field[F, X]]

  def selectDynamic(name: String): Field[F, Any] = Field(name)
}

final class Field[F <: AnyNamedTuple, X](name: String) {
  def :=(value: Super[F] ?=> X): Binding[F] = Binding(name, s => value(using s))
}

final case class Binding[F <: AnyNamedTuple](name: String, value: Super[F] => Any)

final case class Overridable[F <: AnyNamedTuple](
  names: List[String],
  base: Map[String, Self[F] => Any],
  layers: Vector[Map[String, Super[F] => Any]]
) {
  def layer(bindings: (Setter[F] => Binding[F])*): Overridable[F] = {
    val setter = Setter[F]()
    copy(layers = layers :+ bindings.map(_(setter)).map(b => b.name -> b.value).toMap)
  }

  // Level 0 is the base, level i > 0 is layers(i - 1); `upper` of level i is level i - 1.
  def build: F = {
    val top = layers.length
    val caches = Vector.fill(top + 1)(mutable.Map.empty[String, Any])
    val inProgress = mutable.LinkedHashSet.empty[(Int, String)]
    lazy val views: Vector[View[F]] = Vector.tabulate(top + 1)(level => View[F](resolve(level, _)))

    def definingLevel(level: Int, name: String): Int =
      if level == 0 || layers(level - 1).contains(name) then level else definingLevel(level - 1, name)

    def resolve(level: Int, name: String): Any = {
      val l = definingLevel(level, name)
      caches(l).getOrElse(name, compute(l, name))
    }

    def compute(level: Int, name: String): Any = {
      val key = (level, name)
      if inProgress.contains(key) then
        val path = inProgress.toList.dropWhile(_ != key).map(_._2) :+ name
        throw IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}")
      inProgress += key
      try {
        val value =
          if level == 0 then base(name)(new Self[F] { def self = views(top) })
          else layers(level - 1)(name)(new Super[F] {
            def self = views(top)
            def upper = views(level - 1)
          })
        caches(level)(name) = value
        value
      } finally inProgress -= key
    }

    Tuple.fromArray(names.map(views(top).selectDynamic).toArray[Any]).asInstanceOf[F]
  }
}

object Overridable {
  inline def apply[F <: AnyNamedTuple](defs: Defs[F]): Overridable[F] = {
    val names = constValueTuple[Names[F]].toList.asInstanceOf[List[String]]
    val fns = defs.toTuple.toList.map(f => (s: Self[F]) => f.asInstanceOf[Self[F] ?=> Any](using s))
    Overridable(names, names.zip(fns).toMap, Vector.empty)
  }
}

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

  def main(args: Array[String]): Unit = {
    val b: MainDeps = base.build
    println("base.dep1 " ++ b.dep1.toString)
    println("base.dep2 " ++ b.dep2.toString)
    println("base.dep3 " ++ b.dep3.toString)

    val decorated: MainDeps = base
      .layer(_.dep2 := {
        println(s"dep2 initialized using ${self.dep1}")
        Dependency("overridden2")
      })
      .layer(_.dep2 := Dependency(s"wrapped(${upper.dep2.name})"))
      .build

    println("decorated.dep1 " ++ decorated.dep1.toString)
    println("decorated.dep2 " ++ decorated.dep2.toString)
    println("decorated.dep3 " ++ decorated.dep3.toString)
  }
}
