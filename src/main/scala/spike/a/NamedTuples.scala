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

// A layer is evaluated as a unit: its fields can't reference each other via `self`.
final case class Layer[F <: AnyNamedTuple](names: List[String], values: Super[F] => Partial[F])

final class Override[F <: AnyNamedTuple](val layers: Vector[Layer[F]])

object Override {
  // `Override[F]` fixes F; the builder's `apply` then infers the literal's type O.
  def apply[F <: AnyNamedTuple]: OverrideBuilder[F] = OverrideBuilder()
}

final class OverrideBuilder[F <: AnyNamedTuple] {
  inline def apply[O <: AnyNamedTuple](o: Super[F] ?=> O)(using O SubsetOf F): Override[F] = {
    val names = constValueTuple[Names[O]].toList.asInstanceOf[List[String]]
    new Override(Vector(Layer[F](names, s => Partial[F](o(using s)))))
  }

  // Stacked in order: each override's `upper` is the result of the ones before it.
  def apply(first: Override[F], second: Override[F], more: Override[F]*): Override[F] =
    new Override((first +: second +: more).toVector.flatMap(_.layers))
}

final class Overridable[F <: AnyNamedTuple](
  val names: List[String],
  val base: Map[String, Self[F] => Any],
  val layers: Vector[Layer[F]]
)

object Overridable {
  inline def apply[F <: AnyNamedTuple](defs: Defs[F]): Overridable[F] = {
    val names = constValueTuple[Names[F]].toList.asInstanceOf[List[String]]
    val fns = defs.toTuple.toList.map(f => (s: Self[F]) => f.asInstanceOf[Self[F] ?=> Any](using s))
    new Overridable(names, names.zip(fns).toMap, Vector.empty)
  }

  def apply[F <: AnyNamedTuple](base: Overridable[F], ovr: Override[F], more: Override[F]*): Overridable[F] =
    new Overridable(base.names, base.base, base.layers ++ (ovr +: more).flatMap(_.layers))

  // Level 0 is the base, level i > 0 is layers(i - 1); `upper` of level i is level i - 1.
  def build[F <: AnyNamedTuple](base: Overridable[F], overrides: Override[F]*): F = {
    val layers = base.layers ++ overrides.flatMap(_.layers)
    val top = layers.length
    val fieldCache = mutable.Map.empty[String, Any]
    val layerCache = mutable.Map.empty[Int, Map[String, Any]]
    val inProgress = mutable.LinkedHashMap.empty[(Int, String), Unit]
    lazy val views: Vector[View[F]] = Vector.tabulate(top + 1)(level => View[F](resolve(level, _)))

    def label(level: Int, name: String) = if level == 0 then s"$name (base)" else s"$name (layer $level)"

    def definingLevel(level: Int, name: String): Int =
      if level == 0 || layers(level - 1).names.contains(name) then level else definingLevel(level - 1, name)

    // Base fields are guarded per field, layers as a whole.
    def guarded[T](level: Int, name: String)(compute: => T): T = {
      val clash = inProgress.keys.find((l, n) => l == level && (level != 0 || n == name))
      clash.foreach { start =>
        val cycle = inProgress.keys.toList.dropWhile(_ != start)
        val path = cycle.map(label) :+ label(level, name)
        val hint =
          if level != 0 && cycle.forall(_._1 == level) then " (a layer is evaluated as a unit; its fields can't use each other via self)"
          else ""
        throw IllegalStateException(s"cyclic dependency: ${path.mkString(" -> ")}$hint")
      }
      inProgress((level, name)) = ()
      try compute
      finally inProgress.remove((level, name))
    }

    def resolve(level: Int, name: String): Any = definingLevel(level, name) match {
      case 0 =>
        fieldCache.getOrElse(name, guarded(0, name) {
          val value = base.base(name)(new Self[F] { def self = views(top) })
          fieldCache(name) = value
          value
        })
      case l =>
        layerCache.getOrElse(l, guarded(l, name) {
          val layer = layers(l - 1)
          val partial = layer.values(new Super[F] {
            def self = views(top)
            def upper = views(l - 1)
          })
          val values = base.names.zip(partial.asInstanceOf[Product].productIterator).collect { case (n, Some(v)) => n -> v }.toMap
          layerCache(l) = values
          values
        })(name)
    }

    Tuple.fromArray(base.names.map(views(top).selectDynamic).toArray[Any]).asInstanceOf[F]
  }
}
