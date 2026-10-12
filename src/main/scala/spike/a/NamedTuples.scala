package spike.a

import cats.Monoid

import scala.NamedTuple.AnyNamedTuple
import scala.collection.mutable

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

  // `x |+| y` layers y below x: y overrides x, so y's `upper` is x's result.
  given [F <: AnyNamedTuple]: Monoid[Override[F]] with {
    def empty: Override[F] = new Override(Vector.empty)
    def combine(x: Override[F], y: Override[F]): Override[F] = new Override(x.layers ++ y.layers)
  }
}

final class OverrideBuilder[F <: AnyNamedTuple] {
  inline def apply[O <: AnyNamedTuple](o: Super[F] ?=> O)(using O SubsetOf F): Override[F] = {
    new Override(Vector(Layer[F](fieldNames[O], s => Partial[F](o(using s)))))
  }
}

final class Overridable[F <: AnyNamedTuple](
  val names: List[String],
  val base: Map[String, Self[F] => Any],
  val overrides: Override[F]
)

object Overridable {
  inline def apply[F <: AnyNamedTuple](defs: Defs[F]): Overridable[F] = {
    val fns = defs.toTuple.toList.map(f => (s: Self[F]) => f.asInstanceOf[Self[F] ?=> Any](using s))
    new Overridable(fieldNames[F], fieldNames[F].zip(fns).toMap, Monoid.empty)
  }

  def apply[F <: AnyNamedTuple](base: Overridable[F], ovr: Override[F], more: Override[F]*): Overridable[F] =
    new Overridable(base.names, base.base, Monoid.combineAll(base.overrides +: ovr +: more))

  // Level 0 is the base, level i > 0 is layers(i - 1); `upper` of level i is level i - 1.
  def build[F <: AnyNamedTuple](base: Overridable[F], overrides: Override[F]*): F = {
    val knot = Overridable(base, Monoid.combineAll(overrides))
    val layers = knot.overrides.layers
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
          val value = knot.base(name)(new Self[F] { def self = views(top) })
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
          val values = knot.names.zip(partial.asInstanceOf[Product].productIterator).collect { case (n, Some(v)) => n -> v }.toMap
          layerCache(l) = values
          values
        })(name)
    }

    Tuple.fromArray(knot.names.map(views(top).selectDynamic).toArray[Any]).asInstanceOf[F]
  }
}
