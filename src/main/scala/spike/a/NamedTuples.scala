package spike.a

import cats.Monoid

import scala.NamedTuple.{AnyNamedTuple, Names}
import scala.annotation.implicitNotFound
import scala.compiletime.constValueTuple

@implicitNotFound("self is only available inside an Overridable[...](...) or Override[...](...) literal")
trait Self[F <: AnyNamedTuple] {
  def self: View[F]
}

@implicitNotFound("upper is only available in an Override; a base (Overridable) has no layer below")
trait Upper[F <: AnyNamedTuple] extends Self[F] {
  def upper: View[F]
}

inline def self[F <: AnyNamedTuple](using s: Self[F]): View[F] = s.self

inline def upper[F <: AnyNamedTuple](using s: Upper[F]): View[F] = s.upper

// Every field of F, deferred until `self` exists.
// A field that is a View of another named tuple is a module: it is supplied with module(...) (see Modules.scala).
type Defs[F <: AnyNamedTuple] = NamedTuple.Map[F, [X] =>> Self[F] ?=> BaseField[X]]

// The fields of F, each computed on first access (`view.dep2`), also while the knot is being tied.
final class View[F <: AnyNamedTuple](resolve: String => Any) extends Selectable {
  type Fields = F

  def selectDynamic(name: String): Any = resolve(name)
}

extension [F <: AnyNamedTuple](view: View[F])
  // Computes every field.
  inline def toTuple: F =
    Tuple.fromArray(fieldNames[F].map(view.selectDynamic).toArray[Any]).asInstanceOf[F]

// A layer is evaluated as a unit: its fields can't reference each other via `self`.
// `values` has the overridden fields only, by name.
final case class Layer[F <: AnyNamedTuple](names: List[String], values: Upper[F] => Map[String, Any])

final class Override[F <: AnyNamedTuple](val layers: Vector[Layer[F]])

object Override {
  // `Override[F]` fixes F; the builder's `apply` then infers the literal's type O.
  def apply[F <: AnyNamedTuple]: OverrideBuilder[F] = OverrideBuilder()

  // `x |+| y` is y extending x: y's `upper` (its super) is x's result.
  given [F <: AnyNamedTuple]: Monoid[Override[F]] with {
    def empty: Override[F] = new Override(Vector.empty)
    def combine(x: Override[F], y: Override[F]): Override[F] = new Override(x.layers ++ y.layers)
  }
}

final class OverrideBuilder[F <: AnyNamedTuple] {
  inline def apply[O <: AnyNamedTuple](o: Upper[F] ?=> O)(using O SubsetOf F): Override[F] = {
    new Override(Vector(Layer[F](fieldNames[O], s => {
      val partial = Partial[F](o(using s))
      fieldNames[F].zip(partial.asInstanceOf[Product].productIterator).collect { case (n, Some(v)) => n -> v }.toMap
    })))
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

  // Layering on a base keeps it a base: `base |+| o1 |+| o2`.
  extension [F <: AnyNamedTuple](base: Overridable[F]) def |+|(ovr: Override[F]): Overridable[F] =
    new Overridable(base.names, base.base, Monoid.combine(base.overrides, ovr))

  def build[F <: AnyNamedTuple](base: Overridable[F], overrides: Override[F]*): View[F] = {
    val knot = base |+| Monoid.combineAll(overrides)
    tie("", knot.base, knot.overrides.layers, null, null)
  }

  // Level 0 is the base, level i > 0 is layers(i - 1); `upper` of level i is level i - 1. Nothing is
  // computed here: each base field and each layer is a Cell, computed on first access. A layer is
  // computed as a whole, so its fields can't use each other via `self`.
  //
  // A module (Modules.scala) is tied by the same code, focused through its field of the parent:
  // `outerSelf` is the parent's final view of that field (the module's `self`), `outerUpper` the
  // view of the layer below (level 0 of an Override module has no base and passes through to it).
  // Module values found in a base field or a layer are tied this way when the field is computed.
  private[a] def tie[F <: AnyNamedTuple](
    prefix: String,
    base: Map[String, Self[F] => Any] | Null,
    layers: Vector[Layer[F]],
    outerSelf: (() => Any) | Null,
    outerUpper: (() => Any) | Null
  ): View[F] = {
    val top = layers.length
    lazy val views: Vector[View[F]] = Vector.tabulate(top + 1)(level => View[F](resolve(level, _)))
    def finalView: View[F] = if outerSelf == null then views(top) else outerSelf().asInstanceOf[View[F]]

    def definingLevel(level: Int, name: String): Int =
      if level == 0 || layers(level - 1).names.contains(name) then level else definingLevel(level - 1, name)

    def resolve(level: Int, name: String): Any = definingLevel(level, name) match {
      case 0 if base != null => baseCells(name).get
      case 0 if outerUpper != null => outerUpper().asInstanceOf[View[F]].selectDynamic(name)
      case 0 => throw new IllegalStateException(s"no value for $prefix$name: an Override module has no base")
      case l => layerCells(l - 1).get.asInstanceOf[Map[String, Any]](name)
    }

    // A module's sub-knot sees the final view of its field as `self` and the layer below's as `upper`.
    def focus(value: Any, name: String, upper: (() => Any) | Null): Any = value match {
      case m: Module[?] => m.tie(s"$prefix$name.", () => finalView.selectDynamic(name), upper)
      case v => v
    }

    lazy val baseCells = base.nn.map { (name, fn) =>
      name -> Cell(s"$prefix$name (base)", () => focus(fn(new Self[F] { def self = finalView }), name, null))
    }
    lazy val layerCells = layers.zipWithIndex.map { (layer, i) =>
      val l = i + 1
      Cell(s"$prefix${layer.names.mkString(", ")} (layer $l)", () => {
        layer.values(new Upper[F] {
          def self = finalView
          def upper = views(l - 1)
        }).map { (n, v) => n -> focus(v, n, () => views(l - 1).selectDynamic(n)) }
      }, " (a layer is evaluated as a unit; its fields can't use each other via self)")
    }

    views(top)
  }
}
