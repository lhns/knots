package spike.a

import cats.Monoid

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.implicitNotFound

// Type safety lives at the literal boundary: `BaseOf` checks a base literal, `OverrideOf` an override
// literal (Checks.scala). Past that, everything is string-keyed (field names, `View.selectDynamic`) and
// cast back to what the literals already guaranteed, by design. The only macro splits a base or
// override literal into per-field closures (Macros.scala); there is no generated code.

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

// The fields of F, each computed on first access (`view.dep2`), also while the knot is being tied.
final class View[F <: AnyNamedTuple](resolve: String => Any) extends Selectable {
  type Fields = F

  def selectDynamic(name: String): Any = resolve(name)
}

extension [F <: AnyNamedTuple](view: View[F])
  // Computes every field.
  inline def toTuple: F =
    Tuple.fromArray(fieldNames[F].map(view.selectDynamic).toArray[Any]).asInstanceOf[F]

// The fields of one literal, by name, each deferred until its context C exists (`Self[F]` for a base,
// `Upper[F]` for an override). `thunks(c)` has one computation per name, in order; the knot caches each
// in its own Cell, so a field is computed on its own and may use the others via `self`.
final case class Split[C](names: List[String], thunks: C => List[() => Any])

type Layer[F <: AnyNamedTuple] = Split[Upper[F]]

object Split {
  // A literal split by the macro (Macros.scala): one closure per field.
  def perField[C](names: List[String], fns: List[C => Any]): Split[C] =
    Split(names, c => fns.map(f => () => f(c)))

  // Fallback for a literal that isn't one (a named-tuple value, `++`, a variable): its value is computed
  // as a whole, once per knot, and each field reads its element. So the fields of such a value can't use
  // each other via `self` (the cycle error names the whole).
  def whole[C](names: List[String], value: C => Product): Split[C] =
    Split(names, c => {
      val all = Cell(s"${names.mkString(", ")} (evaluated as a whole)", () => value(c))
      names.indices.toList.map(i => () => all.get.asInstanceOf[Product].productElement(i))
    })
}

final class OverridableBuilder[F <: AnyNamedTuple] {
  // Like an override literal, but it must supply every field of F (BaseOf) and has no `upper`.
  inline def apply[O <: AnyNamedTuple](inline o: Self[F] ?=> O)(using BaseOf[O, F]): Overridable[F] =
    new Overridable(Macros.split[Self[F], O](o), Vector.empty)
}

// Layers on top of whatever is below. An Overridable (a base, ADR 0008) is an Override that brings
// its own fields, so it stands wherever an override can: layered on something, it replaces everything below.
class Override[F <: AnyNamedTuple](val layers: Vector[Layer[F]])

object Override {
  // `Override[F]` fixes F; the builder's `apply` then infers the literal's type O.
  def apply[F <: AnyNamedTuple]: OverrideBuilder[F] = OverrideBuilder()

  // `x |+| y` is y extending x: y's `upper` (its super) is x's result.
  given [F <: AnyNamedTuple]: Monoid[Override[F]] with {
    def empty: Override[F] = new Override(Vector.empty)
    def combine(x: Override[F], y: Override[F]): Override[F] = (x, y) match {
      case (base: Overridable[F], _) => base |+| y
      case (_, base: Overridable[F]) => base
      case _ => new Override(x.layers ++ y.layers)
    }
  }
}

final class OverrideBuilder[F <: AnyNamedTuple] {
  // The macro splits a literal `(dep2 = ..., dep3 = ...)` into one closure per field.
  inline def apply[O <: AnyNamedTuple](inline o: Upper[F] ?=> O)(using OverrideOf[O, F]): Override[F] =
    new Override(Vector(Macros.split[Upper[F], O](o)))
}

final class Overridable[F <: AnyNamedTuple](val base: Split[Self[F]], layers: Vector[Layer[F]]) extends Override[F](layers)

// How a knot gets its `self` and what is below its first layer.
private[a] enum Knot[F <: AnyNamedTuple] {
  // The top-level knot: its own result is `self`.
  case Root(base: Split[Self[F]])
  // A complete module: `self` is the parent's final view of the module field.
  case Complete(base: Split[Self[F]], self: () => View[F])
  // An override module has no base: level 0 is the parent's view of the layer below.
  case Passing(self: () => View[F], upper: () => View[F])
}

object Overridable {
  // `Overridable[F]` fixes F; the builder's `apply` then infers the literal's type O.
  def apply[F <: AnyNamedTuple]: OverridableBuilder[F] = OverridableBuilder()

  // Layering on a base keeps it a base: `base |+| o1 |+| o2`. A base on the right replaces everything.
  extension [F <: AnyNamedTuple](base: Overridable[F]) def |+|(ovr: Override[F]): Overridable[F] = ovr match {
    case replacement: Overridable[F] => replacement
    case _ => new Overridable(base.base, base.layers ++ ovr.layers)
  }

  def build[F <: AnyNamedTuple](base: Overridable[F], overrides: Override[F]*): View[F] = {
    val all = base |+| Monoid.combineAll(overrides)
    tie("", all.layers, Knot.Root(all.base))
  }

  private val noLayerBelow: () => Nothing = () => throw new IllegalStateException("there is no layer below the base")

  // Level 0 is the base, level i > 0 is layers(i - 1); `upper` of level i is level i - 1. Nothing is
  // computed here: each base field and each field of each layer is a Cell, computed on first access.
  //
  // A module (Modules.scala) is tied by the same code, focused through its field of the parent (see
  // Knot). Module values found in a base field or a layer are tied this way when the field is computed.
  private[a] def tie[F <: AnyNamedTuple](prefix: String, layers: Vector[Layer[F]], knot: Knot[F]): View[F] = {
    val top = layers.length
    lazy val views: Vector[View[F]] = Vector.tabulate(top + 1)(level => View[F](resolve(level, _)))
    def finalView: View[F] = knot match {
      case Knot.Root(_) => views(top)
      case Knot.Complete(_, self) => self()
      case Knot.Passing(self, _) => self()
    }

    def definingLevel(level: Int, name: String): Int =
      if level == 0 || layers(level - 1).names.contains(name) then level else definingLevel(level - 1, name)

    def resolve(level: Int, name: String): Any = definingLevel(level, name) match {
      case 0 => knot match {
        case Knot.Passing(_, upper) => upper().selectDynamic(name)
        case _ => baseCells(name).get
      }
      case l => layerCells(l - 1)(name).get
    }

    // A module's sub-knot sees the final view of its field as `self` and the layer below's as `upper`.
    def focus(value: Any, name: String, upper: () => View[F]): Any = value match {
      case m: Module[s] =>
        m.tie(s"$prefix$name.", () => finalView.selectDynamic(name).asInstanceOf[View[s]],
          () => upper().selectDynamic(name).asInstanceOf[View[s]])
      case v => v
    }

    lazy val baseCells: Map[String, Cell] = {
      val base = knot match {
        case Knot.Root(base) => base
        case Knot.Complete(base, _) => base
        case Knot.Passing(_, _) => Split[Self[F]](Nil, _ => Nil)
      }
      val self = new Self[F] { def self = finalView }
      base.names.zip(base.thunks(self)).map { (name, thunk) =>
        name -> Cell(s"$prefix$name (base)", () => focus(thunk(), name, noLayerBelow))
      }.toMap
    }
    // One upper per level, shared by the fields of its layer.
    lazy val uppers = Vector.tabulate(top + 1)(l => new Upper[F] {
      def self = finalView
      def upper = views(l - 1)
    })
    lazy val layerCells: Vector[Map[String, Cell]] = layers.zipWithIndex.map { (layer, i) =>
      val l = i + 1
      layer.names.zip(layer.thunks(uppers(l))).map { (n, thunk) =>
        n -> Cell(s"$prefix$n (layer $l)", () => focus(thunk(), n, () => views(l - 1)))
      }.toMap
    }

    views(top)
  }
}
