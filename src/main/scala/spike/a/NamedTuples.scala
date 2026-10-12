package spike.a

import cats.Monoid

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.implicitNotFound

// Type safety lives at the literal boundary: `Defs` types a base literal, `SubsetOf` (via `OverrideOf`)
// checks an override literal. Past that, everything is string-keyed (field names, `Map[String, Any]`,
// `View.selectDynamic`) and cast back to what the literals already guaranteed, by design. The only macro
// splits an override literal into per-field closures (Macros.scala); there is no generated code.

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
type Defs[F <: AnyNamedTuple] = Part[F, F]

// The fields P of F, each deferred like a field of Defs[F]. A base literal over 22 fields doesn't get
// per-field types, so write parts with Overridable.part and join them with `++` (fields stay lazy,
// self sees all of Deps):
//   type Deps = NamedTuple.Concat[P1, P2]
//   Overridable[Deps](Overridable.part[Deps, P1]((f1 = ..., ...)) ++ Overridable.part[Deps, P2]((f13 = ..., ...)))
type Part[F <: AnyNamedTuple, P <: AnyNamedTuple] = NamedTuple.Map[P, [X] =>> Self[F] ?=> X]

// The fields of F, each computed on first access (`view.dep2`), also while the knot is being tied.
class View[F <: AnyNamedTuple](resolve: String => Any) extends Selectable {
  type Fields = F

  def selectDynamic(name: String): Any = resolve(name)
}

extension [F <: AnyNamedTuple](view: View[F])
  // Computes every field.
  inline def toTuple: F =
    Tuple.fromArray(fieldNames[F].map(view.selectDynamic).toArray[Any]).asInstanceOf[F]

// The overridden fields of one override, by name. `thunks(upper)` has one computation per name, in
// order; the knot caches each in its own Cell, so a field is computed on its own and may use the
// others via `self`.
final case class Layer[F <: AnyNamedTuple](names: List[String], thunks: Upper[F] => List[() => Any])

object Layer {
  // A literal split by the macro (Macros.scala): one closure per field.
  def perField[F <: AnyNamedTuple](names: List[String], fns: List[Upper[F] => Any]): Layer[F] =
    Layer(names, u => fns.map(f => () => f(u)))

  // Fallback for an override that isn't a literal (a named-tuple value, `++`, a variable): its value
  // is computed as a whole, once per knot, and each field reads its element. So the fields of such an
  // override can't use each other via `self` (the cycle error names the whole).
  def whole[F <: AnyNamedTuple](names: List[String], value: Upper[F] => Product): Layer[F] =
    Layer(names, u => {
      val all = Cell(s"${names.mkString(", ")} (layer, evaluated as a whole)", () => value(u))
      names.indices.toList.map(i => () => all.get.asInstanceOf[Product].productElement(i))
    })
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
    new Override(Vector(Macros.layer[F, O](o)))
}

final class Overridable[F <: AnyNamedTuple](
  val fields: Map[String, Self[F] => Any],
  layers: Vector[Layer[F]]
) extends Override[F](layers)

// How a knot gets its `self` and what is below its first layer.
private[a] enum Knot[F <: AnyNamedTuple] {
  // The top-level knot: its own result is `self`.
  case Root(base: Map[String, Self[F] => Any])
  // A complete module: `self` is the parent's final view of the module field.
  case Complete(base: Map[String, Self[F] => Any], self: () => View[F])
  // An override module has no base: level 0 is the parent's view of the layer below.
  case Passing(self: () => View[F], upper: () => View[F])
}

object Overridable {
  // A part of a base over 22 fields (see Part). Returns its argument; the parameter type is what
  // defers each field.
  def part[F <: AnyNamedTuple, P <: AnyNamedTuple](fields: Part[F, P]): Part[F, P] = fields

  inline def apply[F <: AnyNamedTuple](defs: Defs[F]): Overridable[F] = {
    rejectReservedNames[F]
    val fns = defs.toTuple.toList.map(f => (s: Self[F]) => f.asInstanceOf[Self[F] ?=> Any](using s))
    new Overridable(fieldNames[F].zip(fns).toMap, Vector.empty)
  }

  // Layering on a base keeps it a base: `base |+| o1 |+| o2`. A base on the right replaces everything.
  extension [F <: AnyNamedTuple](base: Overridable[F]) def |+|(ovr: Override[F]): Overridable[F] = ovr match {
    case replacement: Overridable[F] => replacement
    case _ => new Overridable(base.fields, base.layers ++ ovr.layers)
  }

  def build[F <: AnyNamedTuple](base: Overridable[F], overrides: Override[F]*): View[F] = {
    val all = base |+| Monoid.combineAll(overrides)
    tie("", all.layers, Knot.Root(all.fields))
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

    // Only a complete module can stand in a base; a Module (override) has no layer below to wrap.
    def baseField(name: String, value: Any): Any = value match {
      case m: Module[?] if !m.isInstanceOf[BaseModule[?]] =>
        throw IllegalStateException(s"module($name) wraps an Override, which needs a layer below; a base takes a complete module")
      case v => focus(v, name, noLayerBelow)
    }

    lazy val baseCells = (knot match {
      case Knot.Root(base) => base
      case Knot.Complete(base, _) => base
      case Knot.Passing(_, _) => Map.empty[String, Self[F] => Any]
    }).map { (name, fn) =>
      name -> Cell(s"$prefix$name (base)", () => baseField(name, fn(new Self[F] { def self = finalView })))
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
