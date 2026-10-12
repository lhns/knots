package spike.a

import scala.NamedTuple.{AnyNamedTuple, DropNames, Names}
import scala.compiletime.{constValue, erasedValue, error}

// Evidence that O is a valid override literal for F: F has no reserved field names, no field of O is a
// raw layer (it needs module(...)), and O is a SubsetOf F's override field types (modules allowed).
// The module rules are A's own; SubsetOf stays reusable without them (ADR 0029).
infix sealed trait OverrideOf[O <: AnyNamedTuple, F <: AnyNamedTuple]

type OverrideFields[F <: AnyNamedTuple] = NamedTuple.Map[F, [X] =>> OverrideField[X]]

object OverrideOf {
  private object instance extends OverrideOf[AnyNamedTuple, AnyNamedTuple]

  inline given derived[O <: AnyNamedTuple, F <: AnyNamedTuple]: OverrideOf[O, F] = {
    rejectReservedNames[F]
    rejectRawLayers[Names[O], DropNames[O]]
    val _ = SubsetOf.derived[O, OverrideFields[F]]
    instance.asInstanceOf[OverrideOf[O, F]]
  }

  private inline def rejectRawLayers[Ns <: Tuple, Vs <: Tuple]: Unit =
    inline erasedValue[(Ns, Vs)] match {
      case _: (EmptyTuple, EmptyTuple) => ()
      case _: ((n *: ns), (v *: vs)) =>
        inline if SubsetOf.conforms[v, Override[?]] then
          error("field " + constValue[n] + " is a layer: wrap it in module(...)")
        else rejectRawLayers[ns, vs]
    }
}

// Members of View / Selectable / the toTuple extension win over selectDynamic, so a field with one of
// these names could never be read as `view.name`.
inline def rejectReservedNames[F <: AnyNamedTuple]: Unit = rejectReserved[Names[F]]

private inline def rejectReserved[Ns <: Tuple]: Unit =
  inline erasedValue[Ns] match {
    case _: EmptyTuple => ()
    case _: (n *: ns) =>
      inline constValue[n] match {
        case "selectDynamic" | "toTuple" | "Fields" | "toString" | "hashCode" | "equals" | "getClass" |
            "wait" | "notify" | "notifyAll" | "eq" | "ne" | "synchronized" | "asInstanceOf" | "isInstanceOf" =>
          error("reserved field name: " + constValue[n] + " is a member of View")
        case _ => rejectReserved[ns]
      }
  }
