package spike.a

import scala.NamedTuple.{AnyNamedTuple, DropNames, Names}
import scala.compiletime.{constValue, erasedValue, error}

// Evidence that O is a valid override literal for F: F has no reserved field names, no field of O is a
// raw layer (it needs module(...)), and every field of O exists in F with a conforming type (like SubsetOf,
// but a View field also takes a Module). The module rules are A's own; SubsetOf stays reusable without them (ADR 0029).
infix sealed trait OverrideOf[O <: AnyNamedTuple, F <: AnyNamedTuple]

// The same for a base literal: O must also supply every field of F, and a View field takes a
// BaseModule (a complete layer), not a Module that needs a layer below.
infix sealed trait BaseOf[O <: AnyNamedTuple, F <: AnyNamedTuple]

private object evidence extends OverrideOf[AnyNamedTuple, AnyNamedTuple] with BaseOf[AnyNamedTuple, AnyNamedTuple]

object OverrideOf {
  inline given derived[O <: AnyNamedTuple, F <: AnyNamedTuple]: OverrideOf[O, F] = {
    checkLiteral[O, F, false]
    evidence.asInstanceOf[OverrideOf[O, F]]
  }
}

object BaseOf {
  inline given derived[O <: AnyNamedTuple, F <: AnyNamedTuple]: BaseOf[O, F] = {
    checkLiteral[O, F, true]
    checkComplete[O, F]
    evidence.asInstanceOf[BaseOf[O, F]]
  }
}

private inline def checkLiteral[O <: AnyNamedTuple, F <: AnyNamedTuple, Base <: Boolean]: Unit = {
  rejectReservedNames[F]
  rejectRawLayers[Names[O], DropNames[O]]
  checkFields[F, Names[O], DropNames[O], Base]
}

private inline def rejectRawLayers[Ns <: Tuple, Vs <: Tuple]: Unit =
  inline erasedValue[(Ns, Vs)] match {
    case _: (EmptyTuple, EmptyTuple) => ()
    case _: ((n *: ns), (v *: vs)) =>
      inline if SubsetOf.conforms[v, Override[?]] then
        error("field " + constValue[n] + " is a layer: wrap it in module(...)")
      else rejectRawLayers[ns, vs]
  }

// SubsetOf's check, with the module rule instead of a plain conformance test. It is a per-field test
// rather than a match type over the field types, so a field whose type is itself a named tuple is fine.
private inline def checkFields[F <: AnyNamedTuple, Ns <: Tuple, Vs <: Tuple, Base <: Boolean]: Unit =
  inline erasedValue[(Ns, Vs)] match {
    case _: (EmptyTuple, EmptyTuple) => ()
    case _: ((n *: ns), (v *: vs)) =>
      inline if !constValue[Tuple.Contains[Names[F], n]] then
        error("unknown field: " + constValue[n])
      else {
        checkField[v, FieldType[F, n], n, Base]
        checkFields[F, ns, vs, Base]
      }
  }

// A field of type View[s] is a module: a base takes a View or a BaseModule, an override also a Module.
// (The error is raised in each branch: a Boolean result of an inline match isn't a constant.)
private inline def checkField[V, T, N, Base <: Boolean]: Unit =
  inline erasedValue[T] match {
    case _: View[s] =>
      inline if constValue[Base] then
        inline if !SubsetOf.conforms[V, View[s] | BaseModule[s]] then error("wrong type for field " + constValue[N])
        else ()
      else
        inline if !SubsetOf.conforms[V, View[s] | Module[s]] then error("wrong type for field " + constValue[N])
        else ()
    case _ =>
      inline if !SubsetOf.conforms[V, T] then error("wrong type for field " + constValue[N])
      else ()
  }

// Fields of F that O doesn't supply, in F's order.
private type Missing[Fs <: Tuple, Os <: Tuple] <: Tuple = Fs match {
  case EmptyTuple => EmptyTuple
  case f *: rest => Tuple.Contains[Os, f] match {
    case true => Missing[rest, Os]
    case false => f *: Missing[rest, Os]
  }
}

private inline def checkComplete[O <: AnyNamedTuple, F <: AnyNamedTuple]: Unit =
  inline erasedValue[Missing[Names[F], Names[O]]] match {
    case _: EmptyTuple => ()
    case _: (m *: ms) => error("missing fields: " + joinNames[m *: ms])
  }

private inline def joinNames[Ns <: Tuple]: String =
  inline erasedValue[Ns] match {
    case _: (n *: EmptyTuple) => "" + constValue[n]
    case _: (n *: ns) => "" + constValue[n] + ", " + joinNames[ns]
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
