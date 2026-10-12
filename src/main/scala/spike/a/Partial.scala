package spike.a

import scala.NamedTuple.{AnyNamedTuple, DropNames, Names}
import scala.compiletime.{constValue, constValueTuple, erasedValue, error, summonFrom}

// The type of field N in F; doesn't reduce if F has no field N.
type FieldType[F <: AnyNamedTuple, N] = FieldTypeOf[Names[F], DropNames[F], N]

type FieldTypeOf[Ns <: Tuple, Vs <: Tuple, N] <: Any = (Ns, Vs) match {
  case (N *: _, v *: _) => v
  case (_ *: ns, _ *: vs) => FieldTypeOf[ns, vs, N]
}

// Evidence that every field of O exists in F with a conforming type.
infix sealed trait SubsetOf[O <: AnyNamedTuple, F <: AnyNamedTuple]

object SubsetOf {
  private object instance extends SubsetOf[AnyNamedTuple, AnyNamedTuple]

  inline given derived[O <: AnyNamedTuple, F <: AnyNamedTuple]: SubsetOf[O, F] = {
    check[F, Names[O], DropNames[O]]
    instance.asInstanceOf[SubsetOf[O, F]]
  }

  private inline def check[F <: AnyNamedTuple, Ns <: Tuple, Vs <: Tuple]: Unit =
    inline erasedValue[(Ns, Vs)] match {
      case _: (EmptyTuple, EmptyTuple) => ()
      case _: ((n *: ns), (v *: vs)) =>
        inline if !constValue[Tuple.Contains[Names[F], n]] then
          error("unknown field: " + constValue[n])
        else inline if !conforms[v, FieldType[F, n]] then
          error("wrong type for field " + constValue[n])
        else check[F, ns, vs]
    }

  // Separate so V and T are type parameters: lowercase names in a summonFrom pattern would bind fresh types.
  private[a] inline def conforms[V, T]: Boolean = summonFrom {
    case _: (V <:< T) => true
    case _ => false
  }
}

inline def fieldNames[T <: AnyNamedTuple]: List[String] =
  constValueTuple[Names[T]].toList.asInstanceOf[List[String]]

// F's shape with every field optional.
type Partial[F <: AnyNamedTuple] = NamedTuple.Map[F, Option]

object Partial {
  // `Partial[F]` fixes F; the builder's `apply` then infers the literal's type O.
  def apply[F <: AnyNamedTuple]: PartialBuilder[F] = PartialBuilder()
}

final class PartialBuilder[F <: AnyNamedTuple] {
  // Fields of `o` become Some, the rest of F's fields None.
  inline def apply[O <: AnyNamedTuple](o: O)(using O SubsetOf F): Partial[F] = {
    val present = fieldNames[O].zip(o.asInstanceOf[Product].productIterator).toMap
    Tuple.fromArray(fieldNames[F].map(present.get).toArray[Any]).asInstanceOf[Partial[F]]
  }
}
