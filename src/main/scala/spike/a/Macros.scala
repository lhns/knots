package spike.a

import scala.NamedTuple.AnyNamedTuple
import scala.quoted.*

// Splits a literal `(dep2 = ..., dep3 = ...)` into one closure `C => Any` per field, where C is the
// context the literal is written in (`Self[F]` for a base, `Upper[F]` for an override). A field is
// computed on its own (Split.perField) and the fields of one literal can use each other via `self`.
// The argument is typed as one context function, but its element expressions are separate trees
// inside the lambda: each is moved into a closure with the lambda's parameter replaced by the
// closure's. Anything that is not a literal (a named-tuple value, `++`, a variable) is evaluated as
// a whole instead (Split.whole). The literal is recognized as in spike D.
object Macros {
  inline def split[C, O <: AnyNamedTuple](inline o: C ?=> O): Split[C] =
    ${ splitImpl[C, O]('o) }

  private def splitImpl[C: Type, O <: AnyNamedTuple: Type](o: Expr[C ?=> O])(using Quotes): Expr[Split[C]] = {
    import quotes.reflect.*

    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }

    // A named tuple type as (names, values), seen through aliases and match types.
    def namedTupleParts(t: TypeRepr): Option[(TypeRepr, TypeRepr)] = t.widen match {
      case AppliedType(tc, List(names, values)) if tc.typeSymbol.name == "NamedTuple" => Some((names, values))
      case w =>
        val d = w.dealiasKeepOpaques
        if d != w then namedTupleParts(d)
        else { val simple = w.simplified; if simple != w then namedTupleParts(simple) else None }
    }
    def tupleElems(t: TypeRepr): Option[List[TypeRepr]] = t.dealias match {
      case AppliedType(tc, List(h, tail)) if tc.typeSymbol == Symbol.requiredClass("scala.*:") => tupleElems(tail).map(h :: _)
      case AppliedType(tc, args) if tc.typeSymbol.fullName.matches("scala\\.Tuple\\d+") => Some(args)
      case d if d =:= TypeRepr.of[EmptyTuple] => Some(Nil)
      case d =>
        val simple = d.simplified
        if simple != d then tupleElems(simple) else None
    }
    def fieldNamesOf(t: TypeRepr): Option[List[String]] =
      namedTupleParts(t).flatMap((names, _) => tupleElems(names)).flatMap { ns =>
        val strings = ns.collect { case ConstantType(StringConstant(n)) => n }
        Option.when(strings.length == ns.length)(strings)
      }

    // A named-tuple literal desugars to `x$proxy = TupleN.apply(e1, ..., en)` (TupleXXL.apply(Seq(..))
    // from 23 elements) followed by an ascription of that proxy to the NamedTuple type, nested in
    // Inlined/Block nodes. The tuple is found by following the final ascription to its proxy.
    def spine(t: Term): (List[Statement], List[Statement], Term) = t match {
      case Inlined(_, bindings, e) => val (stats, binds, last) = spine(e); (stats, bindings ++ binds, last)
      case Block(stats, e) => val (more, binds, last) = spine(e); (stats ++ more, binds, last)
      case last => (Nil, Nil, last)
    }
    def unwrapTuple(t: Term): Term = t match {
      case Inlined(_, Nil, e) => unwrapTuple(e)
      case Typed(e, _) => unwrapTuple(e)
      case TypeApply(Select(e, "$asInstanceOf$"), _) => unwrapTuple(e)
      case t => t
    }
    // Some(Left(statement)) for a statement before the literal, Some(Right(elements)), or None if
    // it is not a literal.
    def literal(t: Term, arity: Int): Option[Either[Statement, List[Term]]] = {
      val (stats, bindings, last) = spine(t)
      last match {
        case Typed(e, tpt) if namedTupleParts(tpt.tpe).isDefined =>
          strip(e) match {
            case id: Ident =>
              bindings.collectFirst { case v: ValDef if v.symbol == id.symbol => v }.flatMap(_.rhs).flatMap { rhs =>
                unwrapTuple(rhs) match {
                  case Apply(fn, List(Typed(Repeated(args, _), _))) if fn.symbol.owner.fullName.endsWith("TupleXXL$") => Some(args)
                  case Apply(fn, args) if fn.symbol.owner.fullName.matches("scala\\.Tuple\\d+\\$") => Some(args)
                  case _ => None
                }
              }.filter(_.length == arity).map { elements =>
                val own = (b: Statement) => b match {
                  case v: ValDef => v.symbol == id.symbol || v.name.contains("$proxy") || v.name == "NamedTuple$_this"
                  case _ => false
                }
                (stats ++ bindings.filterNot(own)).headOption match {
                  case Some(stat) => Left(stat)
                  case None => Right(elements)
                }
              }
            case _ => None
          }
        case _ => None
      }
    }

    val whole = '{ Split.whole[C](fieldNames[O], u => $o(using u).asInstanceOf[Product]) }

    strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(body))), _: Closure) =>
        // A literal's own type sees through the opaque NamedTuple, so take it from the final ascription.
        val argType = spine(body)._3 match {
          case Typed(_, tpt) if namedTupleParts(tpt.tpe).isDefined => tpt.tpe
          case _ => body.tpe.widen
        }
        fieldNamesOf(argType).flatMap(names => literal(body, names.length).map(names -> _)) match {
          case Some((_, Left(stat))) =>
            report.errorAndAbort("statements before the named tuple are not supported; put them inside the fields", stat.pos)
          case Some((names, Right(elements))) =>
            val closures = elements.map { e =>
              Lambda(Symbol.spliceOwner, MethodType(List("u"))(_ => List(TypeRepr.of[C]), _ => TypeRepr.of[Any]), (meth, params) => {
                val u = params.head.asInstanceOf[Term]
                val moved = new TreeMap {
                  override def transformTerm(t: Term)(owner: Symbol): Term = t match {
                    case id: Ident if id.symbol == p.symbol => u
                    case _ => super.transformTerm(t)(owner)
                  }
                }.transformTerm(e)(meth)
                Typed(moved, TypeTree.of[Any]).changeOwner(meth)
              }).asExprOf[C => Any]
            }
            '{ Split.perField[C](${ Expr(names) }, ${ Expr.ofList(closures) }) }
          case None => whole
        }
      case _ => whole
    }
  }
}
