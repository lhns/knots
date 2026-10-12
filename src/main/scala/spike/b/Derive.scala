package spike.b

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed:
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies the members it declares: an object literal `new { lazy val dep2 = ... }`
//   or an instance of some class. A generated subclass of A forwards those to it and the rest to
//   `upper`; a base (`isBase`) has no upper and must define every abstract member of A.
// The supplied members are checked against A at compile time.
object Derive {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val isBase = isBaseExpr.valueOrAbort
    if !target.typeSymbol.flags.is(Flags.Trait) then report.errorAndAbort(s"${target.show} must be a trait")

    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }
    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    // The argument, moved into the expansion with its Self/Super parameter replaced by `s`. An object
    // literal's type is widened to Object, so its class definition moves along to keep its type.
    val owner = Symbol.spliceOwner
    val moved = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => s.asTerm
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(strip(body))(owner).changeOwner(owner)
    val (literalClass, construct) = moved match {
      case Block(List(cd: ClassDef), e) => (List(cd), strip(e))
      case e => (Nil, e)
    }
    val partialType = construct.tpe.widen
    if partialType =:= TypeRepr.of[Object] then
      report.errorAndAbort(s"expected new { ... }, an instance of a class, or a ${target.show}", o)
    val partial = Symbol.newVal(owner, "partial", partialType, Flags.EmptyFlags, Symbol.noSymbol)

    // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
    val supplied = (partialType.typeSymbol.declaredFields ++ partialType.typeSymbol.declaredMethods).filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
    }.map(m => m.name -> m).toMap

    def resultType(t: TypeRepr): TypeRepr = t match {
      case ByNameType(r) => r
      case t => t
    }
    def member(name: String) = target.typeSymbol.methodMember(name).headOption
    val problems = supplied.toList.flatMap { (name, m) =>
      val pos = m.pos.getOrElse(o.asTerm.pos)
      member(name) match {
        case None => Some((s"$name is not a member of ${target.show}", pos))
        case Some(expected) =>
          val actual = resultType(partialType.memberType(m))
          val required = resultType(target.memberType(expected))
          Option.when(!(actual <:< required))((s"$name has type ${actual.show}, expected ${required.show}", pos))
      }
    }
    val traitMembers = target.typeSymbol.methodMembers.filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
        !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
    }
    val missing = traitMembers.filter(m => isBase && m.flags.is(Flags.Deferred) && !supplied.contains(m.name)).map(_.name)
    val allProblems =
      if missing.isEmpty then problems
      else problems :+ ((s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o.asTerm.pos))
    // Report every problem, but don't generate code from a mismatching partial.
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // A layer forwards everything it doesn't supply to `upper`; a base keeps A's concrete defaults.
    val upper = Option.when(!isBase)('{ ${ s.asExprOf[spike.b.Super[A]] }.upper }.asTerm)
    val implemented = traitMembers.filter(m => supplied.contains(m.name) || !isBase)
    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe),
      cls => implemented.map(m => Symbol.newMethod(cls, m.name, target.memberType(m), Flags.Override, Symbol.noSymbol)),
      selfType = None)
    val methods = cls.declaredMethods.map { meth =>
      DefDef(meth, argss => {
        val receiver = supplied.get(meth.name) match {
          case Some(m) => Select(Ref(partial), m)
          case None => Select(upper.get, member(meth.name).get)
        }
        val call = argss.foldLeft[Term](receiver) {
          case (fn, (targ: TypeTree) :: targs) => TypeApply(fn, targ :: targs.map(_.asInstanceOf[TypeTree]))
          case (fn, args) => Apply(fn, args.map(_.asInstanceOf[Term]))
        }
        Some(call.changeOwner(meth))
      })
    }
    val instance = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[A])
    val partialDef = ValDef(partial, Some(construct.changeOwner(partial)))
    Block(literalClass ++ List(partialDef, ClassDef(cls, parents, methods)), instance).asExprOf[A]
  }
}
