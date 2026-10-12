package spike.b

import scala.quoted.*

// Implements a trait from an object literal holding only some of its members, so no
// hand-written Delegate is needed. The literal's type is widened to Object, so both macros
// work on its tree: `check` validates its members against A, `derive` generates a class
// extending A whose members forward to the literal, or else to `upper`.
object Derive {
  // Every member of the object literal in `o` must be a member of A with a conforming type.
  // With `complete`, every abstract member of A must be defined too.
  inline def check[A, S](inline o: S ?=> Any, inline complete: Boolean): Unit = ${ checkImpl[A, S]('o, 'complete) }

  // The A for one layer: `o` itself if it already is an A, otherwise a generated subclass of A.
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline withUpper: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'withUpper) }

  private def checkImpl[A: Type, S: Type](o: Expr[S ?=> Any], complete: Expr[Boolean])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.typeSymbol
    if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"${target.show} must be a trait")
    if isInstance[A](o.asTerm) then return '{ () }

    val anon = literal(o.asTerm).map(_._2.symbol)
      .getOrElse(report.errorAndAbort(s"expected an object literal new { ... } or a ${target.show}", o))

    def resultType(t: TypeRepr): TypeRepr = t match {
      case ByNameType(r) => r
      case t => t
    }
    val members = publicMembers(anon)
    for m <- members do {
      val pos = m.pos.getOrElse(o.asTerm.pos)
      targetSym.methodMember(m.name).headOption match {
        case None => report.error(s"${m.name} is not a member of ${target.show}", pos)
        case Some(expected) =>
          val actualType = resultType(anon.typeRef.memberType(m))
          val expectedType = resultType(target.memberType(expected))
          if !(actualType <:< expectedType) then
            report.error(s"${m.name} has type ${actualType.show}, expected ${expectedType.show}", pos)
      }
    }
    if complete.valueOrAbort then {
      val defined = members.map(_.name).toSet
      val missing = targetSym.methodMembers.filter(s => s.flags.is(Flags.Deferred) && !defined(s.name)).map(_.name)
      if missing.nonEmpty then report.error(s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o)
    }
    '{ () }
  }

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], withUpper: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    if isInstance[A](o.asTerm) then return '{ $o(using $s).asInstanceOf[A] }

    val (param, literalDef, construct) = literal(o.asTerm).get // `check` already reported a missing literal

    // Re-home the literal: its `self`/`upper` given becomes `s`, its owner the expansion site.
    val substitute = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => s.asTerm
        case _ => super.transformTerm(t)(owner)
      }
    }
    val owner = Symbol.spliceOwner
    val (classDef, newLiteral) = substitute.transformTerm(Block(List(literalDef), construct))(owner).changeOwner(owner) match {
      case Block(List(cd: ClassDef), e) => (cd, e)
      case other => report.errorAndAbort(s"unexpected literal shape: ${other.show}")
    }
    val anon = classDef.symbol
    val partialSym = Symbol.newVal(owner, "partial", anon.typeRef, Flags.EmptyFlags, Symbol.noSymbol)
    val partialDef = ValDef(partialSym, Some(newLiteral.changeOwner(partialSym)))

    val defined = publicMembers(anon).map(m => m.name -> m).toMap
    val upper = Option.when(withUpper.valueOrAbort)('{ ${ s.asExprOf[spike.b.Super[A]] }.upper }.asTerm)
    // A layer forwards everything it doesn't define to `upper`; a base keeps A's concrete defaults.
    val implemented = forwardable(target).filter(m => defined.contains(m.name) || upper.nonEmpty || m.flags.is(Flags.Deferred))

    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe),
      cls => implemented.map(m => Symbol.newMethod(cls, m.name, target.memberType(m), Flags.Override, Symbol.noSymbol)),
      selfType = None)
    val body = cls.declaredMethods.map { meth =>
      DefDef(meth, argss => {
        val receiver = defined.get(meth.name) match {
          case Some(m) => Select(Ref(partialSym), m)
          case None => Select(upper.get, target.typeSymbol.methodMember(meth.name).head)
        }
        Some(argss.foldLeft[Term](receiver)((fn, args) => Apply(fn, args.map(_.asInstanceOf[Term]))).changeOwner(meth))
      })
    }
    val instance = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[A])
    Block(List(classDef, partialDef, ClassDef(cls, parents, body)), instance).asExprOf[A]
  }

  // The literal `new { ... }` inside the context-function argument: the function's parameter
  // (the given Self/Super), the literal's class and the expression constructing it.
  private def literal(using q: Quotes)(o: q.reflect.Term): Option[(q.reflect.Symbol, q.reflect.ClassDef, q.reflect.Term)] = {
    import q.reflect.*
    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }
    strip(o) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) =>
        strip(rhs) match {
          case Block(List(cd: ClassDef), construct) => Some((p.symbol, cd, strip(construct)))
          case _ => None
        }
      case _ => None
    }
  }

  private def isInstance[A: Type](using q: Quotes)(o: q.reflect.Term): Boolean = {
    import q.reflect.*
    def lambdaBody(t: Term): Option[Term] = t match {
      case Inlined(_, _, e) => lambdaBody(e)
      case Block(List(d: DefDef), _: Closure) => d.rhs
      case _ => None
    }
    lambdaBody(o).exists(_.tpe.widen <:< TypeRepr.of[A])
  }

  private def publicMembers(using q: Quotes)(cls: q.reflect.Symbol): List[q.reflect.Symbol] = {
    import q.reflect.*
    (cls.declaredFields ++ cls.declaredMethods).filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
    }
  }

  // Members of A a generated class may implement: everything but Any/Object's and final ones.
  private def forwardable(using q: Quotes)(target: q.reflect.TypeRepr): List[q.reflect.Symbol] = {
    import q.reflect.*
    target.typeSymbol.methodMembers.filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) &&
        !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic) &&
        m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
    }
  }
}
