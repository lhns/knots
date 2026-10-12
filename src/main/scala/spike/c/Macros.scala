package spike.c

import scala.quoted.*

// Default body of a member of an overridable trait: forwards to the same member of `upper`.
//   trait MainDependencies(using Upper[MainDependencies]) { def dep1: Dependency = inherited }
// Only the result type is a parameter: with a `using Upper[A]` here, T would be inferred as Nothing.
inline def inherited[T]: T = ${ Macros.inheritedImpl[T] }

object Macros {
  def inheritedImpl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    def enclosingMember(sym: Symbol): Symbol =
      if sym.isNoSymbol then report.errorAndAbort("inherited must be the body of a trait member")
      else if (sym.isDefDef || sym.isValDef) && sym.owner.isClassDef then sym
      else enclosingMember(sym.owner)

    val member = enclosingMember(Symbol.spliceOwner)
    if member.owner.methodMember(member.name).size > 1 then
      report.errorAndAbort(s"inherited doesn't support the overloaded member ${member.name}")
    // The trait's own type with its type parameters applied (typeRef alone would be unapplied).
    val upper = TypeRepr.of[Upper].appliedTo(This(member.owner).tpe.widen).asType match {
      case '[s] => Expr.summon[s] match {
        case Some(s) => Select.unique(s.asTerm, "upper")
        case None => report.errorAndAbort(s"inherited needs a given Upper[${member.owner.name}], e.g. a `using` trait parameter")
      }
    }
    val call = Select.unique(upper, member.name)
    if !(call.tpe.widenTermRefByName <:< TypeRepr.of[T]) then
      report.errorAndAbort(s"upper.${member.name} has type ${call.tpe.widenTermRefByName.show}, expected ${TypeRepr.of[T].show}")
    call.asExprOf[T]
  }

  // `o` must be an object literal `new A { ... }`. It must not reference A's members bare
  // (`dep2` instead of `self.dep2` would skip overrides in subclass layers). A base must also
  // override every member of A and can't use `upper`.
  inline def checkLiteral[A](inline o: Upper[A] ?=> A, inline base: Boolean): Unit =
    ${ checkLiteralImpl[A]('o, 'base) }

  private def checkLiteralImpl[A: Type](o: Expr[Upper[A] ?=> A], base: Expr[Boolean])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.typeSymbol
    val isBase = base.valueOrAbort
    val literal = objectLiteral(o.asTerm) match {
      case Some(cd) if cd.symbol.typeRef <:< target => cd
      case _ => report.errorAndAbort(s"expected an object literal: new ${target.show} { ... }", o)
    }
    val anon = literal.symbol

    // A's members, inherited ones included (but not Any's/Object's).
    val members = (targetSym.methodMembers ++ targetSym.fieldMembers).filter { m =>
      !m.isClassConstructor && !(m.flags.is(Flags.Private) || m.flags.is(Flags.Protected) ||
        m.flags.is(Flags.Synthetic) || m.flags.is(Flags.ParamAccessor)) &&
        m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
    }
    members.groupBy(_.name).collectFirst { case (n, ms) if ms.size > 1 =>
      report.errorAndAbort(s"overloaded member $n in ${target.show} is not supported")
    }
    val memberNames = members.map(_.name).toSet
    val upperSym = Symbol.requiredModule("spike.c.Overridable$package").methodMember("upper").head

    if isBase then {
      val overridden = (anon.declaredMethods ++ anon.declaredFields).map(_.name).toSet
      val missing = members.map(_.name).filterNot(overridden)
      if missing.nonEmpty then
        report.error(s"the base must override every member of ${target.show}; missing: ${missing.sorted.mkString(", ")}", literal.pos)
    }

    def isOwnMember(sym: Symbol) = memberNames(sym.name) && (sym.owner == anon || target.baseClasses.contains(sym.owner))

    new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case Select(qual @ This(_), name) if qual.symbol == anon && isOwnMember(tree.symbol) =>
            report.error(s"use self.$name: a bare $name skips overrides in subclass layers", tree.pos)
          case Ident(name) if tree.symbol.isTerm && isOwnMember(tree.symbol) =>
            report.error(s"use self.$name: a bare $name skips overrides in subclass layers", tree.pos)
          case _: Ident if isBase && tree.symbol == upperSym =>
            report.error("the base has no upper", tree.pos)
          case _ =>
        }
        super.traverseTree(tree)(owner)
      }
    }.traverseTree(literal)(anon)

    '{ () }
  }

  // The object literal `new A { ... }` that is the body of the context-function argument.
  private def objectLiteral(using q: Quotes)(o: q.reflect.Term): Option[q.reflect.ClassDef] = {
    import q.reflect.*
    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }
    strip(o) match {
      case Block(List(DefDef(_, _, _, Some(body))), _: Closure) => strip(body) match {
        case Block(List(cd: ClassDef), _) => Some(cd)
        case _ => None
      }
      case _ => None
    }
  }
}
