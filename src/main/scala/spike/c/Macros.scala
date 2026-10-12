package spike.c

import scala.quoted.*

// Default body of a member of an overridable trait: forwards to the same member of `upper`.
//   trait MainDependencies(using Super[MainDependencies]) { def dep1: Dependency = inherited }
// Only the result type is a parameter: with a `using Super[A]` here, T would be inferred as Nothing.
inline def inherited[T]: T = ${ Macros.inheritedImpl[T] }

object Macros {
  def inheritedImpl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    def enclosingMember(sym: Symbol): Symbol =
      if sym.isNoSymbol then report.errorAndAbort("inherited must be the body of a trait member")
      else if (sym.isDefDef || sym.isValDef) && sym.owner.isClassDef then sym
      else enclosingMember(sym.owner)

    val member = enclosingMember(Symbol.spliceOwner)
    val upper = TypeRepr.of[spike.c.Super].appliedTo(member.owner.typeRef).asType match {
      case '[s] => Expr.summon[s] match {
        case Some(s) => Select.unique(s.asTerm, "upper")
        case None => report.errorAndAbort(s"inherited needs a given Super[${member.owner.name}], e.g. a `using` trait parameter")
      }
    }
    val call = Select.unique(upper, member.name)
    if !(call.tpe.widenTermRefByName <:< TypeRepr.of[T]) then
      report.errorAndAbort(s"upper.${member.name} has type ${call.tpe.widenTermRefByName.show}, expected ${TypeRepr.of[T].show}")
    call.asExprOf[T]
  }

  // `o` must be an object literal `new A { ... }`. It must not reference A's members bare
  // (`dep2` instead of `self.dep2` would skip overrides layered on top). A base must also
  // override every member of A and can't use `upper`.
  inline def checkLiteral[A](inline o: Super[A] ?=> A, inline base: Boolean): Unit =
    ${ checkLiteralImpl[A]('o, 'base) }

  private def checkLiteralImpl[A: Type](o: Expr[Super[A] ?=> A], base: Expr[Boolean])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.typeSymbol
    val isBase = base.valueOrAbort

    val firstClass = new TreeAccumulator[Option[ClassDef]] {
      def foldTree(found: Option[ClassDef], tree: Tree)(owner: Symbol): Option[ClassDef] =
        found.orElse(tree match {
          case cd: ClassDef => Some(cd)
          case _ => foldOverTree(None, tree)(owner)
        })
    }
    val literal = firstClass.foldTree(None, o.asTerm)(Symbol.spliceOwner) match {
      case Some(cd) if cd.symbol.typeRef <:< target => cd
      case _ => report.errorAndAbort(s"expected an object literal: new ${target.show} { ... }", o)
    }
    val anon = literal.symbol

    val members = (targetSym.declaredMethods ++ targetSym.declaredFields).filter { m =>
      !m.isClassConstructor && !(m.flags.is(Flags.Private) || m.flags.is(Flags.Protected) ||
        m.flags.is(Flags.Synthetic) || m.flags.is(Flags.ParamAccessor))
    }
    val memberNames = members.map(_.name).toSet

    if isBase then {
      val overridden = (anon.declaredMethods ++ anon.declaredFields).map(_.name).toSet
      val missing = members.map(_.name).filterNot(overridden)
      if missing.nonEmpty then
        report.error(s"the base must override every member of ${target.show}; missing: ${missing.sorted.mkString(", ")}", literal.pos)
    }

    def isOwnMember(sym: Symbol) = memberNames(sym.name) && (sym.owner == anon || sym.owner == targetSym)

    new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case Select(qual @ This(_), name) if qual.symbol == anon && isOwnMember(tree.symbol) =>
            report.error(s"use self.$name: a bare $name skips overrides layered on top", tree.pos)
          case Ident(name) if tree.symbol.isTerm && isOwnMember(tree.symbol) =>
            report.error(s"use self.$name: a bare $name skips overrides layered on top", tree.pos)
          case Ident("upper") if isBase && tree.symbol.owner.fullName.startsWith("spike.c") =>
            report.error("the base has no upper", tree.pos)
          case _ =>
        }
        super.traverseTree(tree)(owner)
      }
    }.traverseTree(literal)(anon)

    '{ () }
  }
}
