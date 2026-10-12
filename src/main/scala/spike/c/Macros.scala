package spike.c

import scala.collection.mutable.ListBuffer
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
  // Builds one layer of an A from the object literal `o` = `new A { override lazy val x = ... }`.
  // - The literal is kept as is, in a `partial` val (its constructor gets `u` as the Upper).
  // - A generated subclass of A (also given `u`) overrides every member the literal declares: a
  //   member without parameters is cached in a Cell (computed once, cycles detected), the rest
  //   forwards to `partial`. Members the literal doesn't declare keep A's `= inherited` default.
  // - A strict `override val` would be evaluated when `partial` is created, before the knot is tied
  //   and outside any Cell. So its initializer is replaced by a placeholder and moved into a thunk
  //   that the Cell runs on first access: such a val is effectively lazy.
  // Compile-time checks: the literal's shape; the base overrides every member and doesn't use
  // `upper`; no member is used bare (`dep2` instead of `self.dep2` would skip overrides in subclass
  // layers); the trait is a pure interface whose members are all `= inherited` (see below).
  //
  // Members of A that have a body must be exactly `= inherited`: any other body uses this, the layer's
  // own object, not self, so overrides in later layers wouldn't reach it. That is checked from the
  // trait's tree, which exists only when the trait is compiled in the same run; for a trait from a
  // compiled library the check can't run.
  inline def derive[A](inline o: Upper[A] ?=> A, u: Upper[A], inline isBase: Boolean): A =
    ${ deriveImpl[A]('o, 'u, 'isBase) }

  private def deriveImpl[A: Type](o: Expr[Upper[A] ?=> A], u: Expr[Upper[A]], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    def sh(t: TypeRepr): String = t.show(using Printer.TypeReprShortCode)

    val target = TypeRepr.of[A]
    val targetName = sh(target)
    val dealiased = target.dealias
    val targetSym = dealiased.typeSymbol
    val isBase = isBaseExpr.valueOrAbort
    val owner = Symbol.spliceOwner
    val argPos = o.asTerm.pos
    val literalHint = s"expected an object literal: new $targetName { ... }"

    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }
    def resultType(t: TypeRepr): TypeRepr = t match {
      case ByNameType(r) => r
      case t => t
    }
    def hasParams(t: TypeRepr) = t match {
      case _: MethodType | _: PolyType => true
      case _ => false
    }
    def applyArgs(fn: Term, argss: List[List[Tree]]): Term = argss.foldLeft(fn) {
      case (fn, (targ: TypeTree) :: targs) => TypeApply(fn, targ :: targs.map(_.asInstanceOf[TypeTree]))
      case (fn, args) => Apply(fn, args.map(_.asInstanceOf[Term]))
    }

    // --- The argument: the context function's parameter and body. ---

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort(literalHint, argPos)
    }
    strip(body) match {
      case Literal(NullConstant()) => report.errorAndAbort(s"null is not supported: $literalHint", body.pos)
      case _ =>
    }
    val literal = strip(body) match {
      case Block(List(cd: ClassDef), _) if cd.symbol.typeRef <:< target => cd
      case _ => report.errorAndAbort(literalHint, argPos)
    }
    val anon = literal.symbol

    // --- The target trait. ---

    dealiased match {
      case _: Refinement => report.errorAndAbort(s"$targetName is a refinement type; only a plain trait is supported", argPos)
      case _ =>
    }
    if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"$targetName must be a trait", argPos)

    val targetProblems = ListBuffer.empty[String]
    val bases = dealiased.baseClasses
    val notAnyRef = Set(defn.AnyClass, defn.ObjectClass, defn.MatchableClass)
    bases.find(c => c != targetSym && !c.flags.is(Flags.Trait) && !notAnyRef(c)).foreach { c =>
      targetProblems += s"${targetSym.name} extends the class ${c.name}; only traits are supported"
    }
    val ctor = targetSym.primaryConstructor
    val upperClass = TypeRepr.of[Upper].typeSymbol
    val termParams = if ctor.exists then ctor.paramSymss.flatten.filter(_.isTerm) else Nil
    if termParams.length != 1 || termParams.head.termRef.widen.typeSymbol != upperClass then
      targetProblems += s"${targetSym.name} must be declared with exactly one constructor parameter, `using Upper[${targetSym.name}]`" +
        (if termParams.length > 1 then " (extra constructor parameters are not supported)" else "")
    val selfType = try targetSym.tree match {
      case ClassDef(_, _, _, Some(self), _) if !(self.tpt.tpe =:= targetSym.typeRef) => true
      case _ => false
    } catch { case _: Exception => false }
    val all = (targetSym.methodMembers ++ targetSym.fieldMembers).distinct.filterNot(m => notAnyRef(m.owner))
    // A member of the self type's classes: its owner is not a base class of the trait.
    if selfType || all.exists(m => !bases.contains(m.owner)) then
      targetProblems += s"${targetSym.name} has a self-type, which is not supported"

    def mentionsThis(t: TypeRepr): Boolean = t match {
      case ThisType(tr) => dealiased.baseClasses.contains(tr.typeSymbol)
      case AppliedType(tc, args) => mentionsThis(tc) || args.exists(mentionsThis)
      case MethodType(_, ps, r) => ps.exists(mentionsThis) || mentionsThis(r)
      case PolyType(_, _, r) => mentionsThis(r)
      case ByNameType(u) => mentionsThis(u)
      case AndType(a, b) => mentionsThis(a) || mentionsThis(b)
      case OrType(a, b) => mentionsThis(a) || mentionsThis(b)
      case AnnotatedType(u, _) => mentionsThis(u)
      case Refinement(p, _, i) => mentionsThis(p) || mentionsThis(i)
      // `this.T` (a type parameter or member of the trait) is fine, only `this.type` itself isn't.
      case TypeRef(_: ThisType, _) | TermRef(_: ThisType, _) => false
      case TypeRef(q, _) => mentionsThis(q)
      case TermRef(q, _) => mentionsThis(q)
      case _ => false
    }
    // Only from the trait's tree, as the member's type already has `this.type` replaced; without a
    // tree (a trait from a library) the compiler's own error is left.
    def memberMentionsThis(m: Symbol): Boolean =
      try m.tree match {
        case d: DefDef =>
          mentionsThis(d.returnTpt.tpe) || d.paramss.exists(_.params.exists { case v: ValDef => mentionsThis(v.tpt.tpe); case _ => false })
        case v: ValDef => mentionsThis(v.tpt.tpe)
        case _ => false
      } catch { case _: Exception => false }

    val inheritedSym = Symbol.requiredMethod("spike.c.inherited")
    def isInheritedCall(t: Tree): Boolean = t match {
      case Inlined(Some(call), _, _) => isInheritedCall(call)
      case Inlined(None, Nil, e) => isInheritedCall(e)
      case Typed(e, _) => isInheritedCall(e)
      case Block(Nil, e) => isInheritedCall(e)
      case TypeApply(f, _) => isInheritedCall(f)
      case Apply(f, _) => isInheritedCall(f)
      case t: Term => t.symbol == inheritedSym
      case _ => false
    }
    // Needs the trait's code: available with -Yretain-trees (set in build.sbt), also for traits from
    // libraries compiled to TASTy. Without it (None) the member can't be checked.
    def isInheritedMember(m: Symbol): Option[Boolean] =
      try m.tree match {
        case d: DefDef if d.rhs.nonEmpty => Some(d.rhs.exists(isInheritedCall))
        case _: ValDef => Some(false)
        case _ => None
      } catch { case _: Exception => None }

    // The trait's own members (public or protected), in the order the compiler lists them.
    val members = all.filter(m => !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.isType && !m.flags.is(Flags.ParamAccessor))
    if targetProblems.isEmpty then {
      // An abstract var is a getter and a setter.
      val abstractVars = members.filter(m => m.flags.is(Flags.Deferred) && m.flags.is(Flags.Mutable) && !m.name.endsWith("_=")).map(_.name).toSet
      abstractVars.toList.sorted.foreach { v =>
        targetProblems += s"abstract var $v in ${members.find(_.name == v).get.owner.name} is not supported; declare it as def $v = inherited"
      }
      val defaults = members.filter(_.name.contains("$default$")).map(_.name.takeWhile(_ != '$')).distinct
      defaults.sorted.foreach(n => targetProblems += s"$n has default arguments, which are not supported")
      members.filterNot(m => m.flags.is(Flags.Synthetic) || m.name.contains("$default$") || abstractVars(m.name) ||
          abstractVars(m.name.stripSuffix("_=")) || (m.name.endsWith("_=") && members.exists(_.name == m.name.stripSuffix("_=")))).foreach { m =>
        if !m.flags.is(Flags.Deferred) then {
          if !m.isDefDef || isInheritedMember(m).contains(false) then
            targetProblems += s"concrete member ${m.name} in ${m.owner.name} must be exactly `def ${m.name}: ... = inherited`: " +
              "any other body uses this, not self, so it isn't late-bound; move it into the base"
        }
        else if m.flags.is(Flags.Protected) then
          targetProblems += s"abstract protected member ${m.name} in ${m.owner.name} is not supported"
        else if m.isValDef then
          targetProblems += s"abstract val ${m.name} in ${m.owner.name} is not supported; declare it as def ${m.name}"
        else if memberMentionsThis(m) then
          targetProblems += s"member ${m.name} in ${m.owner.name} has a type that mentions this.type, which is not supported"
      }
      val overloaded = members.filterNot(_.name.contains("$default$")).groupBy(_.name).collect { case (n, ms) if ms.size > 1 => n }
      overloaded.toList.sorted.foreach(n => targetProblems += s"overloaded member $n in $targetName is not supported")
    }
    if targetProblems.nonEmpty then {
      targetProblems.foreach(report.error(_, argPos))
      return '{ ??? }
    }

    // --- Checks of the literal. ---

    var failed = false
    def fail(msg: String, pos: Position): Unit = { failed = true; report.error(msg, pos) }

    val publicMembers = members.filter(m => !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic))
    val memberNames = publicMembers.map(_.name).toSet
    def publicDeclared(cls: Symbol) = (cls.declaredMethods ++ cls.declaredFields).filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
    }
    val declared = publicDeclared(anon)
    val declaredByName = declared.groupBy(_.name)

    if isBase then {
      val missing = publicMembers.map(_.name).filterNot(declaredByName.contains)
      if missing.nonEmpty then
        fail(s"the base must override every member of $targetName; missing: ${missing.distinct.sorted.mkString(", ")}", literal.pos)
    }
    declared.filter(_.flags.is(Flags.Mutable)).map(_.name.stripSuffix("_=")).distinct.filter(memberNames).foreach { v =>
      fail(s"var $v: use lazy val or def", argPos)
    }

    val upperSym = Symbol.requiredModule("spike.c.Overridable$package").methodMember("upper").head
    def isOwnMember(sym: Symbol) = memberNames(sym.name) && (sym.owner == anon || target.baseClasses.contains(sym.owner))
    new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case Select(qual @ This(_), name) if qual.symbol == anon && isOwnMember(tree.symbol) =>
            fail(s"use self.$name: a bare $name skips overrides in subclass layers", tree.pos)
          case Ident(name) if tree.symbol.isTerm && isOwnMember(tree.symbol) =>
            fail(s"use self.$name: a bare $name skips overrides in subclass layers", tree.pos)
          case _: Ident if isBase && tree.symbol == upperSym =>
            fail("the base has no upper", tree.pos)
          case _ =>
        }
        super.traverseTree(tree)(owner)
      }
    }.traverseTree(literal)(anon)
    if failed then return '{ ??? }

    // --- The generated code. ---

    // The argument, moved into the expansion with its Upper parameter replaced by `u`.
    val moved = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => u.asTerm
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(strip(body))(owner).changeOwner(owner)
    val (movedClass, construct) = moved match {
      case Block(List(cd: ClassDef), e) => (cd, strip(e))
      case _ => report.errorAndAbort(literalHint, argPos)
    }

    // The members A has and the literal declares (cached), or that must be implemented (the
    // abstract ones); the rest keep A's `= inherited` default.
    final case class Field(m: Symbol, memberType: TypeRepr, impl: Option[Symbol], isCached: Boolean)
    // Taken from the moved class: moving copies its symbols (changeOwner), so the literal's own
    // would no longer be members of `partial`.
    val movedByName = publicDeclared(movedClass.symbol).groupBy(_.name)
    val fields = publicMembers.filter(m => !m.name.contains("$default$") && !m.name.endsWith("_=")).flatMap { m =>
      val impl = movedByName.get(m.name).flatMap(_.find(!_.name.endsWith("_=")))
      if impl.isEmpty && !m.flags.is(Flags.Deferred) then None
      else {
        val tpe = target.memberType(m)
        Some(Field(m, tpe, impl, impl.nonEmpty && !hasParams(tpe)))
      }
    }
    // Strict vals of the literal that a Cell must compute lazily.
    val strictVals = fields.filter(_.isCached).flatMap(_.impl).filter(s => s.isValDef && !s.flags.is(Flags.Lazy) && !s.flags.is(Flags.Mutable))
    val strictIndex = strictVals.zipWithIndex.toMap

    val thunksSym = Option.when(strictVals.nonEmpty)(
      Symbol.newVal(owner, "thunks", TypeRepr.of[Array[() => Any]], Flags.EmptyFlags, Symbol.noSymbol))
    val thunksDef = thunksSym.map(s => ValDef(s, Some('{ new Array[() => Any](${ Expr(strictVals.length) }) }.asTerm.changeOwner(s))))
    def thunks: Expr[Array[() => Any]] = Ref(thunksSym.get).asExprOf[Array[() => Any]]

    val classDef =
      if strictVals.isEmpty then movedClass
      else {
        val newBody = movedClass.body.map {
          case v: ValDef if strictIndex.contains(v.symbol) && v.rhs.nonEmpty =>
            val i = strictIndex(v.symbol)
            v.tpt.tpe.widen.asType match {
              case '[t] =>
                // The value is a placeholder: the Cell computes the real one through the thunk.
                val rhs = '{ $thunks(${ Expr(i) }) = () => ${ v.rhs.get.asExprOf[t] }; null.asInstanceOf[t] }
                ValDef.copy(v)(v.name, v.tpt, Some(rhs.asTerm.changeOwner(v.symbol)))
            }
          case s => s
        }
        ClassDef.copy(movedClass)(movedClass.name, movedClass.constructor, movedClass.parents, movedClass.self, newBody)
      }

    val partialSym = Symbol.newVal(owner, "partial", construct.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
    val partialDef = ValDef(partialSym, Some(construct.changeOwner(partialSym)))
    val partial = Ref(partialSym)

    // A name no member of A has, so a cell can't collide with a member.
    val taken = members.map(_.name).toSet
    val cellNames = fields.map { f =>
      var n = s"${f.m.name}$$cell"
      while taken(n) do n += "$"
      f.m.name -> n
    }.toMap

    val ctorCall = {
      val targs = dealiased match {
        case AppliedType(_, args) => args
        case _ => Nil
      }
      val sel = Select(New(TypeTree.of[A]), ctor)
      val applied = Apply(if targs.isEmpty then sel else sel.appliedToTypes(targs), List(u.asTerm))
      // A trait's constructor has an empty parameter list after its parameters.
      applied.tpe.widen match {
        case MethodType(Nil, Nil, _) => Apply(applied, Nil)
        case _ => applied
      }
    }
    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      fields.flatMap { f =>
        Symbol.newMethod(cls, f.m.name, f.memberType, Flags.Override, Symbol.noSymbol) ::
          Option.when(f.isCached)(Symbol.newVal(cls, cellNames(f.m.name), TypeRepr.of[Cell], Flags.Private, Symbol.noSymbol)).toList
      },
      selfType = None)

    val memberDefs = fields.flatMap { f =>
      val name = f.m.name
      val get = cls.declaredMethod(name).head
      val impl = f.impl.getOrElse(f.m)
      if !f.isCached then List(DefDef(get, argss => Some(applyArgs(Select(partial, impl), argss).changeOwner(get))))
      else {
        // The strict val's initializer lives in its thunk; anything else is read from the literal.
        val compute: Term = strictIndex.get(impl) match {
          case Some(i) => '{ $thunks(${ Expr(i) })() }.asTerm
          case None => Select(partial, impl)
        }
        val cell = cls.declaredField(cellNames(name))
        resultType(f.memberType).asType match {
          case '[t] => List(
            ValDef(cell, Some('{ new Cell(${ Expr(name) }, () => ${ compute.asExprOf[Any] }) }.asTerm.changeOwner(cell))),
            DefDef(get, _ => Some('{ ${ Select(This(cls), cell).asExprOf[Cell] }.get.asInstanceOf[t] }.asTerm.changeOwner(get))))
        }
      }
    }
    val derivedDef = ClassDef(cls, List(TypeTree.of[Object], ctorCall), memberDefs)

    val instance = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[A])
    Block(thunksDef.toList ++ List(classDef, partialDef, derivedDef), instance).asExprOf[A]
  }
}
