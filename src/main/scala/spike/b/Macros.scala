package spike.b

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed:
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies the members it declares: an object literal `new { def dep2 = ... }`
//   or an instance of some class. A generated subclass of A forwards those to it and the rest to
//   `upper`; a base (`isBase`) has no upper and must define every abstract member of A.
// The supplied members are checked against A at compile time.
object Macros {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.dealias.typeSymbol
    val isBase = isBaseExpr.valueOrAbort
    val owner = Symbol.spliceOwner
    if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"${target.show} must be a trait")

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

    // --- The members of A that a layer can supply. ---

    val traitMembers = targetSym.methodMembers.filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
        !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
    }
    targetSym.fieldMembers.find(_.flags.is(Flags.Deferred)).foreach { v =>
      report.errorAndAbort(s"abstract val ${v.name} in ${target.show} is not supported; declare it as def ${v.name}")
    }
    traitMembers.groupBy(_.name).collectFirst { case (n, ms) if ms.size > 1 =>
      report.errorAndAbort(s"overloaded member $n in ${target.show} is not supported")
    }
    val memberByName = traitMembers.map(m => m.name -> m).toMap

    // --- What `o` supplies. ---

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    // The argument, moved into the expansion with its Self/Upper parameter replaced by `s`. An object
    // literal's type is widened to Object, so its class definition moves along to keep its type.
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
    literalClass.foreach(checkLiteralMembers(_, o.asTerm.pos))
    val partialType = construct.tpe.widen
    if partialType =:= TypeRepr.of[Object] then
      report.errorAndAbort(s"expected new { ... }, an instance of a class, or a ${target.show}", o)
    val partial = Symbol.newVal(owner, "partial", partialType, Flags.EmptyFlags, Symbol.noSymbol)

    // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
    val supplied = (partialType.typeSymbol.declaredFields ++ partialType.typeSymbol.declaredMethods).filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
    }.map(m => m.name -> m).toMap

    // --- Checks: report every problem, but don't generate code from a mismatching argument. ---

    val problems = supplied.toList.flatMap { (name, m) =>
      val pos = m.pos.getOrElse(o.asTerm.pos)
      memberByName.get(name) match {
        case None => Some((s"$name is not a member of ${target.show}", pos))
        case Some(expected) =>
          val actual = resultType(partialType.memberType(m))
          val required = resultType(target.memberType(expected))
          Option.when(!(actual <:< required))((s"$name has type ${actual.show}, expected ${required.show}", pos))
      }
    }
    val missing = traitMembers.filter(m => isBase && m.flags.is(Flags.Deferred) && !supplied.contains(m.name)).map(_.name)
    val allProblems =
      if missing.isEmpty then problems
      else problems :+ ((s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o.asTerm.pos))
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // --- The generated class. ---

    // A layer forwards everything it doesn't supply to `upper`; a base keeps A's concrete defaults.
    val generated = traitMembers.filter(m => supplied.contains(m.name) || !isBase)
    // Supplied members without parameters are cached in a Cell, which also detects cycles.
    val cached = generated.filter(m => supplied.contains(m.name) && !hasParams(target.memberType(m)))
    def cellName(m: Symbol) = s"${m.name}$$cell"
    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      generated.map(m => Symbol.newMethod(cls, m.name, target.memberType(m), Flags.Override, Symbol.noSymbol)) ++
        cached.map(m => Symbol.newVal(cls, cellName(m), TypeRepr.of[Cell], Flags.Private, Symbol.noSymbol)),
      selfType = None)

    // The supplied member, or `upper`'s, called with the given arguments.
    def forward(m: Symbol, argss: List[List[Tree]]): Term = {
      val receiver = supplied.get(m.name) match {
        case Some(sm) => Select(Ref(partial), sm)
        case None => Select('{ ${ s.asExprOf[Upper[A]] }.upper }.asTerm, m)
      }
      applyArgs(receiver, argss)
    }
    def memberDefs(m: Symbol): List[Definition] = {
      val get = cls.declaredMethod(m.name).head
      if !cached.contains(m) then List(DefDef(get, argss => Some(forward(m, argss).changeOwner(get))))
      else {
        val cell = cls.declaredField(cellName(m))
        resultType(target.memberType(m)).asType match {
          case '[t] => List(
            ValDef(cell, Some('{ new Cell(${ Expr(m.name) }, () => ${ forward(m, Nil).asExprOf[t] }) }.asTerm.changeOwner(cell))),
            DefDef(get, _ => Some('{ ${ Select(This(cls), cell).asExprOf[Cell] }.get.asInstanceOf[t] }.asTerm.changeOwner(get))))
        }
      }
    }
    val instance = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[A])
    val partialDef = ValDef(partial, Some(construct.changeOwner(partial)))
    Block(literalClass ++ List(partialDef, ClassDef(cls, parents, generated.flatMap(memberDefs))), instance).asExprOf[A]
  }

  // An object literal is created while the knot is being tied, before `self` exists, so a strict val
  // that uses `self` would fail at runtime: its members must be defs or lazy vals (the layer caches them).
  private def checkLiteralMembers(using q: Quotes)(literal: q.reflect.ClassDef, fallback: q.reflect.Position): Unit = {
    import q.reflect.*
    literal.body.foreach {
      case v: ValDef
          if !v.symbol.flags.is(Flags.Lazy) && !v.symbol.flags.is(Flags.Private) && !v.symbol.flags.is(Flags.Protected) &&
            !v.symbol.flags.is(Flags.Synthetic) =>
        report.errorAndAbort(s"${v.name}: use def (or lazy val), the layer caches it", v.symbol.pos.getOrElse(fallback))
      case _ =>
    }
  }
}
