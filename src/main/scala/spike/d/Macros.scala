package spike.d

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait, or a
// View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)` (each field becomes its
//   own lazy val), an object literal `new { lazy val dep2 = ... }`, an instance of some class, or a
//   named-tuple value. A generated subclass of A takes those and forwards the rest to `upper`; a base
//   (`isBase`) has no upper and must define every abstract member of A.
// The supplied fields are checked against A at compile time.
object Macros {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val isBase = isBaseExpr.valueOrAbort
    if !target.dealias.typeSymbol.flags.is(Flags.Trait) then report.errorAndAbort(s"${target.show} must be a trait")

    def strip(t: Term): Term = t match {
      case Inlined(_, Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case t => t
    }
    def resultType(t: TypeRepr): TypeRepr = t match {
      case ByNameType(r) => r
      case t => t
    }
    def tupleElems(t: TypeRepr): List[TypeRepr] = t.dealias match {
      case AppliedType(tc, List(h, tail)) if tc.typeSymbol == Symbol.requiredClass("scala.*:") => h :: tupleElems(tail)
      case AppliedType(tc, args) if tc.typeSymbol.fullName.startsWith("scala.Tuple") => args
      case _ => Nil
    }
    // Matched before dealiasing: the desugared literal's type sees through the opaque NamedTuple.
    def namedTupleFields(t: TypeRepr): Option[List[(String, TypeRepr)]] = t.widen match {
      case AppliedType(tc, List(names, values)) if tc.typeSymbol.name == "NamedTuple" =>
        Some(tupleElems(names).collect { case ConstantType(StringConstant(n)) => n }.zip(tupleElems(values)))
      case w =>
        val d = w.dealiasKeepOpaques
        if d == w then None else namedTupleFields(d)
    }

    // The fields A can take: a View[F]'s are F's; a trait's are its overridable members.
    val viewFields = target.baseType(TypeRepr.of[View].typeSymbol) match {
      case AppliedType(_, List(f)) => namedTupleFields(f)
      case _ => None
    }
    val traitMembers = target.dealias.typeSymbol.methodMembers.filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
        !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
    }
    // name -> (required type, required in a base)
    val fields: Map[String, (TypeRepr, Boolean)] = viewFields match {
      case Some(fs) => fs.map((n, t) => n -> (t, true)).toMap
      case None => traitMembers.map(m => m.name -> (resultType(target.memberType(m)), m.flags.is(Flags.Deferred))).toMap
    }

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    // The argument, moved into the expansion with its Self/Upper parameter replaced by `s`.
    val owner = Symbol.spliceOwner
    val moved = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => s.asTerm
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(strip(body))(owner).changeOwner(owner)

    // Where a supplied field's value comes from.
    enum Source {
      case Element(term: Term) // a named-tuple literal's element: becomes the field's own lazy val
      case Member(sym: Symbol) // a member of `partial`
      case Index(i: Int) // an element of the named-tuple value `partial`
    }
    // What `o` supplies, name -> (type, position, source), plus the `partial` value if one is needed.
    // A named-tuple literal desugars to `x$proxy = TupleN.apply(e1, ..., en)` followed by an ascription
    // to the NamedTuple type, nested in Inlined/Block nodes. Only that spine is searched.
    def spine(t: Term): (List[Statement], Term) = t match {
      case Inlined(_, bindings, e) => val (stats, last) = spine(e); (bindings ++ stats, last)
      case Block(stats, e) => val (more, last) = spine(e); (stats ++ more, last)
      case last => (Nil, last)
    }
    val literalFields = spine(body)._2 match {
      case Typed(_, tpt) => namedTupleFields(tpt.tpe)
      case _ => None
    }
    def literalElements(arity: Int): Option[List[Term]] = spine(moved)._1.collectFirst {
      case ValDef(_, _, Some(Apply(fn, args)))
        if fn.symbol.name == "apply" && fn.symbol.owner.fullName == s"scala.Tuple$arity$$" && args.length == arity => args
    }

    type Supplied = (Map[String, (TypeRepr, Position, Source)], Option[(Symbol, Term)], List[Statement])
    val inputs: Supplied =
      literalFields.orElse(namedTupleFields(body.tpe)) match {
        case Some(fs) =>
          literalElements(fs.length) match {
            case Some(terms) =>
              (fs.zip(terms).map { case ((n, t), e) => n -> (t, e.pos, Source.Element(e)) }.toMap, None, Nil)
            case None => // not a literal: read the fields from the tuple value
              val partial = Symbol.newVal(owner, "partial", moved.tpe.widen, Flags.Lazy, Symbol.noSymbol)
              (fs.zipWithIndex.map { case ((n, t), i) => n -> (t, o.asTerm.pos, Source.Index(i)) }.toMap, Some((partial, moved)), Nil)
          }
        case None =>
          // An object literal's type is widened to Object, so its class definition moves along to keep its type.
          val (literalClass, construct) = moved match {
            case Block(List(cd: ClassDef), e) => (List(cd), strip(e))
            case e => (Nil, e)
          }
          val partialType = construct.tpe.widen
          if partialType =:= TypeRepr.of[Object] then
            report.errorAndAbort(s"expected (name = ...), new { ... }, an instance of a class, or a ${target.show}", o)
          val partial = Symbol.newVal(owner, "partial", partialType, Flags.Lazy, Symbol.noSymbol)
          // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
          val members = (partialType.typeSymbol.declaredFields ++ partialType.typeSymbol.declaredMethods).filter { m =>
            !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
          }
          (members.map(m => m.name -> (resultType(partialType.memberType(m)), m.pos.getOrElse(o.asTerm.pos), Source.Member(m))).toMap,
            Some((partial, construct)), literalClass)
      }

    val (supplied, partialDef, literalClass) = inputs
    val problems = supplied.toList.flatMap { case (name, (actual, pos, _)) =>
      fields.get(name) match {
        case None => Some((s"$name is not a member of ${target.show}", pos))
        case Some((required, _)) => Option.when(!(actual <:< required))((s"$name has type ${actual.show}, expected ${required.show}", pos))
      }
    }
    val missing = fields.collect { case (name, (_, true)) if isBase && !supplied.contains(name) => name }
    val allProblems =
      if missing.isEmpty then problems
      else problems :+ ((s"missing members of ${target.show}: ${missing.toList.sorted.mkString(", ")}", o.asTerm.pos))
    // Report every problem, but don't generate code from a mismatching partial.
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    val partialRef = partialDef.map((sym, _) => Ref(sym))
    val upper = Option.when(!isBase)('{ ${ s.asExprOf[Upper[A]] }.upper }.asTerm)
    def fromSource(source: Source, tpe: TypeRepr): Term = source match {
      case Source.Element(term) => term
      case Source.Member(m) => Select(partialRef.get, m)
      case Source.Index(i) => tpe.asType match {
        case '[t] => '{ ${ partialRef.get.asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }).asInstanceOf[t] }.asTerm
      }
    }
    def guard(cls: Symbol, name: String, value: Term): Term = value.tpe.widen.asType match {
      case '[t] => '{ Cycles.guard[t](${ This(cls).asExprOf[AnyRef] }, ${ Expr(name) })(${ value.asExprOf[t] }) }.asTerm
    }
    def applyArgs(fn: Term, argss: List[List[Tree]]): Term = argss.foldLeft(fn) {
      case (fn, (targ: TypeTree) :: targs) => TypeApply(fn, targ :: targs.map(_.asInstanceOf[TypeTree]))
      case (fn, args) => Apply(fn, args.map(_.asInstanceOf[Term]))
    }
    def isElement(name: String) = supplied.get(name).exists(_._3.isInstanceOf[Source.Element])

    // Every generated member is a def running through the cycle guard. A named-tuple element is cached
    // in a private lazy val read inside the guard: with the guard inside the lazy val, re-entry would
    // wait on the lazy val instead of reaching the guard.
    def declare(cls: Symbol, name: String, memberType: TypeRepr, flags: Flags): List[Symbol] = {
      val get = Symbol.newMethod(cls, name, memberType, flags, Symbol.noSymbol)
      if isElement(name) then List(get, Symbol.newVal(cls, s"$name$$cached", resultType(memberType), Flags.Lazy | Flags.Private, Symbol.noSymbol))
      else List(get)
    }
    def define(cls: Symbol, name: String, value: List[List[Tree]] => Term): List[Definition] = {
      val get = cls.declaredMethod(name).head
      val cached = cls.declaredField(s"$name$$cached")
      if cached.exists then List(
        ValDef(cached, Some(value(Nil).changeOwner(cached))),
        DefDef(get, _ => Some(guard(cls, name, Select(This(cls), cached)).changeOwner(get))))
      else List(DefDef(get, argss => Some(guard(cls, name, value(argss)).changeOwner(get))))
    }

    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val classDef = viewFields match {
      case Some(fs) =>
        // Each field becomes a member (forwarded to `upper` unless supplied); selectDynamic dispatches to them.
        val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
          fs.flatMap((n, t) => declare(cls, n, ByNameType(t), Flags.EmptyFlags)) :+ Symbol.newMethod(cls, "selectDynamic",
            MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol),
          selfType = None)
        val members = fs.flatMap { (n, t) =>
          define(cls, n, _ => supplied.get(n) match {
            case Some((_, _, source)) => fromSource(source, t)
            case None => t.asType match {
              case '[t] => '{ ${ upper.get.asExprOf[View[?]] }.selectDynamic(${ Expr(n) }).asInstanceOf[t] }.asTerm
            }
          })
        }
        val dispatch = cls.declaredMethod("selectDynamic").head
        val dispatchDef = DefDef(dispatch, argss => {
          val name = argss.head.head.asInstanceOf[Term].asExprOf[String]
          Some(fs.foldRight[Term]('{ throw new NoSuchElementException($name) }.asTerm) { case ((n, _), rest) =>
            If('{ $name == ${ Expr(n) } }.asTerm, Typed(Select(This(cls), cls.declaredMethod(n).head), TypeTree.of[Any]), rest)
          }.changeOwner(dispatch))
        })
        ClassDef(cls, parents, members :+ dispatchDef)
      case None =>
        // Supplied members are implemented, the rest forward to `upper`. A base keeps A's concrete defaults.
        val implemented = traitMembers.filter(m => supplied.contains(m.name) || !isBase)
        val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
          implemented.flatMap(m => declare(cls, m.name, target.memberType(m), Flags.Override)),
          selfType = None)
        val members = implemented.flatMap { m =>
          define(cls, m.name, argss => supplied.get(m.name) match {
            case Some((_, _, source)) => applyArgs(fromSource(source, fields(m.name)._1), argss)
            case None => applyArgs(Select(upper.get, m), argss)
          })
        }
        ClassDef(cls, parents, members)
    }

    val instance = Typed(Apply(Select(New(TypeIdent(classDef.symbol)), classDef.symbol.primaryConstructor), Nil), TypeTree.of[A])
    val partialVal = partialDef.map((sym, value) => ValDef(sym, Some(value.changeOwner(sym))))
    Block(literalClass ++ partialVal.toList :+ classDef, instance).asExprOf[A]
  }
}
