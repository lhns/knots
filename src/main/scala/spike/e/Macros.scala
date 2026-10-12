package spike.e

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait, or a
// View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)`, an object literal
//   `new { def dep2 = ... }` (its members may only use self/upper, not each other), an instance of
//   some class, or a named-tuple value. A generated subclass of A takes those and forwards the rest
//   to `upper`; a base (`isBase`) has no upper and must define every abstract member of A.
// Each supplied field is computed once, in a Cell, with its own Self/Upper tagged with the path of
// the computation (see Tagged), so cycles are detected without thread- or scope-bound state.
object Macros {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.dealias.typeSymbol
    val isBase = isBaseExpr.valueOrAbort
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
    val traitMembers = targetSym.methodMembers.filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
        !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass &&
        m.owner != TypeRepr.of[Tagged].typeSymbol
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

    // How a supplied field is computed, given the tagged Self/Upper of that computation (the
    // replacement for the argument's own context parameter).
    val owner = Symbol.spliceOwner
    def withContext(t: Term, ctx: Term): Term = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => ctx
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(t)(owner)
    enum Source {
      case Expression(term: Term) // a named-tuple element or an object literal member's body
      case Member(value: Term, sym: Symbol) // a member of a value (e.g. `new Wrapped`), created per computation
      case Index(value: Term, i: Int) // an element of a named-tuple value
    }

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
    def literalElements(arity: Int): Option[List[Term]] = spine(body)._1.collectFirst {
      case ValDef(_, _, Some(Apply(fn, args)))
        if fn.symbol.name == "apply" && fn.symbol.owner.fullName == s"scala.Tuple$arity$$" && args.length == arity => args
    }

    // What `o` supplies: name -> (type, position, source).
    val supplied: Map[String, (TypeRepr, Position, Source)] = literalFields.orElse(namedTupleFields(body.tpe)) match {
      case Some(fs) =>
        literalElements(fs.length) match {
          case Some(terms) => fs.zip(terms).map { case ((n, t), e) => n -> (t, e.pos, Source.Expression(e)) }.toMap
          case None => fs.zipWithIndex.map { case ((n, t), i) => n -> (t, o.asTerm.pos, Source.Index(body, i)) }.toMap
        }
      case None =>
        strip(body) match {
          case Block(List(cd: ClassDef), _) =>
            // An object literal: each member's body is computed on its own, so it may only use
            // self/upper, not other members of the literal.
            val anon = cd.symbol
            cd.body.collect {
              case d: Definition if !d.symbol.flags.is(Flags.Private) && !d.symbol.flags.is(Flags.Protected) &&
                !d.symbol.flags.is(Flags.Synthetic) => d
            }.map { d =>
              val name = d.symbol.name
              val pos = d.symbol.pos.getOrElse(o.asTerm.pos)
              val (rhs, params) = d match {
                case DefDef(_, paramss, _, rhs) => (rhs, paramss.nonEmpty)
                case ValDef(_, _, rhs) => (rhs, false)
                case _ => (None, true)
              }
              if params then report.errorAndAbort(s"$name: object literal members can't take parameters; use a named class", pos)
              val rhsTerm = rhs.getOrElse(report.errorAndAbort(s"$name has no body", pos))
              new TreeTraverser {
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
                  tree match {
                    case _: This if tree.symbol == anon =>
                      report.errorAndAbort(s"$name uses the object literal itself; use self/upper, or a named class", tree.pos)
                    case _: Ident | _: Select if tree.symbol.owner == anon =>
                      report.errorAndAbort(s"$name uses ${tree.symbol.name} of the object literal; use self/upper, or a named class", tree.pos)
                    case _ =>
                  }
                  super.traverseTree(tree)(owner)
                }
              }.traverseTree(rhsTerm)(d.symbol)
              name -> (resultType(anon.typeRef.memberType(d.symbol)), pos, Source.Expression(rhsTerm))
            }.toMap
          case value =>
            val valueType = value.tpe.widen
            if valueType =:= TypeRepr.of[Object] then
              report.errorAndAbort(s"expected (name = ...), new { ... }, an instance of a class, or a ${target.show}", o)
            // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
            (valueType.typeSymbol.declaredFields ++ valueType.typeSymbol.declaredMethods).filter { m =>
              !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
            }.map(m => m.name -> (resultType(valueType.memberType(m)), m.pos.getOrElse(o.asTerm.pos), Source.Member(value, m))).toMap
        }
    }

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
    // Report every problem, but don't generate code from a mismatching argument.
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    val pathType = TypeRepr.of[List[Cell[?]]]
    // The tagged Self/Upper for a computation on behalf of `path`.
    def context(path: Term): Term =
      if isBase then '{ Tag.self(${ s.asExprOf[Self[A]] }, ${ path.asExprOf[List[Cell[?]]] }) }.asTerm
      else '{ Tag.upper(${ s.asExprOf[Upper[A]] }, ${ path.asExprOf[List[Cell[?]]] }) }.asTerm
    def compute(source: Source, path: Term): Term = source match {
      case Source.Expression(term) => withContext(term, context(path))
      case Source.Member(value, sym) => Select(withContext(value, context(path)), sym)
      case Source.Index(value, i) =>
        '{ ${ withContext(value, context(path)).asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }) }.asTerm
    }
    def applyArgs(fn: Term, argss: List[List[Tree]]): Term = argss.foldLeft(fn) {
      case (fn, (targ: TypeTree) :: targs) => TypeApply(fn, targ :: targs.map(_.asInstanceOf[TypeTree]))
      case (fn, args) => Apply(fn, args.map(_.asInstanceOf[Term]))
    }

    // The generated class: its Slot holds the path and the shared cells, set right after creation.
    val cachedNames = supplied.keys.toList.filter(n => viewFields.nonEmpty || traitMembers.exists(m => m.name == n && !hasParams(target.memberType(m))))
    val cellIndex = cachedNames.zipWithIndex.toMap
    val parents = List(TypeTree.of[Object], TypeTree.of[A], TypeTree.of[Tagged[A]])
    val memberDecls: Symbol => List[Symbol] = cls => viewFields match {
      case Some(fs) =>
        fs.map((n, t) => Symbol.newMethod(cls, n, ByNameType(t), Flags.EmptyFlags, Symbol.noSymbol)) :+
          Symbol.newMethod(cls, "selectDynamic", MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol)
      case None =>
        traitMembers.filter(m => supplied.contains(m.name) || !isBase)
          .map(m => Symbol.newMethod(cls, m.name, target.memberType(m), Flags.Override, Symbol.noSymbol))
    }
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      List(
        Symbol.newVal(cls, "slot", TypeRepr.of[Tag.Slot], Flags.EmptyFlags, Symbol.noSymbol),
        Symbol.newMethod(cls, "at", MethodType(List("path"))(_ => List(pathType), _ => target), Flags.Override, Symbol.noSymbol)
      ) ++ memberDecls(cls),
      selfType = None)
    val slotField = cls.declaredField("slot")
    def slot = Select(This(cls), slotField).asExprOf[Tag.Slot]
    def thisPath = '{ $slot.path }.asTerm

    // The value of `name` for this instance: a supplied field through its cell, the rest from `upper`.
    def fieldValue(name: String, tpe: TypeRepr, forward: Term => Term): Term = supplied.get(name) match {
      case Some((_, _, source)) => tpe.asType match {
        case '[t] =>
          val fn = Lambda(owner, MethodType(List("path"))(_ => List(pathType), _ => TypeRepr.of[Any]),
            (meth, params) => compute(source, params.head.asInstanceOf[Term]).changeOwner(meth))
          '{ $slot.cells(${ Expr(cellIndex(name)) }).get($slot.path)(${ fn.asExprOf[List[Cell[?]] => Any] }).asInstanceOf[t] }.asTerm
      }
      case None => forward('{ Tag(${ s.asExprOf[Upper[A]] }.upper, $slot.path) }.asTerm)
    }

    val at = cls.declaredMethod("at").head
    val atDef = DefDef(at, argss => {
      val copy = Symbol.newVal(at, "copy", cls.typeRef, Flags.EmptyFlags, Symbol.noSymbol)
      val copySlot = Select(Ref(copy), slotField).asExprOf[Tag.Slot]
      Some(Block(
        List(
          ValDef(copy, Some(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil))),
          '{ $copySlot.path = ${ argss.head.head.asInstanceOf[Term].asExprOf[List[Cell[?]]] } }.asTerm,
          '{ $copySlot.cells = $slot.cells }.asTerm),
        Typed(Ref(copy), TypeTree.of[A])).changeOwner(at))
    })
    val members = viewFields match {
      case Some(fs) =>
        val fieldDefs = fs.map { (n, t) =>
          val get = cls.declaredMethod(n).head
          DefDef(get, _ => Some(fieldValue(n, t, upper => t.asType match {
            case '[t] => '{ ${ upper.asExprOf[View[?]] }.selectDynamic(${ Expr(n) }).asInstanceOf[t] }.asTerm
          }).changeOwner(get)))
        }
        val dispatch = cls.declaredMethod("selectDynamic").head
        val dispatchDef = DefDef(dispatch, argss => {
          val name = argss.head.head.asInstanceOf[Term].asExprOf[String]
          Some(fs.foldRight[Term]('{ throw new NoSuchElementException($name) }.asTerm) { case ((n, _), rest) =>
            If('{ $name == ${ Expr(n) } }.asTerm, Typed(Select(This(cls), cls.declaredMethod(n).head), TypeTree.of[Any]), rest)
          }.changeOwner(dispatch))
        })
        fieldDefs :+ dispatchDef
      case None =>
        traitMembers.filter(m => supplied.contains(m.name) || !isBase).map { m =>
          val get = cls.declaredMethod(m.name).head
          DefDef(get, argss => {
            val value =
              if cellIndex.contains(m.name) then fieldValue(m.name, fields(m.name)._1, _ => '{ ??? }.asTerm)
              else supplied.get(m.name) match {
                case Some((_, _, source)) => applyArgs(compute(source, thisPath), argss)
                case None => applyArgs(Select('{ Tag(${ s.asExprOf[Upper[A]] }.upper, ${ thisPath.asExprOf[List[Cell[?]]] }) }.asTerm, m), argss)
              }
            Some(value.changeOwner(get))
          })
        }
    }
    val slotDef = ValDef(slotField, Some('{ new Tag.Slot }.asTerm.changeOwner(slotField)))
    val classDef = ClassDef(cls, parents, List(slotDef, atDef) ++ members)

    // The instance for path Nil, with one Cell per cached field.
    val instance = Symbol.newVal(owner, "instance", cls.typeRef, Flags.EmptyFlags, Symbol.noSymbol)
    val cells = '{ Array[Cell[Any]](${ Varargs(cachedNames.map(n => '{ new Cell[Any](${ Expr(n) }) })) }*) }.asTerm
    Block(
      List(
        classDef,
        ValDef(instance, Some(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil))),
        '{ ${ Select(Ref(instance), slotField).asExprOf[Tag.Slot] }.cells = ${ cells.asExprOf[Array[Cell[Any]]] } }.asTerm),
      Typed(Ref(instance), TypeTree.of[A])).asExprOf[A]
  }
}
