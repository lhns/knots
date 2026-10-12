package spike.e

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait, or a
// View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)`, an object literal
//   `new { def dep2 = ... }`, or a value (an instance of some class, a named tuple). A generated
//   subclass of A takes those and forwards the rest to `upper`; a base (`isBase`) has no upper and
//   must define every abstract member of A.
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

    // --- The fields of A: a View[F]'s are F's; a trait's are its overridable members. ---

    final case class Field(name: String, memberType: TypeRepr, requiredInBase: Boolean, fromUpper: (Term, List[List[Tree]]) => Term)
    val viewOf = target.baseType(TypeRepr.of[View].typeSymbol) match {
      case AppliedType(_, List(f)) => Some(f)
      case _ => None
    }
    val isView = viewOf.nonEmpty
    val fields: List[Field] = viewOf match {
      case Some(f) =>
        namedTupleFields(f).getOrElse(report.errorAndAbort(s"${f.show} is not a named tuple")).map { (n, t) =>
          Field(n, ByNameType(t), true, (upper, _) => t.asType match {
            case '[t] => '{ ${ upper.asExprOf[View[?]] }.selectDynamic(${ Expr(n) }).asInstanceOf[t] }.asTerm
          })
        }
      case _ =>
        val members = targetSym.methodMembers.filter { m =>
          !m.isClassConstructor && !m.flags.is(Flags.Final) && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) &&
            !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass && m.owner != defn.MatchableClass
        }
        targetSym.fieldMembers.find(_.flags.is(Flags.Deferred)).foreach { v =>
          report.errorAndAbort(s"abstract val ${v.name} in ${target.show} is not supported; declare it as def ${v.name}")
        }
        members.groupBy(_.name).collectFirst { case (n, ms) if ms.size > 1 =>
          report.errorAndAbort(s"overloaded member $n in ${target.show} is not supported")
        }
        members.map(m => Field(m.name, target.memberType(m), m.flags.is(Flags.Deferred), (upper, argss) => applyArgs(Select(upper, m), argss)))
    }

    // --- What `o` supplies. ---

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    // `t` with the argument's own context parameter replaced by `ctx`.
    def withContext(t: Term, ctx: Term): Term = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => ctx
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(t)(owner)

    // How a supplied field is computed, given the context (tagged Self/Upper) of the computation.
    enum Source {
      case Expression(term: Term) // a named-tuple element or an object literal member's body
      case FromValue(read: Term => Term) // read from the argument's value, see `valueDefs`
    }
    // A value (an instance, a named tuple) is defined once: as a val if it doesn't use the context,
    // otherwise as a method taking the context. Returns that definition and how to read it for a context.
    def valueReader(value: Term): (Statement, Term => Term) = {
      val valueType = value.tpe.widen
      val usesContext = new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol) = found || tree.symbol == param || foldOverTree(false, tree)(owner)
      }.foldTree(false, value)(owner)
      if !usesContext then {
        val sym = Symbol.newVal(owner, "value", valueType, Flags.EmptyFlags, Symbol.noSymbol)
        (ValDef(sym, Some(value.changeOwner(sym))), _ => Ref(sym))
      } else {
        val sym = Symbol.newMethod(owner, "value", MethodType(List("ctx"))(_ => List(TypeRepr.of[S]), _ => valueType))
        (DefDef(sym, argss => Some(withContext(value, argss.head.head.asInstanceOf[Term]).changeOwner(sym))), ctx => Apply(Ref(sym), List(ctx)))
      }
    }

    // name -> (type, position, source), plus the definition of the argument's value if it has one.
    val inputs: (Map[String, (TypeRepr, Position, Source)], Option[Statement]) = namedTupleLiteral(body).orElse(namedTupleFields(body.tpe)) match {
      case Some(fs) =>
        literalElements(body, fs.length) match {
          case Some(terms) => (fs.zip(terms).map { case ((n, t), e) => n -> (t, e.pos, Source.Expression(e)) }.toMap, None)
          case None =>
            val (definition, read) = valueReader(strip(body))
            (fs.zipWithIndex.map { case ((n, t), i) =>
              n -> (t, o.asTerm.pos, Source.FromValue(ctx => '{ ${ read(ctx).asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }) }.asTerm))
            }.toMap, Some(definition))
        }
      case None =>
        strip(body) match {
          case Block(List(literal: ClassDef), _) =>
            // An object literal: each member's body is computed on its own.
            (literal.body.collect {
              case d: Definition if !d.symbol.flags.is(Flags.Private) && !d.symbol.flags.is(Flags.Protected) &&
                !d.symbol.flags.is(Flags.Synthetic) => d
            }.map { d =>
              d.symbol.name -> (resultType(literal.symbol.typeRef.memberType(d.symbol)), d.symbol.pos.getOrElse(o.asTerm.pos),
                Source.Expression(checkLiteralMember(literal.symbol, d, o.asTerm.pos)))
            }.toMap, None)
          case value =>
            val valueType = value.tpe.widen
            if valueType =:= TypeRepr.of[Object] then
              report.errorAndAbort(s"expected (name = ...), new { ... }, an instance of a class, or a ${target.show}", o)
            val (definition, read) = valueReader(value)
            // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
            ((valueType.typeSymbol.declaredFields ++ valueType.typeSymbol.declaredMethods).filter { m =>
              !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
            }.map { m =>
              m.name -> (resultType(valueType.memberType(m)), m.pos.getOrElse(o.asTerm.pos), Source.FromValue(ctx => Select(read(ctx), m)))
            }.toMap, Some(definition))
        }
    }

    val (supplied, valueDef) = inputs

    // --- Checks: report every problem, but don't generate code from a mismatching argument. ---

    val byName = fields.map(f => f.name -> f).toMap
    // A field can also be supplied with a layer of its own type: a module (see Overridable.focus). A base has
    // no layer below, so it needs a complete Overridable there.
    def layerOf(layer: TypeRepr, t: TypeRepr) = layer.appliedTo(t)
    val modules = supplied.collect {
      case (name, (actual, _, _)) if byName.get(name).exists(f => !(actual <:< resultType(f.memberType)) &&
        actual <:< layerOf(TypeRepr.of[Override], resultType(f.memberType))) => name
    }.toSet
    val problems = supplied.toList.flatMap { case (name, (actual, pos, _)) =>
      byName.get(name) match {
        case None => Some((s"$name is not a member of ${target.show}", pos))
        case Some(f) =>
          val required = resultType(f.memberType)
          if modules(name) then
            Option.when(isBase && !(actual <:< layerOf(TypeRepr.of[Overridable], required)))(
              (s"$name: a base needs an Overridable[${required.show}], there is no layer below to override", pos))
          else Option.when(!(actual <:< required)) {
            // A layer of the wrong trait gets a hint that layers are accepted here.
            val isLayer = actual.widen match {
              case AppliedType(tc, _) => Set(TypeRepr.of[Override], TypeRepr.of[Overridable]).exists(_.typeSymbol == tc.typeSymbol)
              case _ => false
            }
            val hint = if isLayer then " or a layer of it" else ""
            (s"$name has type ${actual.show}, expected ${required.show}$hint", pos)
          }
      }
    }
    val missing = fields.filter(f => isBase && f.requiredInBase && !supplied.contains(f.name)).map(_.name)
    val allProblems =
      if missing.isEmpty then problems
      else problems :+ ((s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o.asTerm.pos))
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // --- The generated class: its Slot holds the computation path and the shared cells. ---

    // A layer forwards everything it doesn't supply to `upper`; a base keeps A's concrete defaults.
    val generated = fields.filter(f => supplied.contains(f.name) || !isBase)
    // Supplied fields without parameters are computed once, in a Cell.
    val cellIndex = generated.filter(f => supplied.contains(f.name) && !hasParams(f.memberType)).map(_.name).zipWithIndex.toMap
    val parents = List(TypeTree.of[Object], TypeTree.of[A], TypeTree.of[Tagged[A]])
    val pathType = TypeRepr.of[List[Cell]]
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      List(
        Symbol.newVal(cls, "slot", TypeRepr.of[Tag.Slot], Flags.EmptyFlags, Symbol.noSymbol),
        Symbol.newMethod(cls, "at", MethodType(List("path"))(_ => List(pathType), _ => target), Flags.Override, Symbol.noSymbol)
      ) ++ generated.map(f => Symbol.newMethod(cls, f.name, f.memberType, if isView then Flags.EmptyFlags else Flags.Override, Symbol.noSymbol))
        ++ Option.when(isView)(Symbol.newMethod(cls, "selectDynamic",
          MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol)),
      selfType = None)
    val slotField = cls.declaredField("slot")
    def slot = Select(This(cls), slotField).asExprOf[Tag.Slot]

    // The tagged Self/Upper for a computation on behalf of `path`.
    def context(path: Term): Term =
      if isBase then '{ Tag.self(${ s.asExprOf[Self[A]] }, ${ path.asExprOf[List[Cell]] }) }.asTerm
      else '{ Tag.upper(${ s.asExprOf[Upper[A]] }, ${ path.asExprOf[List[Cell]] }) }.asTerm
    def compute(f: Field, source: Source, path: Term): Term = {
      val ctx = context(path)
      val value = source match {
        case Source.Expression(term) => withContext(term, ctx)
        case Source.FromValue(read) => read(ctx)
      }
      if modules(f.name) then module(f, ctx, value) else value
    }
    // A module's layer run focused through its field (see Overridable.focus).
    def module(f: Field, ctx: Term, layer: Term): Term = resultType(f.memberType).asType match {
      case '[t] =>
        val get = Lambda(owner, MethodType(List("root"))(_ => List(target), _ => TypeRepr.of[t]),
          (meth, params) => f.fromUpper(params.head.asInstanceOf[Term], Nil).changeOwner(meth)).asExprOf[A => t]
        if isBase then '{ Overridable.focus[A, t](${ ctx.asExprOf[Self[A]] }, $get, ${ layer.asExprOf[Overridable[t]] }) }.asTerm
        else '{ Override.focus[A, t](${ ctx.asExprOf[Upper[A]] }, $get, ${ layer.asExprOf[Override[t]] }) }.asTerm
    }
    def fieldBody(f: Field, argss: List[List[Tree]]): Term = supplied.get(f.name) match {
      case Some((_, _, source)) if cellIndex.contains(f.name) =>
        val fn = Lambda(owner, MethodType(List("path"))(_ => List(pathType), _ => TypeRepr.of[Any]),
          (meth, params) => compute(f, source, params.head.asInstanceOf[Term]).changeOwner(meth))
        // Re-tagged on read: a cached module object would otherwise lose the path at the boundary.
        resultType(f.memberType).asType match {
          case '[t] => '{ Tag[Any]($slot.cells(${ Expr(cellIndex(f.name)) }).get($slot.path)(${ fn.asExprOf[List[Cell] => Any] }), $slot.path).asInstanceOf[t] }.asTerm
        }
      case Some((_, _, source)) => applyArgs(compute(f, source, '{ $slot.path }.asTerm), argss)
      case None => f.fromUpper('{ Tag(${ s.asExprOf[Upper[A]] }.upper, $slot.path) }.asTerm, argss)
    }
    // A new instance of the generated class with the given path and cells.
    def newInstance(owner: Symbol, path: Expr[List[Cell]], cells: Expr[Array[Cell]]): Term = {
      val instance = Symbol.newVal(owner, "instance", cls.typeRef, Flags.EmptyFlags, Symbol.noSymbol)
      val instanceSlot = Select(Ref(instance), slotField).asExprOf[Tag.Slot]
      Block(
        List(
          ValDef(instance, Some(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil))),
          '{ $instanceSlot.path = $path }.asTerm,
          '{ $instanceSlot.cells = $cells }.asTerm),
        Typed(Ref(instance), TypeTree.of[A]))
    }

    val at = cls.declaredMethod("at").head
    val fieldDefs = generated.map { f =>
      val get = cls.declaredMethod(f.name).head
      DefDef(get, argss => Some(fieldBody(f, argss).changeOwner(get)))
    }
    val dispatchDef = Option.when(isView) {
      // selectDynamic for a View: dispatches to the field members.
      val dispatch = cls.declaredMethod("selectDynamic").head
      DefDef(dispatch, argss => {
        val name = argss.head.head.asInstanceOf[Term].asExprOf[String]
        Some(generated.foldRight[Term]('{ throw new NoSuchElementException($name) }.asTerm) { (f, rest) =>
          If('{ $name == ${ Expr(f.name) } }.asTerm, Typed(Select(This(cls), cls.declaredMethod(f.name).head), TypeTree.of[Any]), rest)
        }.changeOwner(dispatch))
      })
    }
    val classDef = ClassDef(cls, parents,
      List(
        ValDef(slotField, Some('{ new Tag.Slot }.asTerm.changeOwner(slotField))),
        DefDef(at, argss => Some(newInstance(at, argss.head.head.asInstanceOf[Term].asExprOf[List[Cell]], '{ $slot.cells }).changeOwner(at)))
      ) ++ fieldDefs ++ dispatchDef)

    val cells = Varargs(cellIndex.toList.sortBy(_._2).map((n, _) => '{ new Cell(${ Expr(n) }) }))
    Block(valueDef.toList :+ classDef, newInstance(owner, '{ Nil }, '{ Array[Cell]($cells*) })).asExprOf[A]
  }

  // An object literal member's body, checked: its members are computed one by one, so a body may only
  // use self/upper, not the literal itself or its other members, and members can't take parameters.
  private def checkLiteralMember(using q: Quotes)(literal: q.reflect.Symbol, d: q.reflect.Definition, fallback: q.reflect.Position): q.reflect.Term = {
    import q.reflect.*
    val name = d.symbol.name
    val pos = d.symbol.pos.getOrElse(fallback)
    val rhs = d match {
      case DefDef(_, Nil, _, rhs) => rhs
      case ValDef(_, _, rhs) => rhs
      case _ => report.errorAndAbort(s"$name: object literal members can't take parameters; use a named class", pos)
    }
    val body = rhs.getOrElse(report.errorAndAbort(s"$name has no body", pos))
    new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _: This if tree.symbol == literal =>
            report.errorAndAbort(s"$name uses the object literal itself; use self/upper, or a named class", tree.pos)
          case _: Ident | _: Select if tree.symbol.owner == literal =>
            report.errorAndAbort(s"$name uses ${tree.symbol.name} of the object literal; use self/upper, or a named class", tree.pos)
          case _ =>
        }
        super.traverseTree(tree)(owner)
      }
    }.traverseTree(body)(d.symbol)
    body
  }

  // A named tuple's fields. Matched before dealiasing: the desugared literal's type sees through the
  // opaque NamedTuple.
  private def namedTupleFields(using q: Quotes)(t: q.reflect.TypeRepr): Option[List[(String, q.reflect.TypeRepr)]] = {
    import q.reflect.*
    def elems(t: TypeRepr): List[TypeRepr] = t.dealias match {
      case AppliedType(tc, List(h, tail)) if tc.typeSymbol == Symbol.requiredClass("scala.*:") => h :: elems(tail)
      case AppliedType(tc, args) if tc.typeSymbol.fullName.startsWith("scala.Tuple") => args
      case _ => Nil
    }
    t.widen match {
      case AppliedType(tc, List(names, values)) if tc.typeSymbol.name == "NamedTuple" =>
        Some(elems(names).collect { case ConstantType(StringConstant(n)) => n }.zip(elems(values)))
      case w =>
        val d = w.dealiasKeepOpaques
        if d == w then None else namedTupleFields(d)
    }
  }

  // A named-tuple literal desugars to `x$proxy = TupleN.apply(e1, ..., en)` followed by an ascription
  // to the NamedTuple type, nested in Inlined/Block nodes. Only that spine is searched.
  private def spine(using q: Quotes)(t: q.reflect.Term): (List[q.reflect.Statement], q.reflect.Term) = {
    import q.reflect.*
    t match {
      case Inlined(_, bindings, e) => val (stats, last) = spine(e); (bindings ++ stats, last)
      case Block(stats, e) => val (more, last) = spine(e); (stats ++ more, last)
      case last => (Nil, last)
    }
  }

  private def namedTupleLiteral(using q: Quotes)(t: q.reflect.Term): Option[List[(String, q.reflect.TypeRepr)]] = {
    import q.reflect.*
    spine(t)._2 match {
      case Typed(_, tpt) => namedTupleFields(tpt.tpe)
      case _ => None
    }
  }

  private def literalElements(using q: Quotes)(t: q.reflect.Term, arity: Int): Option[List[q.reflect.Term]] = {
    import q.reflect.*
    spine(t)._1.collectFirst {
      case ValDef(_, _, Some(Apply(fn, args)))
        if fn.symbol.name == "apply" && fn.symbol.owner.fullName == s"scala.Tuple$arity$$" && args.length == arity => args
    }
  }
}
