package spike.e

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait, or a
// View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is: not cached, no cycle detection
//   (it is not generated, so it knows no paths);
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)`, an object literal
//   `new { def dep2 = ... }`, or a value (an instance of some class, a named tuple). A generated
//   subclass of A takes those and forwards the rest to `upper`; a base (`isBase`) has no upper and
//   must define every abstract member of A.
// Each supplied field is computed once, in a Cell, with its own Self/Upper tagged with the path of
// the computation (see Tagged), so cycles are detected without thread- or scope-bound state.
//
// A trait target must be fully abstract: a concrete member (def with a body, val, lazy val, var, also
// an inherited one) would run on the generated layer's own `this`, not on the final `self`, so it is
// rejected. Fields that are modules are supplied as `module(...)` (see Overridable.scala).
object Macros {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  // The desugaring of a named-tuple literal binds these (see `literalOf`).
  private val ProxyName = """\$proxy\d*|NamedTuple\$_this.*""".r

  // View fields are read with selectDynamic on a class that also has these members.
  private val ReservedViewNames = Set("selectDynamic", "toString", "hashCode", "equals", "getClass")

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    def tshow(t: TypeRepr) = t.show(using Printer.TypeReprShortCode)
    val target = TypeRepr.of[A]
    val targetName = tshow(target)
    val targetSym = target.dealias.typeSymbol
    val isBase = isBaseExpr.valueOrAbort
    val owner = Symbol.spliceOwner
    val argPos = o.asTerm.pos
    def failArg(msg: String): Nothing = report.errorAndAbort(msg, o)
    // A symbol's own position only if it is defined in user source; library symbols have none.
    def posOf(sym: Symbol, fallback: Position): Position =
      if sym.isDefinedInCurrentRun then sym.pos.getOrElse(fallback) else fallback

    target.dealias match {
      case _: Refinement => report.errorAndAbort(s"refinement types are not supported as a target: $targetName")
      case _ =>
    }
    if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"$targetName must be a trait")

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

    // A named tuple's fields. Matched before dealiasing: the desugared literal's type sees through the
    // opaque NamedTuple.
    def namedTupleFields(t: TypeRepr): Option[List[(String, TypeRepr)]] = {
      def elems(t: TypeRepr): List[TypeRepr] = t.simplified.dealias match {
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

    // --- The fields of A: a View[F]'s are F's; a trait's are its abstract members. ---

    final case class Field(name: String, memberType: TypeRepr, fromUpper: (Term, List[List[Tree]]) => Term)
    val targetProblems = List.newBuilder[String]

    // `this.type` anywhere in a member's type refers to the generated layer, not to the final self.
    def mentionsThis(t: TypeRepr): Boolean = t match {
      case ThisType(tr) => tr.typeSymbol == targetSym
      case AppliedType(tc, args) => mentionsThis(tc) || args.exists(mentionsThis)
      case AndType(l, r) => mentionsThis(l) || mentionsThis(r)
      case OrType(l, r) => mentionsThis(l) || mentionsThis(r)
      case ByNameType(u) => mentionsThis(u)
      case MethodType(_, ps, r) => ps.exists(mentionsThis) || mentionsThis(r)
      case PolyType(_, bs, r) => bs.exists(mentionsThis) || mentionsThis(r)
      case TypeLambda(_, bs, r) => bs.exists(mentionsThis) || mentionsThis(r)
      case TypeBounds(l, h) => mentionsThis(l) || mentionsThis(h)
      case TypeRef(_: ThisType, _) | TermRef(_: ThisType, _) => false // a member of this, like this.X
      case TypeRef(q, _) => mentionsThis(q)
      case TermRef(q, _) => mentionsThis(q)
      case AnnotatedType(u, _) => mentionsThis(u)
      case Refinement(p, _, i) => mentionsThis(p) || mentionsThis(i)
      case _ => false
    }

    val viewOf = target.baseType(TypeRepr.of[View].typeSymbol) match {
      case AppliedType(_, List(f)) => Some(f)
      case _ => None
    }
    val isView = viewOf.nonEmpty
    val fields: List[Field] = viewOf match {
      case Some(f) =>
        val fs = namedTupleFields(f).getOrElse(report.errorAndAbort(s"${tshow(f)} is not a named tuple"))
        fs.map(_._1).filter(ReservedViewNames).foreach { n =>
          targetProblems += s"View field $n is reserved (it is a member of the generated View); rename it"
        }
        fs.map { (n, t) =>
          Field(n, ByNameType(t), (upper, _) => t.asType match {
            case '[t] => '{ ${ upper.asExprOf[View[?]] }.selectDynamic(${ Expr(n) }).asInstanceOf[t] }.asTerm
          })
        }
      case _ =>
        // The self type and the constructor parameters can't be honored by the generated subclass.
        val selfTyped = scala.util.Try(targetSym.tree).toOption.exists {
          case cd: ClassDef => cd.self.isDefined
          case _ => false
        }
        if selfTyped then targetProblems += s"$targetName has a self type, which is not supported"
        val pc = targetSym.primaryConstructor
        if pc.exists && pc.paramSymss.flatten.exists(_.isTerm) then
          targetProblems += s"$targetName has constructor parameters, which are not supported"
        targetSym.typeRef.baseClasses.filter { c =>
          !c.flags.is(Flags.Trait) && c != defn.AnyClass && c != defn.ObjectClass && c != defn.MatchableClass
        }.foreach(c => targetProblems += s"$targetName extends the class ${c.name}; only traits are supported")
        targetSym.typeMembers.filter(t => t.flags.is(Flags.Deferred) && !t.flags.is(Flags.Param)).foreach { t =>
          targetProblems += s"abstract type member ${t.name} in $targetName is not supported"
        }

        // The members are only analyzed for a target that is otherwise supported (a self type brings in
        // members that aren't the trait's own).
        val members = if targetProblems.result().nonEmpty then Nil else (targetSym.methodMembers ++ targetSym.fieldMembers).distinct.filter { m =>
          !m.isClassConstructor && !m.flags.is(Flags.Synthetic) && m.owner != defn.AnyClass && m.owner != defn.ObjectClass &&
            m.owner != defn.MatchableClass && m.name != "$init$" && !m.name.contains("$default$") && !(m.name.endsWith("_=") && targetSym.fieldMembers.exists(_.name == m.name.stripSuffix("_=")))
        }
        val abstractMembers = members.filter { m =>
          val isField = m.isValDef
          if !m.flags.is(Flags.Deferred) then {
            // A private helper def is harmless; anything else would run on the layer's own `this`.
            if isField || !m.flags.is(Flags.Private) then
              targetProblems += s"concrete member ${m.name} in $targetName is bound to its own layer (its body uses this, not self); make it abstract and define it in the base"
            false
          } else if isField then {
            targetProblems += (
              if m.flags.is(Flags.Mutable) then s"abstract var ${m.name} in $targetName is not supported; declare it as def ${m.name}"
              else s"abstract val ${m.name} in $targetName is not supported; declare it as def ${m.name}")
            false
          } else if m.flags.is(Flags.Protected) then {
            targetProblems += s"abstract protected member ${m.name} in $targetName is not supported"
            false
          } else {
            if m.paramSymss.flatten.exists(_.flags.is(Flags.HasDefault)) then
              targetProblems += s"${m.name} has default arguments, which are not supported"
            // widen turns a bare this.type result into the trait's type, so the declared result is read too.
            val declaredResult = scala.util.Try(m.tree).toOption.collect { case d: DefDef => d.returnTpt.tpe }
            if mentionsThis(m.termRef.widen) || declaredResult.exists(mentionsThis) then
              targetProblems += s"${m.name} in $targetName mentions this.type, which is not supported"
            true
          }
        }
        abstractMembers.groupBy(_.name).collectFirst { case (n, ms) if ms.size > 1 =>
          targetProblems += s"overloaded member $n in $targetName is not supported"
        }
        abstractMembers.map(m => Field(m.name, target.memberType(m), (upper, argss) => applyArgs(Select(upper, m), argss)))
    }
    // One error: the compiler hides further errors at the same position.
    if targetProblems.result().nonEmpty then {
      report.error(targetProblems.result().mkString("\n"))
      return '{ ??? }
    }

    // --- What `o` supplies. ---

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    val expectedKinds = s"expected (name = ...), new { ... }, an instance of a class, or a $targetName"
    if body.tpe.widen =:= TypeRepr.of[Null] then failArg(s"null is not a valid argument; $expectedKinds")
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    val layerSymbols = List(TypeRepr.of[Override], TypeRepr.of[Overridable], TypeRepr.of[Module], TypeRepr.of[BaseModule]).map(_.typeSymbol)
    def isLayerType(t: TypeRepr) = t.dealiasKeepOpaques match {
      case AppliedType(tc, _) => layerSymbols.contains(tc.typeSymbol)
      case _ => false
    }
    if isLayerType(body.tpe.widen) then failArg("this is a layer; combine layers with |+|")

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
      case FromValue(read: Term => Term) // read from the argument's value, see `valueReader`
    }
    final case class Input(tpe: TypeRepr, pos: Position, source: Source)

    // A value (an instance, a named tuple) is defined once: as a val if it doesn't use the context,
    // otherwise as a method taking the context. Returns that definition and how to read it for a context.
    def valueReader(value: Term): (Statement, Term => Term) = {
      val valueType = value.tpe.widen
      val usesContext = new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol) = found || tree.symbol == param || foldOverTree(false, tree)(owner)
      }.foldTree(false, value)(owner)
      if !usesContext then {
        val sym = Symbol.newVal(owner, Symbol.freshName("value"), valueType, Flags.EmptyFlags, Symbol.noSymbol)
        (ValDef(sym, Some(value.changeOwner(sym))), _ => Ref(sym))
      } else {
        val sym = Symbol.newMethod(owner, Symbol.freshName("value"), MethodType(List("ctx"))(_ => List(TypeRepr.of[S]), _ => valueType))
        (DefDef(sym, argss => Some(withContext(value, argss.head.head.asInstanceOf[Term]).changeOwner(sym))), ctx => Apply(Ref(sym), List(ctx)))
      }
    }

    // The desugaring of a named-tuple literal `(a = e1, ...)` is a block of proxy vals (`$proxy3`,
    // `NamedTuple$_this`, `x$proxy5 = Tuple2.apply(e1, e2)`) ending in `(x$proxy5: NamedTuple[..])`.
    // The literal is recognized by following that final ascription to its proxy and the proxy's tuple
    // construction. An element whose type mentions NamedTuple makes the construction a
    // `TupleN.apply(..).$asInstanceOf$[..]`, and 23 fields or more `TupleXXL.apply(Repeated(..))`.
    enum Shape {
      case Fields(fields: List[(String, TypeRepr)], elements: List[Term])
      case Statements(pos: Position) // the literal is preceded by user statements
      case NotALiteral
    }
    def spine(t: Term): (List[Statement], Term) = t match {
      case Inlined(_, bindings, e) => val (stats, last) = spine(e); (bindings ++ stats, last)
      case Block(stats, e) => val (more, last) = spine(e); (stats ++ more, last)
      case last => (Nil, last)
    }
    def stripAs(t: Term): Term = t match {
      case Inlined(_, Nil, e) => stripAs(e)
      case Typed(e, _) => stripAs(e)
      case TypeApply(Select(e, "$asInstanceOf$"), _) => stripAs(e)
      case t => t
    }
    def literalOf(t: Term): Shape = {
      val (stats, last) = spine(t)
      last match {
        case Typed(inner, tpt) =>
          (namedTupleFields(tpt.tpe), stripAs(inner)) match {
            case (Some(fs), proxy: Ident) =>
              val definition = stats.collectFirst { case v: ValDef if v.symbol == proxy.symbol => v.rhs }.flatten
              val elements = definition.map(stripAs).collect {
                case Apply(fn, args) if fn.symbol.name == "apply" &&
                    (fn.symbol.owner.fullName.startsWith("scala.Tuple") || fn.symbol.owner.fullName == "scala.runtime.TupleXXL$") =>
                  args match {
                    case List(Typed(Repeated(es, _), _)) => es
                    case es => es
                  }
              }.filter(_.length == fs.length)
              elements match {
                case Some(es) =>
                  stats.find {
                    case v: ValDef => v.symbol != proxy.symbol && !ProxyName.matches(v.name)
                    case _ => true
                  } match {
                    case Some(extra) => Shape.Statements(extra.pos)
                    case None => Shape.Fields(fs, es)
                  }
                case None => Shape.NotALiteral
              }
            case _ => Shape.NotALiteral
          }
        case _ => Shape.NotALiteral
      }
    }

    // An object literal's members are computed one by one, so a body may only use self/upper, not the
    // literal itself or its other members, and members can't take parameters.
    def checkLiteralMember(literal: Symbol, d: Definition): Term = {
      val name = d.symbol.name
      val pos = posOf(d.symbol, argPos)
      val rhs = d match {
        case DefDef(_, Nil, _, rhs) => rhs
        case ValDef(_, _, rhs) => rhs
        case _ => report.errorAndAbort(s"$name: object literal members can't take parameters; use a named class", pos)
      }
      val body = rhs.getOrElse(report.errorAndAbort(s"$name has no body", pos))
      def isLiteralThis(tpe: TypeRepr) = tpe match {
        case TermRef(ThisType(tr), _) => tr.typeSymbol == literal
        case _ => false
      }
      new TreeTraverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
          tree match {
            case t @ (_: Ident | _: Select) if t.symbol.exists && t.symbol.owner == literal =>
              report.errorAndAbort(s"$name uses ${t.symbol.name} of the object literal; use self/upper, or a named class", t.pos)
            case t @ (Select(_: This, _) | _: Ident) if t.symbol.exists && (t match {
                  case Select(th: This, _) => th.symbol == literal
                  case _ => isLiteralThis(t.asInstanceOf[Term].tpe)
                }) =>
              report.errorAndAbort(
                s"$name uses ${t.symbol.name}, inherited by the object literal from ${t.symbol.owner.name}; use self/upper, or a named class", t.pos)
            case _: This if tree.symbol == literal =>
              report.errorAndAbort(s"$name uses the object literal itself; use self/upper, or a named class", tree.pos)
            case _ =>
          }
          super.traverseTree(tree)(owner)
        }
      }.traverseTree(body)(d.symbol)
      body
    }

    // An argument that is none of the accepted kinds. A class defined in user source is an instance
    // of a class; the library ones (Int, String, functions, Option, tuples, ...) are never meant.
    def checkInstanceType(valueType: TypeRepr): Unit = {
      val d = valueType.dealias
      val sym = d.typeSymbol
      val isLibrary = sym == defn.AnyClass || sym == defn.ObjectClass || sym == defn.MatchableClass || sym == defn.NullClass ||
        sym == defn.NothingClass || sym.fullName.startsWith("scala.") || sym.fullName.startsWith("java.") || sym.fullName.startsWith("javax.")
      d match {
        case _: OrType | _: AndType | _: Refinement => failArg(expectedKinds)
        case _ => if isLibrary then failArg(expectedKinds)
      }
    }

    // name -> input, plus the definition of the argument's value if it has one.
    val (supplied: Map[String, Input], valueDef: Option[Statement]) = literalOf(body) match {
      case Shape.Fields(fs, terms) =>
        (fs.zip(terms).map { case ((n, t), e) => n -> Input(t, e.pos, Source.Expression(e)) }.toMap, None)
      case Shape.Statements(pos) =>
        report.errorAndAbort("statements before the named tuple are not supported; put them inside the fields", pos)
      case Shape.NotALiteral =>
        namedTupleFields(body.tpe) match {
          case Some(fs) =>
            // A named-tuple value (a variable, `++`, ...): evaluated once, each field read as its element.
            val (definition, read) = valueReader(strip(body))
            (fs.zipWithIndex.map { case ((n, t), i) =>
              n -> Input(t, argPos, Source.FromValue(ctx => t.asType match {
                case '[e] => '{ ${ read(ctx).asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }).asInstanceOf[e] }.asTerm
              }))
            }.toMap, Some(definition))
          case None =>
            strip(body) match {
              case Block(List(literal: ClassDef), _) =>
                // An object literal: each member's body is computed on its own.
                (literal.body.flatMap {
                  case c: ClassDef =>
                    report.errorAndAbort(s"nested class ${c.name} in an object literal is not supported; use a named class", posOf(c.symbol, argPos))
                  case t: TypeDef =>
                    report.errorAndAbort(s"type member ${t.name} in an object literal is not supported; use a named class", posOf(t.symbol, argPos))
                  case v: ValDef if v.symbol.flags.is(Flags.Mutable) =>
                    report.errorAndAbort(s"var ${v.name} in an object literal is not supported; use a val or def", posOf(v.symbol, argPos))
                  case d: DefDef if d.name.endsWith("_=") && d.symbol.flags.is(Flags.Mutable) =>
                    report.errorAndAbort(s"var ${d.name.stripSuffix("_=")} in an object literal is not supported; use a val or def", posOf(d.symbol, argPos))
                  case d: Definition if !d.symbol.flags.is(Flags.Private) && !d.symbol.flags.is(Flags.Protected) && !d.symbol.flags.is(Flags.Synthetic) =>
                    Some(d)
                  case _ => None
                }.map { d =>
                  d.symbol.name -> Input(resultType(literal.symbol.typeRef.memberType(d.symbol)), posOf(d.symbol, argPos),
                    Source.Expression(checkLiteralMember(literal.symbol, d)))
                }.toMap, None)
              case value =>
                val valueType = value.tpe.widen
                checkInstanceType(valueType)
                val (definition, read) = valueReader(value)
                val vsym = valueType.typeSymbol
                // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
                val members = (vsym.declaredFields ++ vsym.declaredMethods).filter { m =>
                  !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
                }
                if !members.exists(m => fields.exists(_.name == m.name)) then failArg(s"this argument supplies no field of $targetName")
                (members.map { m =>
                  m.name -> Input(resultType(valueType.memberType(m)), posOf(m, argPos), Source.FromValue(ctx => Select(read(ctx), m)))
                }.toMap, Some(definition))
            }
        }
    }
    if supplied.isEmpty then failArg(s"this argument supplies no field of $targetName")

    // --- Checks: report every problem, but don't generate code from a mismatching argument. ---

    val byName = fields.map(f => f.name -> f).toMap
    enum Kind { case Plain, BaseModule, Module, RawLayer, Mismatch }
    def classify(actual: TypeRepr, required: TypeRepr): Kind =
      if actual <:< required then Kind.Plain
      else if actual <:< TypeRepr.of[spike.e.BaseModule].appliedTo(required) then Kind.BaseModule
      else if actual <:< TypeRepr.of[spike.e.Module].appliedTo(required) then Kind.Module
      else if actual <:< TypeRepr.of[Override].appliedTo(required) then Kind.RawLayer
      else Kind.Mismatch
    val kinds = supplied.collect { case (name, input) if byName.contains(name) => name -> classify(input.tpe, resultType(byName(name).memberType)) }
    val modules = kinds.collect { case (name, Kind.BaseModule | Kind.Module) => name }.toSet
    val problems = supplied.toList.flatMap { case (name, Input(actual, pos, source)) =>
      byName.get(name) match {
        case None => Some((s"$name is not a member of $targetName", pos))
        case Some(f) =>
          val required = resultType(f.memberType)
          (kinds(name), source) match {
            case (_, Source.Expression(_)) if hasParams(f.memberType) =>
              Some((s"$name has a parameter list; a tuple or object literal can't define it, use an instance of a class", pos))
            case (Kind.Plain, _) => None
            case (Kind.BaseModule, _) => None
            case (Kind.Module, _) =>
              Option.when(isBase)((s"$name: a base needs a complete module (module(Overridable[...])), there is no layer below to override", pos))
            case (Kind.RawLayer, _) => Some((s"$name is a layer; wrap it in module(...) to plug it in as a module", pos))
            case (Kind.Mismatch, _) =>
              // Only a field of a dependency trait can be a module.
              val hint = if isLayerType(actual) && required.typeSymbol.flags.is(Flags.Trait) then " or a module(...) of it" else ""
              Some((s"$name has type ${tshow(actual)}, expected ${tshow(required)}$hint", pos))
          }
      }
    }
    val missing = fields.filter(f => isBase && !supplied.contains(f.name)).map(_.name)
    val allProblems =
      if missing.isEmpty then problems
      else problems :+ ((s"missing members of $targetName: ${missing.sorted.mkString(", ")}", argPos))
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // --- The generated class: its Slot holds the computation path and the shared cells. ---

    // A layer forwards everything it doesn't supply to `upper`; a base defines all (checked above).
    val generated = fields.filter(f => supplied.contains(f.name) || !isBase)
    // Supplied fields without parameters are computed once, in a Cell.
    val cellIndex = generated.filter(f => supplied.contains(f.name) && !hasParams(f.memberType)).map(_.name).zipWithIndex.toMap
    val parents = List(TypeTree.of[Object], TypeTree.of[A], TypeTree.of[Tagged[A]])
    val pathType = TypeRepr.of[List[Cell]]
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      List(
        Symbol.newVal(cls, "__knots_slot", TypeRepr.of[Tag.Slot], Flags.EmptyFlags, Symbol.noSymbol),
        Symbol.newMethod(cls, "__knots_at", MethodType(List("path"))(_ => List(pathType), _ => target), Flags.Override, Symbol.noSymbol)
      ) ++ generated.map(f => Symbol.newMethod(cls, f.name, f.memberType, if isView then Flags.EmptyFlags else Flags.Override, Symbol.noSymbol))
        ++ Option.when(isView)(Symbol.newMethod(cls, "selectDynamic",
          MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol)),
      selfType = None)
    val slotField = cls.declaredField("__knots_slot")
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
      if modules(f.name) then focused(f, ctx, value) else value
    }
    // A module's layer run focused through its field (see Overridable.focus).
    def focused(f: Field, ctx: Term, layer: Term): Term = resultType(f.memberType).asType match {
      case '[t] =>
        val get = Lambda(owner, MethodType(List(Symbol.freshName("root")))(_ => List(target), _ => TypeRepr.of[t]),
          (meth, params) => f.fromUpper(params.head.asInstanceOf[Term], Nil).changeOwner(meth)).asExprOf[A => t]
        if isBase then '{ Overridable.focus[A, t](${ ctx.asExprOf[Self[A]] }, $get, ${ layer.asExprOf[BaseModule[t]] }) }.asTerm
        else '{ Override.focus[A, t](${ ctx.asExprOf[Upper[A]] }, $get, ${ layer.asExprOf[Module[t]] }) }.asTerm
    }
    def fieldBody(f: Field, argss: List[List[Tree]]): Term = supplied.get(f.name) match {
      case Some(Input(_, _, source)) if cellIndex.contains(f.name) =>
        val fn = Lambda(owner, MethodType(List(Symbol.freshName("path")))(_ => List(pathType), _ => TypeRepr.of[Any]),
          (meth, params) => compute(f, source, params.head.asInstanceOf[Term]).changeOwner(meth))
        // Re-tagged on read: a cached module object would otherwise lose the path at the boundary.
        resultType(f.memberType).asType match {
          case '[t] => '{ Tag[Any]($slot.cells(${ Expr(cellIndex(f.name)) }).get($slot.path)(${ fn.asExprOf[List[Cell] => Any] }), $slot.path).asInstanceOf[t] }.asTerm
        }
      case Some(Input(_, _, source)) => applyArgs(compute(f, source, '{ $slot.path }.asTerm), argss)
      case None => f.fromUpper('{ Tag(${ s.asExprOf[Upper[A]] }.upper, $slot.path) }.asTerm, argss)
    }
    // A new instance of the generated class with the given path and cells.
    def newInstance(owner: Symbol, path: Expr[List[Cell]], cells: Expr[Array[Cell]]): Term = {
      val instance = Symbol.newVal(owner, Symbol.freshName("instance"), cls.typeRef, Flags.EmptyFlags, Symbol.noSymbol)
      val instanceSlot = Select(Ref(instance), slotField).asExprOf[Tag.Slot]
      Block(
        List(
          ValDef(instance, Some(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil))),
          '{ $instanceSlot.path = $path }.asTerm,
          '{ $instanceSlot.cells = $cells }.asTerm),
        Typed(Ref(instance), TypeTree.of[A]))
    }

    val at = cls.declaredMethod("__knots_at").head
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
}
