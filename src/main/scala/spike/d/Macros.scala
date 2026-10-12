package spike.d

import scala.collection.mutable.ListBuffer
import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait with only
// abstract members, or a View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is: not cached, no cycle detection;
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)` (each element is its own
//   computation), an object literal `new { def dep2 = ... }`, an instance of some class, or a
//   named-tuple value. The last three are held in a `partial` val and the fields read from it. A
//   generated subclass of A takes those and forwards the rest to `upper`; a base (`isBase`) has no
//   upper and must define every member of A.
// Each supplied field is computed once, in a Cell (see Cell, which also detects cycles).
// Object literals may not have strict `val` members: creating `partial` then evaluates nothing, so a
// cycle can't hang inside it.
// A member of A that is a module (`db = module(...)`) runs its layer focused through the field.
//
// A concrete member of A (with a body) is rejected: its `this` would be the layer's own object, not
// `self`, so it would not see later layers (unlike the abstract members, which go through `self`).
//
// `upper` chains are plain nested calls: a very deep one (about 1000 layers overriding one field) can
// overflow the stack.
object Macros {
  inline def derive[A, S <: Self[A]](inline o: S ?=> Any, s: S, inline isBase: Boolean): A =
    ${ deriveImpl[A, S]('o, 's, 'isBase) }

  // Names a View can't have as fields: members of the generated class itself.
  private val reservedViewFields = Set(
    "selectDynamic", "toString", "hashCode", "equals", "getClass", "notify", "notifyAll", "wait", "clone", "finalize",
    "eq", "ne", "synchronized", "isInstanceOf", "asInstanceOf", "==", "!=", "##")

  private def deriveImpl[A: Type, S: Type](o: Expr[S ?=> Any], s: Expr[S], isBaseExpr: Expr[Boolean])(using Quotes): Expr[A] = {
    import quotes.reflect.*

    def sh(t: TypeRepr): String = t.show(using Printer.TypeReprShortCode)

    val target = TypeRepr.of[A]
    val targetName = sh(target)
    val dealiased = target.dealias
    val targetSym = dealiased.typeSymbol
    val isBase = isBaseExpr.valueOrAbort
    val owner = Symbol.spliceOwner
    val argPos = o.asTerm.pos

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
    // The position of a symbol when it is in the same source as the argument; otherwise (library
    // symbols, other files) the argument's.
    def posOf(sym: Symbol): Position = sym.pos.filter(_.sourceFile == argPos.sourceFile).getOrElse(argPos)
    def isContextFunction(t: TypeRepr) = t.dealias match {
      case AppliedType(tc, _) => tc.typeSymbol.fullName.startsWith("scala.ContextFunction")
      case _ => false
    }
    def isLibraryClass(sym: Symbol) = {
      val n = sym.fullName
      n.startsWith("scala.") || n.startsWith("java.") || n.startsWith("javax.")
    }

    val layerTypes = List(TypeRepr.of[Override], TypeRepr.of[Overridable], TypeRepr.of[Module], TypeRepr.of[BaseModule]).map(_.typeSymbol)
    def isLayer(t: TypeRepr): Boolean = t.widen.dealiasKeepOpaques match {
      case AppliedType(tc, _) => layerTypes.contains(tc.typeSymbol)
      case _ => false
    }

    // --- The fields of A: a View[F]'s are F's; a trait's are its members, all abstract. ---

    final case class Field(name: String, memberType: TypeRepr, fromUpper: (Term, List[List[Tree]]) => Term)
    val targetProblems = ListBuffer.empty[String]
    val viewOf = target.baseType(TypeRepr.of[View].typeSymbol) match {
      case AppliedType(_, List(f)) => Some(f)
      case _ => None
    }
    val isView = viewOf.nonEmpty

    // A named tuple type as (names, values), seen through aliases and match types.
    def namedTupleParts(t: TypeRepr): Option[(TypeRepr, TypeRepr)] = t.widen match {
      case AppliedType(tc, List(names, values)) if tc.typeSymbol.name == "NamedTuple" => Some((names, values))
      case w =>
        val d = w.dealiasKeepOpaques
        if d != w then namedTupleParts(d)
        else { val simple = w.simplified; if simple != w then namedTupleParts(simple) else None }
    }
    def tupleElems(t: TypeRepr): Option[List[TypeRepr]] = t.dealias match {
      case AppliedType(tc, List(h, tail)) if tc.typeSymbol == Symbol.requiredClass("scala.*:") => tupleElems(tail).map(h :: _)
      case AppliedType(tc, args) if tc.typeSymbol.fullName.matches("scala\\.Tuple\\d+") => Some(args)
      case d if d =:= TypeRepr.of[EmptyTuple] => Some(Nil)
      case d =>
        val simple = d.simplified
        if simple != d then tupleElems(simple) else None
    }
    // The fields of a named tuple type; None if it is not one or its shape can't be worked out.
    def namedTupleFields(t: TypeRepr): Option[List[(String, TypeRepr)]] =
      namedTupleParts(t).flatMap { (names, values) =>
        for {
          ns <- tupleElems(names)
          vs <- tupleElems(values)
          if ns.length == vs.length
          strings = ns.collect { case ConstantType(StringConstant(n)) => n }
          if strings.length == ns.length
        } yield strings.zip(vs)
      }

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

    val fields: List[Field] = viewOf match {
      case Some(f) =>
        val fs = namedTupleFields(f).getOrElse(report.errorAndAbort(s"${sh(f)} is not a named tuple", argPos))
        fs.foreach { (n, _) => if reservedViewFields(n) then targetProblems += s"field name $n is reserved" }
        fs.map { (n, t) =>
          Field(n, ByNameType(t), (upper, _) => t.asType match {
            case '[t] => '{ ${ upper.asExprOf[View[?]] }.selectDynamic(${ Expr(n) }).asInstanceOf[t] }.asTerm
          })
        }
      case _ =>
        dealiased match {
          case _: Refinement => report.errorAndAbort(s"$targetName is a refinement type; only a plain trait is supported", argPos)
          case _ =>
        }
        if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"$targetName must be a trait", argPos)

        val bases = dealiased.baseClasses
        val notAnyRef = Set(defn.AnyClass, defn.ObjectClass, defn.MatchableClass)
        bases.find(c => c != targetSym && !c.flags.is(Flags.Trait) && !notAnyRef(c)).foreach { c =>
          targetProblems += s"${targetSym.name} extends the class ${c.name}; only traits are supported"
        }
        val pc = targetSym.primaryConstructor
        if pc.exists && pc.paramSymss.flatten.exists(_.isTerm) then
          targetProblems += s"${targetSym.name} has constructor parameters, which are not supported"
        val selfType = try targetSym.tree match {
          case ClassDef(_, _, _, Some(self), _) if !(self.tpt.tpe =:= targetSym.typeRef) => true
          case _ => false
        } catch { case _: Exception => false }
        val all = (targetSym.methodMembers ++ targetSym.fieldMembers).distinct.filterNot(m => notAnyRef(m.owner))
        // A member of the self type's classes: its owner is not a base class of the trait.
        if selfType || all.exists(m => !bases.contains(m.owner)) then
          targetProblems += s"${targetSym.name} has a self-type, which is not supported"

        if targetProblems.isEmpty then {
          val members = all.filter(m => !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.isType &&
            !m.flags.is(Flags.ParamAccessor))
          // An abstract var is a getter and a setter.
          val abstractVars = members.filter(m => m.flags.is(Flags.Deferred) && m.flags.is(Flags.Mutable) && !m.name.endsWith("_=")).map(_.name).toSet
          abstractVars.toList.sorted.foreach { v =>
            val ownerName = members.find(_.name == v).get.owner.name
            targetProblems += s"abstract var $v in $ownerName is not supported; declare it as def $v"
          }
          val defaults = members.filter(_.name.contains("$default$")).map(_.name.takeWhile(_ != '$')).distinct
          defaults.sorted.foreach(n => targetProblems += s"$n has default arguments, which are not supported")
          members.filterNot(m => m.flags.is(Flags.Synthetic) || m.name.contains("$default$") || abstractVars(m.name) ||
              abstractVars(m.name.stripSuffix("_=")) || (m.name.endsWith("_=") && members.exists(_.name == m.name.stripSuffix("_=")))).foreach { m =>
            if !m.flags.is(Flags.Deferred) then
              targetProblems += s"concrete member ${m.name} in ${m.owner.name} is bound to its own layer (its body uses this, not self); " +
                "make it abstract and define it in the base"
            else if m.flags.is(Flags.Protected) then
              targetProblems += s"abstract protected member ${m.name} in ${m.owner.name} is not supported"
            else if m.isValDef then
              targetProblems += s"abstract val ${m.name} in ${m.owner.name} is not supported; declare it as def ${m.name}"
            else if memberMentionsThis(m) then
              targetProblems += s"member ${m.name} in ${m.owner.name} has a type that mentions this.type, which is not supported"
          }
          val overloaded = members.filter(_.flags.is(Flags.Deferred)).groupBy(_.name).collect { case (n, ms) if ms.size > 1 => n }
          overloaded.toList.sorted.foreach(n => targetProblems += s"overloaded member $n in $targetName is not supported")
        }
        if targetProblems.nonEmpty then Nil
        else
          all.filter(m => m.isDefDef && m.flags.is(Flags.Deferred) && !m.isClassConstructor && !m.flags.is(Flags.Private))
            .map(m => Field(m.name, target.memberType(m), (upper, argss) => applyArgs(Select(upper, m), argss)))
    }
    if targetProblems.nonEmpty then {
      targetProblems.foreach(report.error(_, argPos))
      return '{ ??? }
    }

    // --- What `o` supplies. ---

    val (param, body) = strip(o.asTerm) match {
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(rhs))), _: Closure) => (p.symbol, rhs)
      case _ => report.errorAndAbort("expected the argument inline", o)
    }
    val genericProblem = s"expected (name = ...), new { ... }, an instance of a class, or a $targetName"
    strip(body) match {
      case Literal(NullConstant()) => report.errorAndAbort(s"null is not supported: $genericProblem", body.pos)
      case _ =>
    }
    // Complete instances are used as is: not cached, no cycle detection.
    if body.tpe.widen <:< target then return '{ $o(using $s).asInstanceOf[A] }

    // The argument, moved into the expansion with its Self/Upper parameter replaced by `s`.
    val moved = new TreeMap {
      override def transformTerm(t: Term)(owner: Symbol): Term = t match {
        case id: Ident if id.symbol == param => s.asTerm
        case _ => super.transformTerm(t)(owner)
      }
    }.transformTerm(strip(body))(owner).changeOwner(owner)

    // The `partial` val holding the argument's value, and a reference to it.
    def partialVal(value: Term): (Statement, Term) = {
      val sym = Symbol.newVal(owner, "partial", value.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
      (ValDef(sym, Some(value.changeOwner(sym))), Ref(sym))
    }

    // A named-tuple literal desugars to `x$proxy = TupleN.apply(e1, ..., en)` (TupleXXL.apply(Seq(..))
    // from 23 elements) followed by an ascription of that proxy to the NamedTuple type, nested in
    // Inlined/Block nodes. The tuple is found by following the final ascription to its proxy.
    // Result: Some(Left(statement)) for statements before the literal, Some(Right(elements)), or None
    // if it is not a literal.
    def spine(t: Term): (List[Statement], List[Statement], Term) = t match {
      case Inlined(_, bindings, e) => val (stats, binds, last) = spine(e); (stats, bindings ++ binds, last)
      case Block(stats, e) => val (more, binds, last) = spine(e); (stats ++ more, binds, last)
      case last => (Nil, Nil, last)
    }
    def unwrapTuple(t: Term): Term = t match {
      case Inlined(_, Nil, e) => unwrapTuple(e)
      case Typed(e, _) => unwrapTuple(e)
      case TypeApply(Select(e, "$asInstanceOf$"), _) => unwrapTuple(e)
      case t => t
    }
    def literal(t: Term, arity: Int): Option[Either[Statement, List[Term]]] = {
      val (stats, bindings, last) = spine(t)
      last match {
        case Typed(e, tpt) if namedTupleParts(tpt.tpe).isDefined =>
          strip(e) match {
            case id: Ident =>
              bindings.collectFirst { case v: ValDef if v.symbol == id.symbol => v }.flatMap(_.rhs).flatMap { rhs =>
                unwrapTuple(rhs) match {
                  case Apply(fn, List(Typed(Repeated(args, _), _))) if fn.symbol.owner.fullName.endsWith("TupleXXL$") => Some(args)
                  case Apply(fn, args) if fn.symbol.owner.fullName.matches("scala\\.Tuple\\d+\\$") => Some(args)
                  case _ => None
                }
              }.filter(_.length == arity).map { elements =>
                val own = (b: Statement) => b match {
                  case v: ValDef => v.symbol == id.symbol || v.name.contains("$proxy") || v.name == "NamedTuple$_this"
                  case _ => false
                }
                (stats ++ bindings.filterNot(own)).headOption match {
                  case Some(stat) => Left(stat)
                  case None => Right(elements)
                }
              }
            case _ => None
          }
        case _ => None
      }
    }

    // name -> (type, position, how to compute it); the statements defining `partial`, if needed;
    // problems with the argument itself (fatal: nothing else is checked); how to hint at members
    // that aren't fields.
    final case class Supplied(tpe: TypeRepr, pos: Position, term: Term, viaMember: Boolean = false)
    final case class Inputs(supplied: Map[String, Supplied], statements: List[Statement], problems: List[(String, Position)], notMember: String)
    def fatal(msg: String, pos: Position = argPos) = Inputs(Map.empty, Nil, List((msg, pos)), "")

    // A literal's own type sees through the opaque NamedTuple, so take it from the final ascription.
    val argType = spine(body)._3 match {
      case Typed(_, tpt) if namedTupleParts(tpt.tpe).isDefined => tpt.tpe
      case _ => body.tpe.widen
    }
    val inputs: Inputs =
      if isLayer(argType) then fatal("this is a layer; combine layers with |+|")
      else if namedTupleParts(argType).isDefined then namedTupleFields(argType) match {
        case None => fatal(s"cannot determine the fields of ${sh(argType)}")
        case Some(fs) =>
          literal(moved, fs.length) match {
            case Some(Left(stat)) => fatal("statements before the named tuple are not supported; put them inside the fields", stat.pos)
            case Some(Right(terms)) => Inputs(fs.zip(terms).map { case ((n, t), e) => n -> Supplied(t, e.pos, e) }.toMap, Nil, Nil, "")
            case None => // not a literal: read the fields from the tuple value
              val (definition, partial) = partialVal(moved)
              val supplied = fs.zipWithIndex.map { case ((n, t), i) =>
                n -> Supplied(t, argPos, t.asType match {
                  case '[t] => '{ ${ partial.asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }).asInstanceOf[t] }.asTerm
                })
              }.toMap
              Inputs(supplied, List(definition), Nil, "")
          }
      }
      else {
        // An object literal's type is widened to Object, so its class definition moves along to keep its type.
        val (literalClass, construct) = moved match {
          case Block(List(cd: ClassDef), e) => (List(cd), strip(e))
          case e => (Nil, e)
        }
        val valueType = construct.tpe.widen
        val valueSym = valueType.typeSymbol
        if !valueSym.isClassDef || valueSym == defn.ObjectClass || (literalClass.isEmpty && isLibraryClass(valueSym)) then fatal(genericProblem)
        else {
          val (definition, partial) = partialVal(construct)
          // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
          val declared = (valueSym.declaredFields ++ valueSym.declaredMethods).filter { m =>
            !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
          }
          val vars = declared.filter(_.flags.is(Flags.Mutable)).map(_.name).toSet
          val supplied = declared.filterNot(m => m.name.endsWith("_=") && vars(m.name.stripSuffix("_="))).map { m =>
            m.name -> Supplied(resultType(valueType.memberType(m)), posOf(m), Select(partial, m): Term, viaMember = true)
          }.toMap
          val problems = literalClass.flatMap(_.body).flatMap {
            case d: Definition if !d.symbol.flags.is(Flags.Private) && !d.symbol.flags.is(Flags.Protected) && !d.symbol.flags.is(Flags.Synthetic) =>
              checkLiteralMember(d, posOf(d.symbol))
            case _ => None
          }
          Inputs(supplied, literalClass :+ definition, problems, " (other members of an object literal or class must be private)")
        }
      }
    val supplied = inputs.supplied

    // --- Checks: report every problem, but don't generate code from a mismatching argument. ---

    val byName = fields.map(f => f.name -> f).toMap
    val moduleOf = TypeRepr.of[Module]
    val baseModuleOf = TypeRepr.of[BaseModule]
    val overrideOf = TypeRepr.of[Override]
    // A module field is given module(layer) (see Overridable.focus); a raw layer is an error.
    val modules = supplied.collect {
      case (name, Supplied(actual, _, _, _)) if byName.get(name).exists(f => actual <:< moduleOf.appliedTo(resultType(f.memberType))) => name
    }.toSet
    val typeProblems = supplied.toList.flatMap { case (name, Supplied(actual, pos, _, viaMember)) =>
      byName.get(name) match {
        case None =>
          val reason =
            if name == "toString" || name == "hashCode" || name == "equals" then s"$name: overriding members of Object is not supported"
            else s"$name is not a member of $targetName${inputs.notMember}"
          Some((reason, pos))
        case Some(f) =>
          val required = resultType(f.memberType)
          // The compiler rewrites such a member of a class to take the context parameter, after the macro.
          if viaMember && isContextFunction(required) then
            Some((s"$name: a member of context function type can't be supplied by an object literal or a class; use ($name = ...)", pos))
          else if modules(name) then
            Option.when(isBase && !(actual <:< baseModuleOf.appliedTo(required)))(
              (s"$name: a base needs a complete module (module(Overridable[${sh(required)}])), there is no layer below to override", pos))
          else if actual <:< required then None
          else if actual <:< overrideOf.appliedTo(required) then
            Some((s"$name is a layer; wrap it in module(...) to plug it in as a module", pos))
          else if isLayer(actual) && !isLayer(required) then Some((s"$name has type ${sh(actual)}, expected module(...) of ${sh(required)}", pos))
          else Some((s"$name has type ${sh(actual)}, expected ${sh(required)}", pos))
      }
    }
    val missing = fields.filter(f => isBase && !supplied.contains(f.name)).map(_.name)
    val missingProblem = Option.when(missing.nonEmpty)((s"missing members of $targetName: ${missing.sorted.mkString(", ")}", argPos))
    val allProblems =
      if supplied.isEmpty then
        if inputs.problems.nonEmpty then inputs.problems else List((s"the argument supplies no field of $targetName", argPos))
      else inputs.problems ++ typeProblems ++ missingProblem
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // --- The generated class. ---

    // A layer forwards everything it doesn't supply to `upper`; a base supplies everything (checked above).
    val generated = fields
    // Supplied fields without parameters are computed once, in a Cell.
    def isCached(f: Field) = supplied.contains(f.name) && !hasParams(f.memberType)
    // A name no member of A has, so a cell can't collide with a field.
    val cellNames = fields.map { f =>
      var n = s"${f.name}$$cell"
      while byName.contains(n) do n += "$"
      f.name -> n
    }.toMap
    val upper = Option.when(!isBase)('{ ${ s.asExprOf[Upper[A]] }.upper }.asTerm)

    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      generated.flatMap { f =>
        Symbol.newMethod(cls, f.name, f.memberType, if isView then Flags.EmptyFlags else Flags.Override, Symbol.noSymbol) ::
          Option.when(isCached(f))(Symbol.newVal(cls, cellNames(f.name), TypeRepr.of[Cell], Flags.Private, Symbol.noSymbol)).toList
      } ++ Option.when(isView)(Symbol.newMethod(cls, "selectDynamic",
        MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol)),
      selfType = None)

    def compute(f: Field, argss: List[List[Tree]]): Term = supplied.get(f.name) match {
      case Some(Supplied(_, _, term, _)) if modules(f.name) => module(f, term)
      case Some(Supplied(_, _, term, _)) => applyArgs(term, argss)
      case None => f.fromUpper(upper.get, argss)
    }
    // A module's layer run focused through its field (see Overridable.focus).
    def module(f: Field, layer: Term): Term = resultType(f.memberType).asType match {
      case '[t] =>
        val get = Lambda(owner, MethodType(List("root"))(_ => List(target), _ => TypeRepr.of[t]),
          (meth, params) => f.fromUpper(params.head.asInstanceOf[Term], Nil).changeOwner(meth)).asExprOf[A => t]
        if isBase then '{ Overridable.focus[A, t](${ s.asExprOf[Self[A]] }, $get, ${ layer.asExprOf[BaseModule[t]] }) }.asTerm
        else '{ Override.focus[A, t](${ s.asExprOf[Upper[A]] }, $get, ${ layer.asExprOf[Module[t]] }) }.asTerm
    }
    val fieldDefs = generated.flatMap { f =>
      val get = cls.declaredMethod(f.name).head
      val cell = cls.declaredField(cellNames(f.name))
      if !cell.exists then List(DefDef(get, argss => Some(compute(f, argss).changeOwner(get))))
      else resultType(f.memberType).asType match {
        case '[t] => List(
          ValDef(cell, Some('{ new Cell(${ Expr(f.name) }, () => ${ compute(f, Nil).asExprOf[Any] }) }.asTerm.changeOwner(cell))),
          DefDef(get, _ => Some('{ ${ Select(This(cls), cell).asExprOf[Cell] }.get.asInstanceOf[t] }.asTerm.changeOwner(get))))
      }
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
    val classDef = ClassDef(cls, parents, fieldDefs ++ dispatchDef)

    val instance = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[A])
    Block(inputs.statements :+ classDef, instance).asExprOf[A]
  }

  // An object literal member is cached by the layer, so it must be a def or lazy val: a strict val
  // would be evaluated together with the whole literal, and a cycle through it would hang.
  private def checkLiteralMember(using q: Quotes)(d: q.reflect.Definition, pos: q.reflect.Position): Option[(String, q.reflect.Position)] = {
    import q.reflect.*
    d match {
      case v: ValDef if !v.symbol.flags.is(Flags.Lazy) =>
        Some((s"${v.name}: use def (or lazy val), the layer caches it", pos))
      case _ => None
    }
  }
}
