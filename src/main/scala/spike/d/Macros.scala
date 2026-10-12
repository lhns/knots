package spike.d

import scala.quoted.*

// Builds one layer of an A from `o`, so no hand-written Delegate is needed. A is a trait, or a
// View[F] for a named tuple F (its fields are then F's fields).
// - an A (a variable, a complete `new A { ... }`) is used as is;
// - anything else supplies some fields: a named-tuple literal `(dep2 = ...)` (each element is its own
//   computation), an object literal `new { def dep2 = ... }`, an instance of some class, or a
//   named-tuple value. The last three are held in a `partial` val and the fields read from it. A
//   generated subclass of A takes those and forwards the rest to `upper`; a base (`isBase`) has no
//   upper and must define every abstract member of A.
// Each supplied field is computed once, in a Cell (see Cell, which also detects cycles).
// Object literals may not have strict `val` members: creating `partial` then evaluates nothing, so a
// cycle can't hang inside it.
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

    // name -> (type, position, how to compute it); the statements defining `partial`, if needed;
    // and problems with the argument itself.
    final case class Inputs(supplied: Map[String, (TypeRepr, Position, Term)], statements: List[Statement], problems: List[(String, Position)])
    val inputs: Inputs = namedTupleLiteral(body).orElse(namedTupleFields(body.tpe)) match {
      case Some(fs) =>
        literalElements(moved, fs.length) match {
          case Some(terms) => Inputs(fs.zip(terms).map { case ((n, t), e) => n -> (t, e.pos, e) }.toMap, Nil, Nil)
          case None => // not a literal: read the fields from the tuple value
            val (definition, partial) = partialVal(moved)
            val supplied = fs.zipWithIndex.map { case ((n, t), i) =>
              n -> (t, o.asTerm.pos, t.asType match {
                case '[t] => '{ ${ partial.asExprOf[Any] }.asInstanceOf[Product].productElement(${ Expr(i) }).asInstanceOf[t] }.asTerm
              })
            }.toMap
            Inputs(supplied, List(definition), Nil)
        }
      case None =>
        // An object literal's type is widened to Object, so its class definition moves along to keep its type.
        val (literalClass, construct) = moved match {
          case Block(List(cd: ClassDef), e) => (List(cd), strip(e))
          case e => (Nil, e)
        }
        val valueType = construct.tpe.widen
        if valueType =:= TypeRepr.of[Object] then
          report.errorAndAbort(s"expected (name = ...), new { ... }, an instance of a class, or a ${target.show}", o)
        val (definition, partial) = partialVal(construct)
        // Only public members the class declares itself: inherited and compiler-generated ones are ignored.
        val supplied = (valueType.typeSymbol.declaredFields ++ valueType.typeSymbol.declaredMethods).filter { m =>
          !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
        }.map { m =>
          m.name -> (resultType(valueType.memberType(m)), m.pos.getOrElse(o.asTerm.pos), Select(partial, m): Term)
        }.toMap
        val problems = literalClass.flatMap(_.body).flatMap {
          case d: Definition if !d.symbol.flags.is(Flags.Private) && !d.symbol.flags.is(Flags.Protected) && !d.symbol.flags.is(Flags.Synthetic) =>
            checkLiteralMember(d, o.asTerm.pos)
          case _ => None
        }
        Inputs(supplied, literalClass :+ definition, problems)
    }
    val supplied = inputs.supplied

    // --- Checks: report every problem, but don't generate code from a mismatching argument. ---

    val byName = fields.map(f => f.name -> f).toMap
    val typeProblems = supplied.toList.flatMap { case (name, (actual, pos, _)) =>
      byName.get(name) match {
        case None => Some((s"$name is not a member of ${target.show}", pos))
        case Some(f) =>
          val required = resultType(f.memberType)
          Option.when(!(actual <:< required))((s"$name has type ${actual.show}, expected ${required.show}", pos))
      }
    }
    val missing = fields.filter(f => isBase && f.requiredInBase && !supplied.contains(f.name)).map(_.name)
    val missingProblem = Option.when(missing.nonEmpty)((s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o.asTerm.pos))
    val allProblems = inputs.problems ++ typeProblems ++ missingProblem
    if allProblems.nonEmpty then {
      allProblems.foreach(report.error(_, _))
      return '{ ??? }
    }

    // --- The generated class. ---

    // A layer forwards everything it doesn't supply to `upper`; a base keeps A's concrete defaults.
    val generated = fields.filter(f => supplied.contains(f.name) || !isBase)
    // Supplied fields without parameters are computed once, in a Cell.
    def isCached(f: Field) = supplied.contains(f.name) && !hasParams(f.memberType)
    def cellName(f: Field) = s"${f.name}$$cell"
    val upper = Option.when(!isBase)('{ ${ s.asExprOf[Upper[A]] }.upper }.asTerm)

    val parents = List(TypeTree.of[Object], TypeTree.of[A])
    val cls = Symbol.newClass(owner, "Derived", parents.map(_.tpe), cls =>
      generated.flatMap { f =>
        Symbol.newMethod(cls, f.name, f.memberType, if isView then Flags.EmptyFlags else Flags.Override, Symbol.noSymbol) ::
          Option.when(isCached(f))(Symbol.newVal(cls, cellName(f), TypeRepr.of[Cell], Flags.Private, Symbol.noSymbol)).toList
      } ++ Option.when(isView)(Symbol.newMethod(cls, "selectDynamic",
        MethodType(List("name"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Any]), Flags.Override, Symbol.noSymbol)),
      selfType = None)

    def compute(f: Field, argss: List[List[Tree]]): Term = supplied.get(f.name) match {
      case Some((_, _, term)) => applyArgs(term, argss)
      case None => f.fromUpper(upper.get, argss)
    }
    val fieldDefs = generated.flatMap { f =>
      val get = cls.declaredMethod(f.name).head
      val cell = cls.declaredField(cellName(f))
      if !cell.exists then List(DefDef(get, argss => Some(compute(f, argss).changeOwner(get))))
      else resultType(f.memberType).asType match {
        case '[t] => List(
          ValDef(cell, Some('{ new Cell(${ Expr(f.name) }) }.asTerm.changeOwner(cell))),
          DefDef(get, _ => Some('{ ${ Select(This(cls), cell).asExprOf[Cell] }.get(${ compute(f, Nil).asExprOf[Any] }).asInstanceOf[t] }.asTerm.changeOwner(get))))
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
  private def checkLiteralMember(using q: Quotes)(d: q.reflect.Definition, fallback: q.reflect.Position): Option[(String, q.reflect.Position)] = {
    import q.reflect.*
    d match {
      case v: ValDef if !v.symbol.flags.is(Flags.Lazy) =>
        Some((s"${v.name}: use def (or lazy val), the layer caches it", v.symbol.pos.getOrElse(fallback)))
      case _ => None
    }
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
