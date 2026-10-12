package spike.b

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import scala.quoted.*

// Implements a trait from an object literal holding only some of its members, so no
// hand-written Delegate is needed. The literal is checked at compile time (a macro reads
// its members; the literal's type is widened to Object, so the tree is all there is) and
// the trait is implemented at runtime with a java.lang.reflect.Proxy.
object Derive {
  // Every member of the object literal in `o` must be a member of A with a conforming type.
  // With `complete`, every abstract member of A must be defined too.
  inline def check[A, S](inline o: S ?=> Any, inline complete: Boolean): Unit = ${ checkImpl[A, S]('o, 'complete) }

  private def checkImpl[A: Type, S: Type](o: Expr[S ?=> Any], complete: Expr[Boolean])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val target = TypeRepr.of[A]
    val targetSym = target.typeSymbol
    if !targetSym.flags.is(Flags.Trait) then report.errorAndAbort(s"${target.show} must be a trait")

    val firstClass = new TreeAccumulator[Option[Symbol]] {
      def foldTree(found: Option[Symbol], tree: Tree)(owner: Symbol): Option[Symbol] =
        found.orElse(tree match {
          case cd: ClassDef => Some(cd.symbol)
          case _ => foldOverTree(None, tree)(owner)
        })
    }
    def lambdaBody(t: Term): Option[Term] = t match {
      case Inlined(_, _, e) => lambdaBody(e)
      case Block(List(d: DefDef), _: Closure) => d.rhs
      case _ => None
    }
    // Any value that already is an A (a variable, a `new A { ... }`) is used as is.
    if lambdaBody(o.asTerm).exists(_.tpe.widen <:< target) then return '{ () }

    val anon = firstClass.foldTree(None, o.asTerm)(Symbol.spliceOwner)
      .getOrElse(report.errorAndAbort(s"expected an object literal new { ... } or a ${target.show}", o))

    def resultType(t: TypeRepr): TypeRepr = t match {
      case ByNameType(r) => r
      case t => t
    }
    val members = (anon.declaredFields ++ anon.declaredMethods).filter { m =>
      !m.isClassConstructor && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) && !m.flags.is(Flags.Synthetic)
    }
    for m <- members do {
      val pos = m.pos.getOrElse(o.asTerm.pos)
      targetSym.methodMember(m.name).headOption match {
        case None => report.error(s"${m.name} is not a member of ${target.show}", pos)
        case Some(expected) =>
          val actualType = resultType(anon.typeRef.memberType(m))
          val expectedType = resultType(target.memberType(expected))
          if !(actualType <:< expectedType) then
            report.error(s"${m.name} has type ${actualType.show}, expected ${expectedType.show}", pos)
      }
    }
    if complete.valueOrAbort then {
      val defined = members.map(_.name).toSet
      val missing = targetSym.methodMembers.filter(s => s.flags.is(Flags.Deferred) && !defined(s.name)).map(_.name)
      if missing.nonEmpty then report.error(s"missing members of ${target.show}: ${missing.sorted.mkString(", ")}", o)
    }
    '{ () }
  }

  // Calls go to `partial` if it has the member, otherwise to `fallback` (or A's default method).
  def implement[A](cls: Class[?], partial: AnyRef, fallback: Option[() => A]): A =
    if cls.isInstance(partial) then partial.asInstanceOf[A]
    else {
      val methods = partial.getClass.getMethods.map(m => (m.getName, m.getParameterCount) -> m).toMap
      val handler: InvocationHandler = (proxy, m, args) => {
        val as = if args == null then Array.empty[AnyRef] else args
        if m.getDeclaringClass == classOf[Object] then m.getName match {
          case "toString" => s"${cls.getSimpleName}($partial)"
          case "hashCode" => Int.box(System.identityHashCode(proxy))
          case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq as(0))
          case _ => m.invoke(partial, as*)
        }
        else unwrap(methods.get((m.getName, m.getParameterCount)) match {
          case Some(pm) =>
            pm.setAccessible(true) // the literal's class isn't public
            pm.invoke(partial, as*)
          case None => fallback match {
            case Some(lower) => m.invoke(lower(), as*)
            case None if m.isDefault => InvocationHandler.invokeDefault(proxy, m, as*)
            case None => throw new IllegalStateException(s"no definition for ${m.getName}")
          }
        })
      }
      Proxy.newProxyInstance(cls.getClassLoader, Array(cls), handler).asInstanceOf[A]
    }

  private def unwrap(call: => AnyRef): AnyRef =
    try call
    catch case e: InvocationTargetException => throw e.getCause
}
