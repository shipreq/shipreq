package shipreq.webapp.macros

import scala.reflect.macros.blackbox.Context

object MacroUtils {

  def fail(c: Context, msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  def concreteWeakTypeOf[T: c.WeakTypeTag](c: Context): c.universe.Type = {
    val t = c.universe.weakTypeOf[T]
    ensureConcrete(c)(t)
    t
  }

  def ensureConcrete(c: Context)(t: c.universe.Type): Unit = {
    val sym = t.typeSymbol.asClass
    if (sym.isAbstract)
      fail(c, s"ensureConcrete: [${sym.name}] is abstract which is not allowed.")
    if (sym.isTrait)
      fail(c, s"ensureConcrete: [${sym.name}] is a trait which is not allowed.")
    if (sym.isSynthetic)
      fail(c, s"ensureConcrete: [${sym.name}] is synthetic which is not allowed.")
  }

  def primaryConstructorParams[T: c.WeakTypeTag](c: Context): List[c.universe.Symbol] = {
    import c.universe._
    val T = weakTypeOf[T]
    T.decls
      .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
      .getOrElse(fail(c, "Unable to discern primary constructor."))
      .paramLists
      .headOption
      .getOrElse(fail(c, "Primary constructor missing paramList."))
  }

  def nameAndType[T: c.WeakTypeTag](c: Context)(s: c.universe.Symbol): (c.universe.TermName, c.universe.Type) = {
    import c.universe._
    val T = weakTypeOf[T]

    def paramType(name: TermName): Type =
      T.decl(name).typeSignatureIn(T) match {
        case NullaryMethodType(t) => t
        case t                    => t
      }

    val a = s.asTerm.name
    val A = paramType(a)
    (a, A)
  }

  sealed trait FindSubClasses
  case object DirectOnly extends FindSubClasses
  case object LeavesOnly extends FindSubClasses
  case object Everything extends FindSubClasses

  /**
   * Constraints:
   * - Type must be sealed.
   * - Type must be abstract or a trait.
   */
  def findConcreteTypes(c: Context)(tpe: c.universe.Type, f: FindSubClasses): Set[c.universe.ClassSymbol] = {
    import c.universe._

    tpe.typeConstructor // https://issues.scala-lang.org/browse/SI-7755
    val sym = tpe.typeSymbol.asClass

    if (!sym.isSealed)
      fail(c, s"${sym.name} must be sealed.")

    if (!(sym.isAbstract || sym.isTrait))
      fail(c, s"${sym.name} must be abstract or a trait.")

    if (sym.knownDirectSubclasses.isEmpty)
      fail(c, s"${sym.name} does not have any sub-classes. This may happen due to a limitation of scalac (SI-7046).")

    def findSubClasses(p: ClassSymbol): Set[ClassSymbol] = {
      p.knownDirectSubclasses.flatMap { sub =>
        val subClass = sub.asClass
        if (subClass.isTrait)
          findSubClasses(subClass)
        else f match {
          case DirectOnly => Set(subClass)
          case Everything => Set(subClass) ++ findSubClasses(subClass)
          case LeavesOnly =>
            val s = findSubClasses(subClass)
            if (s.isEmpty)
              Set(subClass)
            else
              s
        }
      }
    }

    findSubClasses(sym)
  }

  def findConcreteTypesNE(c: Context)(tpe: c.universe.Type, f: FindSubClasses): Set[c.universe.ClassSymbol] = {
    val r = findConcreteTypes(c)(tpe, f)
    if (r.isEmpty)
      fail(c, s"Unable to find concrete types for ${tpe.typeSymbol.name}.")
    r
  }

  /**
   * findConcreteTypes will spit out type constructors. This will turn them into types.
   *
   * @param T The ADT base trait.
   * @param t The subclass.
   */
  def determineAdtType(c: Context)(T: c.universe.Type, t: c.universe.ClassSymbol): c.universe.Type = {
    val t2 =
      if (t.typeParams.isEmpty)
        t.toType
      else
        caseClassTypeCtorToType(c)(T, t)
    require(t2 <:< T, s"$t2 is not a subtype of $T")
    t2
  }

  /**
   * Turns a case class type constructor into a type.
   *
   * Eg. caseClassTypeCtorToType(c)(Option[Int], Some[_]) → Some[Int]
   *
   * Actually this doesn't work with type variance :(
   */
  private def caseClassTypeCtorToType(c: Context)(baseTrait: c.universe.Type, caseclass: c.universe.ClassSymbol): c.universe.Type = {
    import c.universe._

    val companion = caseclass.companion
    val apply = companion.typeSignature.member(TermName("apply"))
    if (apply == NoSymbol)
      fail(c, s"Don't know how to turn $caseclass into a real type of $baseTrait; it's generic and its companion has no `apply` method.")

    val matchArgs = apply.asMethod.paramLists.flatten.map { arg => pq"_" }
    val name = TermName(c.freshName("x"))
    c.typecheck(q"""(??? : $baseTrait) match {case $name@$companion(..$matchArgs) => $name }""").tpe
  }

  def modStringHead(s: String, f: Char => Char): String =
    if (s.isEmpty)
      ""
    else {
      val h = f(s.head).toString
      if (s.length == 1)
        h
      else
        h + s.tail
    }

  def lowerCaseHead(s: String): String =
    modStringHead(s, _.toLower)

  /**
   * Create code for a function that will call .apply() on a given type's type companion object.
   */
  def tcApplyFn(c: Context)(t: c.universe.Type): c.universe.Select = {
    import c.universe._
    val sym = t.typeSymbol
    val tc  = sym.companion
    val pre = t match {
      case TypeRef(p, _, _) => p
      case x                => fail(c, s"Don't know how to extract `pre` from ${showRaw(x)}")
    }

    pre match {
      // Path dependent, eg. `t.Literal`
      case SingleType(NoPrefix, path) =>
        Select(Ident(path), tc.asTerm.name)

      // Assume type companion .apply exists
      case _ =>
        Select(Ident(tc), TermName("apply"))
    }
  }

  def selectFQN(c: Context)(s: String): c.universe.RefTree = {
    import c.universe._
    val terms = s.split('.').map(TermName(_): Name)
    val l = terms.length - 1
    terms(l) = terms(l).toTypeName
    val h = Ident(terms.head): RefTree
    if (l == 0)
      h
    else
      terms.tail.foldLeft(h)(Select(_, _))
  }
}
