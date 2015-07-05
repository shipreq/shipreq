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
      fail(c, s"${sym.name} is abstract which is not allowed.")
    if (sym.isTrait)
      fail(c, s"${sym.name} is a trait which is not allowed.")
    if (sym.isSynthetic)
      fail(c, s"${sym.name} is synthetic which is not allowed.")
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

//    findSubClasses(sym).toSeq.sortBy(_.name.toString).map { s =>
//      if (s.typeParams.isEmpty) {
//        q"""addConcreteType[$s]"""
//      } else {
//        val t = unifyCaseClassWithTrait(c)(tpe, s)
//        q"""addConcreteType[$t]"""
//      }
//    }

    findSubClasses(sym)
  }

  def findConcreteTypesNE(c: Context)(tpe: c.universe.Type, f: FindSubClasses): Set[c.universe.ClassSymbol] = {
    val r = findConcreteTypes(c)(tpe, f)
    if (r.isEmpty)
      fail(c, s"Unable to find concrete types for ${tpe.typeSymbol.name}.")
    r
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
}
