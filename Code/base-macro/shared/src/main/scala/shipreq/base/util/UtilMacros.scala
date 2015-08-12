package shipreq.base.util

import scalaz.Equal
import scala.reflect.macros.blackbox
import shipreq.base.macros.MacroUtils

object UtilMacros {

  def  valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.quietValuesForAdt[T, V]
  def _valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.debugValuesForAdt[T, V]

  def  deriveEqual[A]: Equal[A] = macro UtilMacroImpls.quietDeriveEqual[A]
  def _deriveEqual[A]: Equal[A] = macro UtilMacroImpls.debugDeriveEqual[A]
}

class UtilMacroImpls(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  def quietValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = implValuesForAdt(false)(f)
  def debugValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = implValuesForAdt(true)(f)
  def implValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](debug: Boolean)(f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = {
    val T       = weakTypeOf[T]
    val V       = weakTypeOf[V]
    val valueFn = readMacroArg_tToTree(f)
    val values  = valueFn.map(_._2)
    val types   = findConcreteTypesNE(T, LeavesOnly).toVector.map(t => determineAdtType(T, t))
    val unseen  = types.filterNot(t => valueFn.exists(t <:< _._1.fold(_.tpe, identity)))

    if (unseen.nonEmpty)
      warn(s"The following types are unaccounted for: ${unseen.mkString(", ")}")

    val impl = q"_root_.shipreq.base.util.NonEmptyVector.varargs[$V](..$values)"

    if (debug) println("\n" + impl + "\n")
    c.Expr[NonEmptyVector[V]](impl)
  }

  private val equal = c.typeOf[Equal[_]]

  def quietDeriveEqual[T: c.WeakTypeTag]: c.Expr[Equal[T]] = implDeriveEqual(false)
  def debugDeriveEqual[T: c.WeakTypeTag]: c.Expr[Equal[T]] = implDeriveEqual(true )
  def implDeriveEqual[T: c.WeakTypeTag](debug: Boolean): c.Expr[Equal[T]] = {
    if (debug) println()
    val T = weakTypeOf[T]
    val t = T.typeSymbol

    def caseClass0: Tree =
      q"_root_.scalaz.Equal.equal[$T]((_, _) => true)"

    def caseClass1up(params: List[Symbol]): Tree = {
      val init = Init()
      var cmps = Vector.empty[Tree]
      for (p <- params) {
        val (pn, pt) = nameAndType(T, p)
        val e = init.valImp(appliedType(equal, pt))
        cmps :+= q"$e.equal(a.$pn,b.$pn)"
      }
      val expr = cmps.reduce((a, b) => q"$a && $b")
      q"""
        ..$init
        _root_.scalaz.Equal.equal[$T]((a, b) => $expr)
      """
    }

    def adt: Tree = {
      val init = Init()
      val cases = crawlADT[CaseDef](T, p => {
        val pt = determineAdtType(T, p)
        tryInferImplicit(appliedType(equal, pt)).map { et =>
          val e = init.valDef(et)
          cq"x: $pt => b match {case y: $pt => $e.equal(x,y); case _ => false}"
        }
      }, p => {
        val pt = p.asType.toType
        val u = appliedType(equal, pt)
        fail(s"Implicit not found: $u")
      })
      init wrap q"_root_.scalaz.Equal.equal[$T]((a,b) => a match {case ..$cases})"
    }

    val impl =
      if (t.isClass && t.asClass.isCaseClass) {
        ensureConcrete(T)
        val params = primaryConstructorParams(T)
        if (params.isEmpty)
          caseClass0
        else
          caseClass1up(params)
      } else
        adt

    if (debug) println("\n" + impl + "\n")
    c.Expr[Equal[T]](impl)
  }
}