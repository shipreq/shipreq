package shipreq.base.util

import scala.reflect.macros.blackbox
import shipreq.base.macros.MacroUtils

object UtilMacros {

  def  valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.quietValuesForAdt[T, V]
  def _valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.debugValuesForAdt[T, V]

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
}
