package shipreq.base.util

import scalaz.Equal
import scala.reflect.macros.blackbox
import shipreq.base.macros.MacroUtils
import UtilMacros.AdtIso

object UtilMacros {

  type AdtIso[Adt, T] = (Adt => T, T => Adt, NonEmptySet[Adt], NonEmptySet[T])

  def  adtIso[Adt, T](f: Adt => T): AdtIso[Adt, T] = macro UtilMacroImpls.quietAdtIso[Adt, T]
  def _adtIso[Adt, T](f: Adt => T): AdtIso[Adt, T] = macro UtilMacroImpls.debugAdtIso[Adt, T]

  def  adtValues[T]: NonEmptyVector[T] = macro UtilMacroImpls.quietAdtValues[T]
  def _adtValues[T]: NonEmptyVector[T] = macro UtilMacroImpls.debugAdtValues[T]

  def  valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.quietValuesForAdt[T, V]
  def _valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro UtilMacroImpls.debugValuesForAdt[T, V]

  def  deriveEqual[A]: Equal[A] = macro UtilMacroImpls.quietDeriveEqual[A]
  def _deriveEqual[A]: Equal[A] = macro UtilMacroImpls.debugDeriveEqual[A]
}

class UtilMacroImpls(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  def quietAdtIso[Adt: c.WeakTypeTag, T: c.WeakTypeTag](f: c.Expr[Adt => T]): c.Expr[AdtIso[Adt, T]] = implAdtIso(false)(f)
  def debugAdtIso[Adt: c.WeakTypeTag, T: c.WeakTypeTag](f: c.Expr[Adt => T]): c.Expr[AdtIso[Adt, T]] = implAdtIso(true)(f)
  def implAdtIso[Adt: c.WeakTypeTag, T: c.WeakTypeTag](debug: Boolean)(f: c.Expr[Adt => T]): c.Expr[AdtIso[Adt, T]] = {
    val Adt       = weakTypeOf[Adt]
    val T         = weakTypeOf[T]
    val fromFn    = readMacroArg_tToTree(f).toStream
    val adtTypes  = findConcreteTypesNE(Adt, LeavesOnly)
    var toCases   = Vector.empty[CaseDef]
    var toValues  = Set.empty[Any]
    var adtValues = Vector.empty[Tree]

    for (adtClass <- adtTypes) {
      val adt = determineAdtType(Adt, adtClass)

      ensureConcrete(adt)
      if (primaryConstructorParams(adt).nonEmpty)
        fail(s"$adt requires constructor params.")

      val matchingCases = fromFn.filter(adt <:< _._1.fold(_.tpe, identity))
      if (matchingCases.size != 1)
        fail(s"Found ${matchingCases.size} cases for ${adt}.")

      val fromCase = matchingCases.head
      val toValue = fromCase._2 match {
        case Literal(Constant(v)) => v
        case x => fail(s"Expected a constant literal, got: ${showRaw(x)} ")
      }
      if (toValues contains toValue)
        fail(s"Non-unique value encountered: $toValue")
      toValues += toValue

      val adtObj = toSelectFQN(adtClass)
      adtValues :+= adtObj
      toCases :+= cq"${fromCase._2} => $adtObj"
    }

    val impl = q"""
      import shipreq.base.util.NonEmptySet
      import shipreq.base.util.UnivEq.Implicits._
      val from: $Adt => $T = $f
      val to: $T => $Adt = {case ..$toCases}
      val adts = NonEmptySet[$Adt](..$adtValues)
      val tos = NonEmptySet[$T](..${fromFn.map(_._2)})
      assert(adts.size == tos.size)
      assert(adts.forall(a => to(from(a)) == a))
      (from,to,adts,tos)
    """

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[AdtIso[Adt, T]](impl)
  }

  def quietAdtValues[T: c.WeakTypeTag]: c.Expr[NonEmptyVector[T]] = implAdtValues(false)
  def debugAdtValues[T: c.WeakTypeTag]: c.Expr[NonEmptyVector[T]] = implAdtValues(true)
  def implAdtValues[T: c.WeakTypeTag](debug: Boolean): c.Expr[NonEmptyVector[T]] = {
    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(T, LeavesOnly)

    val values = types.iterator.map { cs =>
      if (cs.isModuleClass)
        toSelectFQN(cs)
      else
        fail(s"Case object expected. Found: $cs")
    }.toList

    val impl = q"_root_.shipreq.base.util.NonEmptyVector.varargs[$T](..$values)"

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[NonEmptyVector[T]](impl)
  }

  def quietValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = implValuesForAdt(false)(f)
  def debugValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = implValuesForAdt(true)(f)
  def implValuesForAdt[T: c.WeakTypeTag, V: c.WeakTypeTag](debug: Boolean)(f: c.Expr[T => V]): c.Expr[NonEmptyVector[V]] = {
    val T       = weakTypeOf[T]
    val V       = weakTypeOf[V]
    val valueFn = readMacroArg_tToTree(f)
    val values  = valueFn.map(_._2)
    val types   = findConcreteAdtTypesNE(T, LeavesOnly).toVector
    val unseen  = types.filterNot(t => valueFn.exists(t <:< _._1.fold(_.tpe, identity)))

    if (unseen.nonEmpty)
      warn(s"The following types are unaccounted for: ${unseen.mkString(", ")}")

    val impl = q"_root_.shipreq.base.util.NonEmptyVector.varargs[$V](..$values)"

    if (debug) println("\n" + showCode(impl) + "\n")
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

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Equal[T]](impl)
  }
}