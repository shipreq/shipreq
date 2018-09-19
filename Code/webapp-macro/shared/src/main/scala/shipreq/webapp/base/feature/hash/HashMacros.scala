package shipreq.webapp.base.feature.hash

import japgolly.microlibs.macro_utils.MacroUtils
import japgolly.microlibs.utils.Utils
import scala.reflect.macros.blackbox.Context
import shipreq.base.util.SetDiff

trait HashMacros {
  def joinHashes(hashes: List[Int]): Int

  final def  hashCaseClass[T]: HashFn[T] = macro HashMacroImpls.quietCaseClass[T]
  final def _hashCaseClass[T]: HashFn[T] = macro HashMacroImpls.debugCaseClass[T]

  final def  hashCaseClassSubset[T](include: (Symbol, Boolean)*): HashFn[T] = macro HashMacroImpls.quietCaseClassSubset[T]
  final def _hashCaseClassSubset[T](include: (Symbol, Boolean)*): HashFn[T] = macro HashMacroImpls.debugCaseClassSubset[T]

  final def  hashConstClass[T](key: String): HashFn[T] = macro HashMacroImpls.quietConstClass[T]
  final def _hashConstClass[T](key: String): HashFn[T] = macro HashMacroImpls.debugConstClass[T]

  final def  hashADT[T]: HashFn[T] = macro HashMacroImpls.quietADT[T]
  final def _hashADT[T]: HashFn[T] = macro HashMacroImpls.debugADT[T]
}

class HashMacroImpls(val c: Context) extends MacroUtils {
  import c.universe._

  private def HashFn =
    c.universe.Ident(c.mirror staticModule "shipreq.webapp.base.feature.hash.HashFn")

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have more than 0 params.
   */
  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[HashFn[T]] = implCaseClass[T](false, identity)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[HashFn[T]] = implCaseClass[T](true , identity)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean, preprocessParams: List[NameAndType] => List[NameAndType]): c.Expr[HashFn[T]] = {
    val T      = concreteWeakTypeOf[T]
    val params = preprocessParams(primaryConstructorParams(T).map(nameAndType(T, _)))
    val HashFn = this.HashFn

    val impl =
      params match {
        case Nil =>
          fail("Class constructor has no parameters.")

        case (pName, pType) :: Nil =>
          val h = needInferImplicit(HashType(pType))
          q"$h.contramap[$T](_.$pName)"

        case _ =>
          val init           = new Init("i$" + _)
          val hashes = params.foldLeft(LitNil: c.universe.Tree) { case (q, (pName, pType)) =>
            val h = init.valImp(HashType(pType))
            q"$q.::($h hashFn t.$pName)"
          }
          q"..$init; $HashFn[$T](t => joinHashes($hashes))"
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[HashFn[T]](impl)
  }

  def quietCaseClassSubset[T: c.WeakTypeTag](include: c.Expr[(scala.Symbol, Boolean)]*): c.Expr[HashFn[T]] = implCaseClassSubset[T](false, include)
  def debugCaseClassSubset[T: c.WeakTypeTag](include: c.Expr[(scala.Symbol, Boolean)]*): c.Expr[HashFn[T]] = implCaseClassSubset[T](true , include)
  def implCaseClassSubset[T: c.WeakTypeTag](debug: Boolean, include: Seq[c.Expr[(scala.Symbol, Boolean)]]): c.Expr[HashFn[T]] = {
    val spec: Vector[(String, Boolean)] = include.map(readMacroArg_symbolBoolean)(collection.breakOut)
    if (debug) println(s"Subset spec: $spec")

    val map: Map[String, Boolean] = spec.toMap
    if (map.size != spec.length)
      fail("Duplicate field names found: " + Utils.dups(spec.iterator.map(_._1)).toSet.mkString(", "))

    implCaseClass[T](debug, ps => {
      val fieldDiff = SetDiff.compare(ps.map(_._1.decodedName.toString).toSet, map.keySet)
      if (fieldDiff.nonEmpty)
        fail(s"Mismatch between ${weakTypeOf[T]} fields and specified fields: $fieldDiff")
      ps.filter(p => map(p._1.decodedName.toString))
    })
  }

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have 0 params.
   */
  def quietConstClass[T: c.WeakTypeTag](key: c.Expr[String]): c.Expr[HashFn[T]] = implConstClass[T](false)(key)
  def debugConstClass[T: c.WeakTypeTag](key: c.Expr[String]): c.Expr[HashFn[T]] = implConstClass[T](true)(key)
  def implConstClass[T: c.WeakTypeTag](debug: Boolean)(key: c.Expr[String]): c.Expr[HashFn[T]] = {
    val T      = concreteWeakTypeOf[T]
    val params = primaryConstructorParams(T)
    val HashFn = this.HashFn

    val impl =
      params match {
        case Nil =>
          q"$HashFn.constByHashing[String,$T]($key)"

        case _ =>
          fail(s"Class constructor has ${params.length} parameters. Expected 0.")
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[HashFn[T]](impl)
  }

  /**
   * Constraints:
   * - Type must be sealed.
   * - Type must be abstract or a trait.
   */
  def quietADT[T: c.WeakTypeTag]: c.Expr[HashFn[T]] = implADT[T](false)
  def debugADT[T: c.WeakTypeTag]: c.Expr[HashFn[T]] = implADT[T](true)
  def implADT[T: c.WeakTypeTag](debug: Boolean): c.Expr[HashFn[T]] = {
    val T      = weakTypeOf[T]
    val types  = findConcreteAdtTypesNE(T, DirectOnly)
    val a      = TermName("a")
    val init   = new Init("i$" + _)
    val HashFn = this.HashFn
    val cases  = types.map { t =>
      val h = init.valImp(HashType(t))
      cq"$a : $t => $h.hashFn($a)"
    }
    val impl = q"..$init; $HashFn[$T]{ case ..$cases }"

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[HashFn[T]](impl)
  }

  def HashType(t: Type): Type =
    appliedType(c.typeOf[HashFn[_]], t)
}
