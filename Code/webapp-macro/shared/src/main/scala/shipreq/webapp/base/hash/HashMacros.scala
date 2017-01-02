package shipreq.webapp.base.hash

import japgolly.microlibs.macro_utils.MacroUtils
import scala.reflect.macros.blackbox.Context
import shipreq.base.util.Util

trait HashMacros {
  def joinHashes(hashes: List[Int]): Int

  final def  hashCaseClass[T]: Hash[T] = macro HashMacroImpls.quietCaseClass[T]
  final def _hashCaseClass[T]: Hash[T] = macro HashMacroImpls.debugCaseClass[T]

  final def  hashCaseClassExcept[T](fields: Symbol*): Hash[T] = macro HashMacroImpls.quietCaseClassExcept[T]
  final def _hashCaseClassExcept[T](fields: Symbol*): Hash[T] = macro HashMacroImpls.debugCaseClassExcept[T]

  final def  hashConstClass[T](key: String): Hash[T] = macro HashMacroImpls.quietConstClass[T]
  final def _hashConstClass[T](key: String): Hash[T] = macro HashMacroImpls.debugConstClass[T]

  final def  hashADT[T]: Hash[T] = macro HashMacroImpls.quietADT[T]
  final def _hashADT[T]: Hash[T] = macro HashMacroImpls.debugADT[T]
}

class HashMacroImpls(val c: Context) extends MacroUtils {
  import c.universe._

  private def Hash =
    c.universe.Ident(c.mirror staticModule "shipreq.webapp.base.hash.Hash")

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have more than 0 params.
   */
  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implCaseClass[T](false, identity)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implCaseClass[T](true , identity)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean, preprocessParams: List[NameAndType] => List[NameAndType]): c.Expr[Hash[T]] = {
    val T      = concreteWeakTypeOf[T]
    val params = preprocessParams(primaryConstructorParams(T).map(nameAndType(T, _)))
    val Hash   = this.Hash

    val impl =
      params match {
        case Nil =>
          fail("Class constructor has no parameters.")

        case (pName, pType) :: Nil =>
          val h = needInferImplicit(HashType(pType))
          q"$h.cmap[$T](_.$pName)"

        case _ =>
          val init = new Init
          val hashes = params.foldLeft(LitNil: c.universe.Tree) { case (q, (pName, pType)) =>
            val h = init.valImp(HashType(pType))
            q"$q.::($h hash t.$pName)"
          }
          q"..$init; $Hash.fn[$T](t => joinHashes($hashes))"
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Hash[T]](impl)
  }

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have more than 0 params.
   */
  def quietCaseClassExcept[T: c.WeakTypeTag](fields: c.Expr[scala.Symbol]*): c.Expr[Hash[T]] = implCaseClassExcept[T](false, fields)
  def debugCaseClassExcept[T: c.WeakTypeTag](fields: c.Expr[scala.Symbol]*): c.Expr[Hash[T]] = implCaseClassExcept[T](true , fields)
  def implCaseClassExcept[T: c.WeakTypeTag](debug: Boolean, fields: Seq[c.Expr[scala.Symbol]]): c.Expr[Hash[T]] = {
    if (fields.isEmpty)
      fail("At least one field name is required.")
    val fieldVector = fields.map(readMacroArg_symbol).toVector
    val fieldSet = fieldVector.toSet
    if (fieldSet.size != fieldVector.size) {
      val dups = fieldSet.foldLeft(fieldVector)((q, f) => Util.deleteVectorElement(q, q indexOf f))
      fail("Duplicate field names found: " + dups.mkString(", "))
    }

    implCaseClass[T](debug, ps => {
      val r = ps.filterNot(fieldSet contains _._1.decodedName.toString)
      if ((ps.size - r.size) != fieldSet.size) {
        val missing = fieldSet &~ ps.map(_._1.decodedName.toString).toSet
        fail(s"Field(s) not found: ${missing mkString ", "}")
      }
      r
    })
  }

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have 0 params.
   */
  def quietConstClass[T: c.WeakTypeTag](key: c.Expr[String]): c.Expr[Hash[T]] = implConstClass[T](false)(key)
  def debugConstClass[T: c.WeakTypeTag](key: c.Expr[String]): c.Expr[Hash[T]] = implConstClass[T](true)(key)
  def implConstClass[T: c.WeakTypeTag](debug: Boolean)(key: c.Expr[String]): c.Expr[Hash[T]] = {
    val T      = concreteWeakTypeOf[T]
    val params = primaryConstructorParams(T)
    val Hash   = this.Hash

    val impl =
      params match {
        case Nil =>
          q"$Hash.constOf[String,$T]($key)"

        case _ =>
          fail(s"Class constructor has ${params.length} parameters. Expected 0.")
      }

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Hash[T]](impl)
  }

  /**
   * Constraints:
   * - Type must be sealed.
   * - Type must be abstract or a trait.
   */
  def quietADT[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implADT[T](false)
  def debugADT[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implADT[T](true)
  def implADT[T: c.WeakTypeTag](debug: Boolean): c.Expr[Hash[T]] = {
    val T     = weakTypeOf[T]
    val types = findConcreteAdtTypesNE(T, DirectOnly)
    val a     = TermName("a")
    val init  = new Init
    val cases = types.map { t =>
      val h = init.valImp(HashType(t))
      cq"$a : $t => $h.hash($a)"
    }
    val impl = q"..$init; Hash.fn[$T]{ case ..$cases }"

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Hash[T]](impl)
  }

  def HashType(t: Type): Type =
    appliedType(c.typeOf[Hash[_]], t)
}
