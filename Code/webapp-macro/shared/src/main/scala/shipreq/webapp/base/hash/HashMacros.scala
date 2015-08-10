package shipreq.webapp.base.hash

import scala.reflect.macros.blackbox.Context
import shipreq.base.macros.MacroUtils

trait HashMacros {
  def joinHashes(hashes: List[Int]): Int

  final def hashCaseClass [T]: Hash[T] = macro HashMacroImpls.quietCaseClass[T]
  final def _hashCaseClass[T]: Hash[T] = macro HashMacroImpls.debugCaseClass[T]

  final def hashConstClass [T](key: String): Hash[T] = macro HashMacroImpls.quietConstClass[T]
  final def _hashConstClass[T](key: String): Hash[T] = macro HashMacroImpls.debugConstClass[T]

  final def hashADT [T]: Hash[T] = macro HashMacroImpls.quietADT[T]
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
  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implCaseClass[T](false)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[Hash[T]] = implCaseClass[T](true)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean): c.Expr[Hash[T]] = {
    import c.universe._

    val T      = concreteWeakTypeOf[T]
    val params = primaryConstructorParams(T)
    val Hash   = this.Hash

    val impl =
      params match {
        case Nil =>
          fail("Class constructor has no parameters.")

        case param :: Nil =>
          val (pName, pType) = nameAndType(param)
          q"$Hash[$pType].cmap[$T](_.$pName)"

        case _ =>
          val nil = Ident(c.mirror staticModule "scala.collection.immutable.Nil")

          val hashes = params.foldLeft(nil: c.universe.Tree) { (q, p) =>
            val (pName, pType) = nameAndType(p)
            q"$q.::($Hash[$pType].hash(t.$pName))"
          }

          q"$Hash.fn[$T](t => joinHashes($hashes))"
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[Hash[T]](impl)
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
    import c.universe._

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

    if (debug) println("\n" + impl + "\n")
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
    import c.universe._

    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(T, DirectOnly)
    val a     = TermName("a")
    val cases = types.map(t => cq"$a : $t => Hash[$t].hash($a)")
    val impl  = q"Hash.fn[$T]{ case ..$cases }"

    if (debug) println("\n" + impl + "\n")
    c.Expr[Hash[T]](impl)
  }
}
