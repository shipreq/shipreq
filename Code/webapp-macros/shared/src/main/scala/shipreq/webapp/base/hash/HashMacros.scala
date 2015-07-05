package shipreq.webapp.base.hash

object HashMacros {
  import shipreq.webapp.macros.MacroUtils._
  import scala.reflect.macros.blackbox.Context

  private def Hash(c: Context) =
    c.universe.Ident(c.mirror staticModule "shipreq.webapp.base.hash.Hash")

  /**
   * Constraints:
   * - Type must be concrete (not abstract, synthetic, or a trait)
   * - Type must have a primary constructor.
   * - Primary constructor must have more than 0 params.
   */
  def implCaseClass[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[Hash[T]] = {
    import c.universe._

    val T      = concreteWeakTypeOf[T](c)
    val params = primaryConstructorParams(c)
    val Hash   = this.Hash(c)

    val impl =
      params match {
        case Nil =>
          fail(c, "Class constructor has no parameters.")

        case param :: Nil =>
          val (pName, pType) = nameAndType(c)(param)
          q"$Hash[$pType].cmap[$T](_.$pName)"

        case _ =>
          val nil = Ident(c.mirror staticModule "scala.collection.immutable.Nil")

          val hashes = params.foldLeft(nil: c.universe.Tree) { (q, p) =>
            val (pName, pType) = nameAndType(c)(p)
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
  def implConstClass[T: c.WeakTypeTag](c: Context, debug: Boolean)(key: c.Expr[String]): c.Expr[Hash[T]] = {
    import c.universe._

    val T      = concreteWeakTypeOf[T](c)
    val params = primaryConstructorParams(c)
    val Hash   = this.Hash(c)

    val impl =
      params match {
        case Nil =>
          q"$Hash.constOf[String,$T]($key)"

        case _ =>
          fail(c, s"Class constructor has ${params.length} parameters. Expected 0.")
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[Hash[T]](impl)
  }

  /**
   * Constraints:
   * - Type must be sealed.
   * - Type must be abstract or a trait.
   */
  def implADT[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[Hash[T]] = {
    import c.universe._

    val T     = weakTypeOf[T]
    val types = findConcreteTypesNE(c)(T, DirectOnly)
    val a     = TermName("a")
    val cases = types.map(t => cq"$a : $t => Hash[$t].hash($a)")
    val impl  = q"Hash.fn[$T]{ case ..$cases }"

    if (debug) println("\n" + impl + "\n")
    c.Expr[Hash[T]](impl)
  }

  def quietCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[Hash[T]] = implCaseClass[T](c, false)
  def debugCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[Hash[T]] = implCaseClass[T](c, true)

  def quietConstClass[T: c.WeakTypeTag](c: Context)(key: c.Expr[String]): c.Expr[Hash[T]] = implConstClass[T](c, false)(key)
  def debugConstClass[T: c.WeakTypeTag](c: Context)(key: c.Expr[String]): c.Expr[Hash[T]] = implConstClass[T](c, true)(key)

  def quietADT[T: c.WeakTypeTag](c: Context): c.Expr[Hash[T]] = implADT[T](c, false)
  def debugADT[T: c.WeakTypeTag](c: Context): c.Expr[Hash[T]] = implADT[T](c, true)

}

trait HashMacros {
  def joinHashes(hashes: List[Int]): Int

  final def hashCaseClass [T]: Hash[T] = macro HashMacros.quietCaseClass[T]
  final def _hashCaseClass[T]: Hash[T] = macro HashMacros.debugCaseClass[T]

  final def hashConstClass [T](key: String): Hash[T] = macro HashMacros.quietConstClass[T]
  final def _hashConstClass[T](key: String): Hash[T] = macro HashMacros.debugConstClass[T]

  final def hashADT [T]: Hash[T] = macro HashMacros.quietADT[T]
  final def _hashADT[T]: Hash[T] = macro HashMacros.debugADT[T]
}