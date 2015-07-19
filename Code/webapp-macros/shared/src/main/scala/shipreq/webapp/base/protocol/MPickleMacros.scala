package shipreq.webapp.base.protocol

import scala.reflect.macros.blackbox.Context
import shipreq.webapp.macros.MacroUtils._
import upickle._

object MPickleMacros {

  def caseClass [T]: ReadWriter[T] = macro quietCaseClass[T]
  def _caseClass[T]: ReadWriter[T] = macro debugCaseClass[T]

  def quietCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[ReadWriter[T]] = implCaseClass[T](c, false)
  def debugCaseClass[T: c.WeakTypeTag](c: Context): c.Expr[ReadWriter[T]] = implCaseClass[T](c, true)

  // TODO Store implicits in vals first
  def implCaseClass[T: c.WeakTypeTag](c: Context, debug: Boolean): c.Expr[ReadWriter[T]] = {
    import c.universe._

    val T      = concreteWeakTypeOf[T](c)
    val params = primaryConstructorParams(c)

    val TC         = T.typeSymbol.companion
    val ReadWriter = Ident(c.mirror staticModule "upickle.ReadWriter")
    val JsArr      = Ident(c.mirror staticModule "upickle.Js.Arr")
    val Fns        = Ident(c.mirror staticModule "upickle.Fns")
    val writeJs    = q"$Fns.writeJs"
    val readJs     = q"$Fns.readJs"

    def invokeWriteJs(param: Symbol) = {
      val a = nameAndType(c)(param)._1
      q"$writeJs(t.$a)"
    }
    def invokeReadJs(vname: TermName, param: Symbol) = {
      val A = nameAndType(c)(param)._2
      q"$readJs[$A]($vname)"
    }

    val impl =
      params match {
        case Nil =>
          fail(c, "Class constructor has no parameters.")

        case param :: Nil =>
          val j = TermName("j")
          q"$ReadWriter[$T](t => ${invokeWriteJs(param)}, {case $j => $TC(${invokeReadJs(j, param)})})"

        case _ =>
          val writes = params map (invokeWriteJs(_))
          var nextChar = 'a'.toInt
          val tmp = params map { p =>
            val v = TermName(nextChar.toChar.toString)
            nextChar += 1
            (pq"$v", invokeReadJs(v, p))
          }
          val (vals, reads) = tmp.unzip
          val readCase = cq"$JsArr(..$vals) => $TC(..$reads)"
          q"$ReadWriter[$T](t => $JsArr(..$writes), {case $readCase})"
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[ReadWriter[T]](impl)
  }
}
