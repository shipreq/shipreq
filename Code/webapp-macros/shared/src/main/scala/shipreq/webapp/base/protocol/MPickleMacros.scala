package shipreq.webapp.base.protocol

import scala.reflect.macros.blackbox.Context
import shipreq.webapp.macros.MacroUtils
import upickle._

object MPickleMacros {
  def  caseClass[T]: ReadWriter[T] = macro MPickleMacroImpls.quietCaseClass[T]
  def _caseClass[T]: ReadWriter[T] = macro MPickleMacroImpls.debugCaseClass[T]
}

// =====================================================================================================================
class MPickleMacroImpls(val c: Context) extends MacroUtils {
  import c.universe._

  private class Helper {

    @volatile var init = Vector.empty[Tree]

    init :+= q"import _root_.upickle._"

    def prepVal(n: String, i: TermName => Tree): TermName = {
      val v = TermName(c.freshName(n))
      init :+= i(v)
      v
    }

    def prepReader(t: Type): TermName =
      prepVal("r", v => q"val $v = implicitly[Reader[$t]]")

    def prepWriter(t: Type): TermName =
      prepVal("w", v => q"val $v = implicitly[Writer[$t]]")

    def impl(t: Type, writeImpl: Tree, readImpl: Tree) =
      wrap(q"ReadWriter[$t]($writeImpl, $readImpl)")

    def wrap(body: Tree) =
      q"..$init; $body"
  }

  private def Helper = new Helper

  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass[T](false)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass[T](true)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean): c.Expr[ReadWriter[T]] = {
    val T      = concreteWeakTypeOf[T]
    val TC     = T.typeSymbol.companion
    val params = primaryConstructorParams(T)
    val helper = Helper

    def invokeWriteJs(subj: TermName, param: Symbol) = {
      val (n, t) = nameAndType(param)
      val w = helper.prepWriter(t)
      q"$w.write($subj.$n)"
    }
    def invokeReadJs(subj: TermName, param: Symbol) = {
      val t = nameAndType(param)._2
      val r = helper.prepReader(t)
      q"$r.read($subj)"
    }

    val impl =
      params match {
        case Nil =>
          fail("Class constructor has no parameters.")

        case param :: Nil =>
          val j = TermName("j")
          val w = invokeWriteJs(j, param)
          val r = invokeReadJs(j, param)
          helper.wrap(q"ReadWriter[$T](j => $w, {case j => $TC($r)} )")

        case _ =>
          val j = TermName("j")
          var writes = Vector.empty[Tree]
          var nextChar = 'a'.toInt
          val tmp = params map { p =>
            val v = TermName(nextChar.toChar.toString)
            nextChar += 1
            writes :+= invokeWriteJs(j, p)
            (pq"$v", invokeReadJs(v, p))
          }
          val (vals, reads) = tmp.unzip
          val rCases = cq"Js.Arr(..$vals) => $TC(..$reads)"
          helper.wrap(q"ReadWriter[$T](j => Js.Arr(..$writes), {case $rCases} )")
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[ReadWriter[T]](impl)
  }
}
