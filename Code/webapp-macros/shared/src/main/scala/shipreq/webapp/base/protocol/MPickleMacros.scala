package shipreq.webapp.base.protocol

import scala.reflect.macros.blackbox.Context
import shipreq.webapp.macros.MacroUtils
import upickle._

object MPickleMacros {
  def  caseClass[T]: ReadWriter[T] = macro MPickleMacroImpls.quietCaseClass[T]
  def _caseClass[T]: ReadWriter[T] = macro MPickleMacroImpls.debugCaseClass[T]
}

// =====================================================================================================================

trait MPickleMacroUtils { self: MacroUtils =>
  import c.universe._

  def importMPickle =
    q"import _root_.upickle._"

  def summonR(i: Init, tot: TypeOrTree) = tot match {
    case GotType(t) => i valImp appliedType(typeOf[Reader[_]], t)
    case GotTree(t) => i valImp tq"_root_.upickle.Reader[$t]"
  }

  def summonW(i: Init, tot: TypeOrTree) = tot match {
    case GotType(t) => i valImp appliedType(typeOf[Writer[_]], t)
    case GotTree(t) => i valImp tq"_root_.upickle.Writer[$t]"
  }

  def summonRW(i: Init, t: TypeOrTree) =
    (summonR(i, t), summonW(i, t))

  def newReadWriter(i: Init, t: Type)(write: Tree, read: Tree): Tree =
    i.wrap(q"ReadWriter[$t]($write, $read)")

  def writeToJsonStr(writer: TermName, value: Tree): Tree =
    q"_root_.upickle.json.write($writer.write($value))"

  def readFromJsonStr(reader: TermName, value: Tree): Tree =
    q"$reader.read(_root_.upickle.json read $value)"
}

class MPickleMacroImpls(val c: Context) extends MacroUtils with MPickleMacroUtils {
  import c.universe._

  def quietCaseClass[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass[T](false)
  def debugCaseClass[T: c.WeakTypeTag]: c.Expr[ReadWriter[T]] = implCaseClass[T](true)
  def implCaseClass[T: c.WeakTypeTag](debug: Boolean): c.Expr[ReadWriter[T]] = {
    val T      = concreteWeakTypeOf[T]
    val TC     = T.typeSymbol.companion
    val params = primaryConstructorParams(T)
    val init   = Init(importMPickle)

    def invokeWriteJs(subj: TermName, param: Symbol) = {
      val (n, t) = nameAndType(param)
      val w = summonW(init, t)
      q"$w.write($subj.$n)"
    }
    def invokeReadJs(subj: TermName, param: Symbol) = {
      val t = nameAndType(param)._2
      val r = summonR(init, t)
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
          init.wrap(q"ReadWriter[$T](j => $w, {case j => $TC($r)} )")

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
          init.wrap(q"ReadWriter[$T](j => Js.Arr(..$writes), {case $rCases} )")
      }

    if (debug) println("\n" + impl + "\n")
    c.Expr[ReadWriter[T]](impl)
  }
}
