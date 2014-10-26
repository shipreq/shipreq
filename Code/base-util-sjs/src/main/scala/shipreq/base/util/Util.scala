package shipreq.base.util

import java.net.URL
import scala.annotation.tailrec
import scala.util.Try
import scalaz.Memo
import ScalaExt.StringBuilderExt

object Util {

  @inline def quickSB(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  @inline def quickSB(start: String, f: StringBuilder => Unit): String = {
    val sb = new StringBuilder(start)
    f(sb)
    sb.toString
  }

  @inline def quickToString(clz: Class[_])(fs: (StringBuilder => Any)*): String =
    quickSB(clz.getSimpleName, _.mkStringF("(", ", ", ")")(fs: _*))


  private[this] val simpleClassNameRegex = "^.+[\\.\\$]".r

  // https://issues.scala-lang.org/browse/SI-2034
  def simpleName(c: Class[_]): String =
    simpleClassNameRegex.replaceFirstIn(c.getName, "")

  val simpleNameMemo =
    Memo.immutableHashMapMemo[Class[_], String](simpleName _)

  def existentLocalResources(paths: List[String]): Stream[URL] =
    paths.toStream.map(f => Try(getClass.getResource(f)).getOrElse(null)).filter(_ ne null)

  def cutoffStr(s: String, cutoff: Int): String =
    if (s.length <= cutoff)
      s
    else
      s.substring(0, cutoff - 1) + "\u2026"

  def quoteString(s: String): String =
    "\"" + escapeString(s) + "\""

  def escapeString(s: String): String =
    s.toCharArray.map {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\\' => "\\"
      case '"' => "\\\""
      case n if n >= 32 && n <= 127 => n.toString
      //case n if n < 256 => "\\x%02x" format n.toInt
      case n => "\\u%04x" format n.toLong
    }.mkString

  def asciiTree[N](root: List[N], show: N => String, leaves: N => List[N], indent: String): String =
    quickSB(asciiTreeSB(_, root, show, leaves, indent))

  def asciiTreeSB[N](sb: StringBuilder, root: List[N], show: N => String, leaves: N => List[N], indent: String): Unit = {
    val pm = "│  "
    val pl = "   "
    val cm = "├─ "
    val cl = "└─ "
    var first = true
    val im = Memo.mutableHashMapMemo[Int, String](i => "\n" + (" " * i))
    @inline def loop2 = loop(_, _, _)
    @tailrec
    def loop(parentLvlLast: Vector[Boolean], fs: List[N], root: Boolean): Unit = fs match {
      case Nil =>
      case h :: t =>
        if (first) first = false else sb append '\n'
        var indentlen = sb.length
        sb append indent
        for (b <- parentLvlLast) sb.append(if (b) pl else pm)
        val last = t.isEmpty
        if (!root) sb.append(if (last) cl else cm)
        indentlen = sb.length - indentlen
        sb append show(h).replaceAll("\n(?=[^\n])", im(indentlen))
        val nextLvl = if (root) Vector.empty[Boolean] else parentLvlLast :+ last
        loop2(nextLvl, leaves(h), false)
        loop(parentLvlLast, t, root)
    }
    loop(Vector.empty, root, true)
  }
}