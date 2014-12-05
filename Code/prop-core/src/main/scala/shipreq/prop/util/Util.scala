package shipreq.prop.util

import scala.annotation.tailrec
import scalaz.Memo

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

  def asciiTree[N](root: List[N], leaves: N => List[N], show: N => String, indent: String): String =
    quickSB(asciiTreeSB(_, root, leaves, show, indent))

  def asciiTreeSB[N](sb: StringBuilder, root: List[N], leaves: N => List[N], show: N => String, indent: String): Unit = {
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
