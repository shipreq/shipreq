package shipreq.base.util

import java.util.UUID

/** Takes a potentially slow `String* => String` function and makes it super fast by executing it once,
  * turning the result into a template, then using the template for all subsequent calls.
  *
  * This assumes provided functions are pure.
  */
object Template {

  def apply1(f: String => String): String => String = {
    val t = prepare(1, a => f(a(0)))
    s => t(Array(s))
  }

  def apply2(f: (String, String) => String): (String, String) => String = {
    val t = prepare(2, a => f(a(0), a(1)))
    (s1, s2) => t(Array(s1, s2))
  }

  def apply3(f: (String, String, String) => String): (String, String, String) => String = {
    val t = prepare(3, a => f(a(0), a(1), a(2)))
    (s1, s2, s3) => t(Array(s1, s2, s3))
  }

  def apply4(f: (String, String, String, String) => String): (String, String, String, String) => String = {
    val t = prepare(4, a => f(a(0), a(1), a(2), a(3)))
    (s1, s2, s3, s4) => t(Array(s1, s2, s3, s4))
  }

  def apply5(f: (String, String, String, String, String) => String): (String, String, String, String, String) => String = {
    val t = prepare(5, a => f(a(0), a(1), a(2), a(3), a(4)))
    (s1, s2, s3, s4, s5) => t(Array(s1, s2, s3, s4, s5))
  }

  def apply6(f: (String, String, String, String, String, String) => String): (String, String, String, String, String, String) => String = {
    val t = prepare(6, a => f(a(0), a(1), a(2), a(3), a(4), a(5)))
    (s1, s2, s3, s4, s5, s6) => t(Array(s1, s2, s3, s4, s5, s6))
  }

  private def prepare(arity: Int, f: Array[String] => String): Array[String] => String = {
    val ids = Array.fill(arity)(newId())

    val regex = {
      val i = ids.mkString("|")
      val r = s"(?=$i)|(?<=$i)"
      r.r
    }

    def makeFragFn(frag: String): (Array[String], StringBuilder) => Unit =
      ids.indexWhere(frag == _) match {
        case -1 => (_, sb) => sb.append(frag)
        case i  => (a, sb) => sb.append(a(i))
      }

    val fragFns =
      regex
        .split(f(ids))
        .iterator
        .filter(_.nonEmpty)
        .map(makeFragFn)
        .toArray

    fragFns.length match {
      case 0 =>
        _ => ""

      case 1 =>
        val ff = fragFns(0)
        a => {
          val sb = new StringBuilder
          ff(a, sb)
          sb.toString()
        }

      case len =>
        a => {
          val sb = new StringBuilder
          var i = 0
          while (i < len) {
            fragFns(i)(a, sb)
            i += 1
          }
          sb.toString()
        }
    }
  }

  private def newId(): String =
    "\u0001" + UUID.randomUUID().toString.replace("-", " ") + "\u0002"
}
