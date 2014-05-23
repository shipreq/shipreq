package shipreq.webapp.util

import java.util.regex.Pattern
import scala.util.matching.Regex.quoteReplacement
import shipreq.base.util.ScalaExt._

object JsGen {

  private val dangerous = "\\[]^-&.$?*+,{}()|:".toCharArray.toList
  private val literalQuotes = """(?d)\\Q(.+?)\\E""".r

  def translateRegex(p: Pattern): String = {

    def quote(s: String): String =
      s.flatMapSB((c, sb) => c match {
        case _ if dangerous.contains(c) => sb append "\\"+c.toString // "\\x%02x".format(c.toInt)
        case _ => sb append c
      })

    def escape(c: Char, sb: StringBuilder): Unit = {
      val i = c.toInt
      c match {
        case '/'          => sb append "\\/"
        case '\n'         => sb append "\\n"
        case '\r'         => sb append "\\r"
        case '\t'         => sb append "\\t"
        case _ if i < 32  => sb append "\\x%02x".format(i)
        case _ if i > 255 => sb append "\\u%04x".format(i)
        case _            => sb append c
      }
    }

    val r = literalQuotes.replaceAllIn(p.toString, m => quoteReplacement(quote(m group 1))).flatMapSB(escape)
    s"/$r/"
  }


}
