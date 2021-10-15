package shipreq.webapp.member.test

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactDOMServer
import japgolly.scalajs.react.test.ReactTestUtils
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.LinkingInfo
import sourcecode.Line
import utest.framework.TestPath

object RenderTestUtil {
  import WebappTestUtil._

  def formatHTML(html: String): String = {
    def addIndent(): String => String = {
      var indent = 0
      s => {
        val closer = s.startsWith("</")
        if (closer && indent > 0)
          indent -= 1
        val s2 = s.indentLines(indent << 1)
        if (!closer && s.startsWith("<"))
          indent += 1
        s2
      }
    }

    html
      .trim
      .replaceAll("\n +", "")
      .replace("<", "\n<")
      .replace(">", ">\n")
      .linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(addIndent())
      .mkString("\n")
  }

  private val removeClassNamesInProd: String => String =
    if (LinkingInfo.developmentMode)
      identity
    else {
      val r = " class=\"[^\"]+\"".r
      r.replaceAllIn(_, "")
    }

  def assertRender(actual: VdomNode, expect: String)(implicit l: Line, p: TestPath): Unit = {
    def norm(s: String): String =
      formatHTML(removeClassNamesInProd(s))

    val a = norm(ReactTestUtils.removeReactInternals(ReactDOMServer.renderToStaticMarkup(actual)))
    val e = norm(expect)

    if (a != e) {
      println(s"${Console.RED_B}${Console.WHITE}${p.value.mkString(".")}${Console.RESET}")
      println(s"$a\n")
    }

    assertMultiline(a, e)
  }

}