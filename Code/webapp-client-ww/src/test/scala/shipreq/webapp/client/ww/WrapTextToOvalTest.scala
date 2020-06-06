package shipreq.webapp.client.ww

import utest._
import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line

object WrapTextToOvalTest extends TestSuite {

  private final val w = 14.0

  override def tests = Tests {

    "examples" - {

      def test(input: String, iw: Double)(exp: String)(implicit l: Line): Unit = {
        val a = WrapTextToOval(input, iw)
        if (a != exp) {
          def f(s: String): String = s.split('\n').map(l => "(%5.1f) %s".format(CharWidths.string(l), l)).mkString("\n")
          val a2 = f(a)
          val e2 = f(exp)
          assertMultiline(a2, e2)
        }
      }

      "single" - test("abcdef", w)("abcdef")
      "short" - test("I'm eating a big apple", w * 10)("I'm eating\na big apple")

    }
  }
}
