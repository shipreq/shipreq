package shipreq.webapp.client.ww

import utest._
import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line

object WrapTextTest extends TestSuite {

  override def tests = Tests {

    "examples" - {

      def test(input: String, iw: Double)(exp: String)(implicit l: Line): Unit = {
        val a = WrapText(input, iw)
        if (a != exp) {
          def f(s: String): String = s.split('\n').map(l => "(%5.1f) %s".format(CharWidths.string(l), l)).mkString("\n")
          val a2 = f(a)
          val e2 = f(exp)
          assertMultiline(a2, e2)
        }
      }

      "single" - test("abcdef", 1)("abcdef")

      "short" - test("I'm eating a big apple", 65)("I'm eating\na big apple")

      "long" - test("I'm eating a big apple and not eating a banana right now ok then great thanks", 65)(
        """I'm eating
          |a big apple
          |and not
          |eating a
          |banana
          |right now
          |ok then
          |great
          |thanks
          |""".stripMargin.trim
      )

      "multiline" - test("X X!\na b c d e\n\nI'm eating a big apple and not eating a banana right now ok then great thanks", 65)(
        """X X!
          |a b c d e
          |
          |I'm eating
          |a big apple
          |and not
          |eating a
          |banana
          |right now
          |ok then
          |great
          |thanks
          |""".stripMargin.trim
      )
    }
  }
}
