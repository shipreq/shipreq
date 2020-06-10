package shipreq.webapp.base.text

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.SampleProject6._
import shipreq.webapp.base.test.UnsafeTypes._
import sourcecode.Line
import utest._

object PlainTextTest extends TestSuite {
  import Values._

  def ctxUc1 = ProjectText.Context.Req(uc1 )
  def ctxUc0 = ProjectText.Context.Req(0.UC)

  private def assertRoundTrip(input: String)(implicit l: Line) = {
    val rich = Text.CustomTextField.parse(project, None)(input)
    val actual = plainText.text(rich, Live, Optional)
    assertMultiline(actual, input)
  }

  override def tests = Tests {
    "useCaseStepRefs" - {
      val full = s"[UC-$step16_label] and [UC-$step17_label] are dead. [UC-$step19_label] and [UC-$step18_label] are not."

      "noCtx" - assertEq(plainText                .reqTitleById(uc1), full)
      "uc1"   - assertEq(plainText.withCtx(ctxUc1).reqTitleById(uc1), full.replaceAll("UC-", ""))
      "ucN"   - assertEq(plainText.withCtx(ctxUc0).reqTitleById(uc1), full.replaceAll("UC-", ""))
    }

    "multilineText" - {

      "combo" - {
        val input =
          """
            |hehe
            |
            |ok
            |* a
            |* b
            |
            |yes
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "ulWithNLs1" - {
        val input =
          """
            |ok
            |
            |* a
            |
            |* b1
            |
            |  b2
            |
            |* c
            |
            |* d
            |
            |* e1
            |
            |  e2
            |
            |* f1
            |
            |  f2
            |
            |  f3
            |
            |good
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "ulWithNLs2" - {
        val input =
          """
            |* a
            |
            |* b1
            |
            |  b2
            |
            |* c1
            |
            |  c2
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "ul" - {
        val input =
          """
            |* a
            |* b
            |* c
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "codeBlock" - {
        val input =
          """
            |```
            |ok
            |
            |
            |  1
            |
            |
            |cool
            |```
            |
            |hello
            |
            |```md
            |* here we go again!
            |```
            |
            |```
            | and again!
            |```
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "ulWithCodeBlocks" - {
        val input =
          """
            |* ```xxx
            |  ok
            |  ```
            |
            |* cool
            |
            |  ```
            |  * good job, me
            |  ```
            |
            |noice
            |""".stripMargin.trim
        assertRoundTrip(input)
      }
    }
  }
}
