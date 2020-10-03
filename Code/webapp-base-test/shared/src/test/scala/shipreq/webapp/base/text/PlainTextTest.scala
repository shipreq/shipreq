package shipreq.webapp.base.text

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.SampleProject6._
import sourcecode.Line
import utest._

object PlainTextTest extends TestSuite {
  import Values._

  def ctxUc1 = ProjectText.Context.Req(uc1)
  def ctxUc0 = ProjectText.Context.Req(UseCaseId(0))

  private def assertRoundTrip(input: String)(implicit l: Line) =
    assertCorrection(input, input)

  private def assertCorrection(input: String, expect: String, ignoreBlankLines: Boolean = false)(implicit l: Line) = {
    val rich = Text.CustomTextField.parse(project, None)(input)
    var actual = plainText.text(rich, Live, Optional)
    if (ignoreBlankLines)
      actual = actual.linesIterator.filter(_.trim.nonEmpty).mkString("\n")
    assertMultiline(actual.trim, expect.trim)
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
            |* a
            |* b
            |
            |hehe
            |
            |ok
            |
            |1. a
            |2. b
            |
            |how about both kinds of lists together
            |
            |* a
            |
            |1. a
            |
            |* a
            |* a
            |
            |1. a
            |2. a
            |
            |""".stripMargin.trim
        assertRoundTrip(input)
      }

      "olWithNLs1" - {
        val input =
          """
            |ok
            |
            |1. a
            |
            |2. b1
            |
            |   b2
            |
            |3. c
            |
            |4. d
            |
            |5. e1
            |
            |   e2
            |
            |6. f1
            |
            |   f2
            |
            |   f3
            |
            |7. x
            |
            |8. x
            |
            |9. x
            |
            |10. x
            |
            |    e
            |
            |good
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

      "olWithNLs2" - {
        val input =
          """
            |1. a
            |
            |2. b1
            |
            |   b2
            |
            |3. c1
            |
            |   c2
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
            |```md:render:x,y=1
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

      "headings" - {
        val input =
          """
            | # h1
            | #not
            |# !
            |  #   h1  again     !
            |      wow
            |
            | # # nope
            |### h3
            |
            |
            |
            |#### h4
            |
            |
            |x
            |
            |
            |##### h5
            |x
            |###### h6
            |###### h6b
            |###### h6c
            |""".stripMargin.replace("!", "")
        val expect =
          """
            |# h1
            |
            |#not
            |
            |#
            |
            |# h1 again
            |
            |wow
            |
            |# # nope
            |
            |### h3
            |
            |#### h4
            |
            |x
            |
            |##### h5
            |
            |x
            |
            |###### h6
            |
            |###### h6b
            |
            |###### h6c
            |""".stripMargin
        assertCorrection(input, expect)
      }

      "nestedLists" - {
        val input =
          """1. nice
            |* a
            |   * x
            |    * a
            |  * y
            |       * b
            |       * c
            |         * c1
            |        * c2
            |       * d
            |        * d1
            |  *   !
            |   * x
            |* b
            | 1. voi
            | *  oho
            | 1. ja
            | 1. ei
            | *  miksi
            |* c
            | 1. z
            |  nice
            | 1. w
            |  * xx
            |* d
            |1. cool
            |""".stripMargin.replace("!", "")
        val expect =
          """1. nice
            |* a
            |  * x
            |    * a
            |  * y
            |    * b
            |    * c
            |      * c1
            |      * c2
            |    * d
            |      * d1
            |  * !
            |    * x
            |* b
            |  1. voi
            |  * oho
            |  1. ja
            |  2. ei
            |  * miksi
            |* c
            |  1. z
            |     nice
            |  2. w
            |     * xx
            |* d
            |1. cool
            |""".stripMargin.replace("!", "")
        assertCorrection(input, expect, true) // TODO blank lines
      }
    }
  }
}
