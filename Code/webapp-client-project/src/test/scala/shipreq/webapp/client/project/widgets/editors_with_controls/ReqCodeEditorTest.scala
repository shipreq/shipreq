package shipreq.webapp.client.project.widgets.editors_with_controls

import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.project.test.PrepareEnv
import utest._

object ReqCodeEditorTest extends TestSuite {
  PrepareEnv()

  override def tests = Tests {
    "forReqs" - {
      "liveCorrect" - {
        def test(in: String, out: String): Unit =
          assertEq(ReqCodeEditor.Multiple.liveCorrect(in), out)
        def testOk(is: String*): Unit =
          is.foreach(i => test(i, i))

        "lowercase" - {
          test("A", "a")
          test("abcE", "abce")
          test("ab.cE", "ab.ce")
        }
        "noSpace" - {
          test(" ", "")
          test("a ", "a")
        }
        "noSym" - {
          test("!", "")
          test("`", "")
          test("<", "")
        }
        "numbers" -
          testOk("1", "123", "1.2.3")
        "underscores" - {
          testOk("a_", "a_b", "abc_123")
          test("ab__", "ab_") // squash underscores together
          test("ab___c", "ab_c") // squash underscores together
        }
        "dot" - {
          testOk(".", ".abc123") // for auto-complete
          test("..", ".") // for auto-complete
          testOk("a.")
          test("a..", "a.")
        }
        "enter" - {
          testOk("a\n", "\nd", "a\n\nb", "abc.1\ndef\n")
          test("\n", "")
          test(" \n ", "")
//          test("b\n\n", "b\n")
//          test("\n\nc", "\nc")
//          test("\n \nc", "\nc")
//          test(" \n\nc", "\nc")
//          test("\n\n c", "\nc")
        }
      }
    }
    "forGroups" - {
      "liveCorrect" - {
        def test(in: String, out: String): Unit =
          assertEq(ReqCodeEditor.Single.liveCorrect(in), out)
        def testOk(is: String*): Unit =
          is.foreach(i => test(i, i))

        "lowercase" - {
          test("A", "a")
          test("abcE", "abce")
          test("ab.cE", "ab.ce")
        }
        "noSpace" - {
          test(" ", "")
          test("a ", "a")
        }
        "noSym" - {
          test("!", "")
          test("`", "")
          test("<", "")
        }
        "numbers" -
          testOk("1", "123", "1.2.3")
        "underscores" - {
          testOk("a_", "a_b", "abc_123")
          test("ab__", "ab_") // squash underscores together
          test("ab___c", "ab_c") // squash underscores together
        }
        "dot" - {
          testOk(".", ".abc123") // for auto-complete
          test("..", ".") // for auto-complete
          testOk("a.")
          test("a..", "a.")
        }
        "enter" -
          test("a\n", "a")
      }
    }
  }
}
