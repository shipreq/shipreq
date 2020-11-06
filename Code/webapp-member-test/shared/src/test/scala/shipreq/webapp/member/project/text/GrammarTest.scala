package shipreq.webapp.member.project.text

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

object GrammarTest extends TestSuite {

  override def tests = Tests {
    "pubid" - {
      "stringPrism" - {
        import Grammar.pubid.stringPrism

        def test(m: ReqType.Mnemonic, p: ReqTypePos)(exact: String)(ok: String*): Unit = {
          assertEq("pubid → string", stringPrism.reverseGet(ExternalPubid(m, p)), exact)
          for (s <- ok :+ exact)
            assertEq("string → pubid", stringPrism.getOption(s), Some(ExternalPubid(m, p)))
        }

        "*" - test("A", 4)("A-4")("a4", "a-4", "a-04", "A04")
        "*" - test("OMG", 10)("OMG-10")("Omg10", "OMG10", "omg10", "oMG-00010")
        "*" - test("MF", 0)("MF-0")("mf0")

        "bad" - {
          for (b <- List("mf-", "MF-9X", "9MF-9", "X--9"))
            assertEq("bad path", stringPrism.getOption(b), None)
        }
      }
    }
  }
}
