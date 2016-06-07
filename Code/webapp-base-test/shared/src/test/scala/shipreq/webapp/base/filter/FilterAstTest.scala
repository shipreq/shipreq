package shipreq.webapp.base.filter

import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test._
import shipreq.webapp.base.filter.{FilterSpec => S}
import FilterAst._

object FilterAstTest extends TestSuite {

  def assertTranslation(s: FilterSpec, p: Project = SampleProject6.project)(expect: FilterAst): Unit =
    assertEq(FilterAst(p, s), \/-(expect))

  def assertTranslationFails(s: FilterSpec, p: Project = SampleProject6.project)(errFrag: String): Unit =
    FilterAst(p, s) match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  override def tests = TestSuite {
    import UnsafeTypes._
    import SampleProject6.Values._

    'translation {

      'reqType {
        'ok  - assertTranslation(S.ReqType("MF"))(ReqType(mf))
        'bad - assertTranslationFails(S.ReqType("XWE"))("unknown type")
      }

    }
  }
}
