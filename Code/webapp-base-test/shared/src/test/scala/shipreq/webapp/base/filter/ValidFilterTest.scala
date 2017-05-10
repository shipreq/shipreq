package shipreq.webapp.base.filter

import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test._
import shipreq.webapp.base.filter.{PotentialFilter => PF}
import ValidFilter._

object ValidFilterTest extends TestSuite {

  def assertTranslation(s: PF, p: Project = SampleProject6.project)(expect: ValidFilter): Unit =
    assertEq(PF.validator(p).run(s), \/-(expect))

  def assertTranslationFails(s: PF, p: Project = SampleProject6.project)(errFrag: String): Unit =
    PF.validator(p).run(s) match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  override def tests = TestSuite {
    import UnsafeTypes._
    import SampleProject6.Values._

    'translation {

      'reqType {
        'ok  - assertTranslation(PF.ReqType("MF"))(ReqType(mf))
        'bad - assertTranslationFails(PF.ReqType("XWE"))("unknown type")
      }

    }
  }
}
