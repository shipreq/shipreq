package shipreq.webapp.base.filter

import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test._
import shipreq.webapp.base.filter.Filter.Implicits._

object ValidFilterTest extends TestSuite {

  val PF = Filter.Potential
  val VF = Filter.Valid

  def assertTranslation(pf: Filter.Potential, p: Project = SampleProject6.project)(expect: Filter.Valid): Unit =
    assertEq(Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)), \/-(expect))

  def assertTranslationFails(pf: Filter.Potential, p: Project = SampleProject6.project)(errFrag: String): Unit =
    Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)) match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  override def tests = TestSuite {
    import UnsafeTypes._
    import SampleProject6.Values._

    'fromPotential {
      'reqType {
        'ok  - assertTranslation(PF.reqType("MF"))(VF.reqType(mf))
        'bad - assertTranslationFails(PF.reqType("XWE"))("unknown type")
      }
    }
  }
}
