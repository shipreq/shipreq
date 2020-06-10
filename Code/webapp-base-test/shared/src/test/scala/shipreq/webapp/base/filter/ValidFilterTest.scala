package shipreq.webapp.base.filter

import scalaz.{-\/, \/-}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.data.SpecialBuiltInField._
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import utest._

object ValidFilterTest extends TestSuite {

  private val PF = Filter.Potential
  private val VF = Filter.Valid

  private def assertTranslation(pf: Filter.Potential, p: Project = SampleProject6.project)(expect: Filter.Valid): Unit =
    assertEq(Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)), \/-(expect))

  private def assertTranslationFails(pf: Filter.Potential, p: Project = SampleProject6.project)(errFrag: String): Unit =
    Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)) match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  override def tests = Tests {
    import UnsafeTypes._
    import SampleProject6.Values._
    import FilterAst.FieldAttr._

    "fromPotential" - {
      "reqType" - {
        "ok"  - assertTranslation(PF.reqType("MF"))(VF.reqType(mf))
        "bad" - assertTranslationFails(PF.reqType("XWE"))("unknown type")
      }
      "field" - {
        "exact" - assertTranslation(PF.fieldProp("Description", "blank"))(VF.fieldProp(\/-(descField), Blank))
        "caseWrong" - assertTranslation(PF.fieldProp("descriPTION", "blank"))(VF.fieldProp(\/-(descField), Blank))
        "title" - assertTranslation(PF.fieldProp("TITLE", "BLANK"))(VF.fieldProp(-\/(Title), Blank))
      }
    }

    "remove" - {
      def test(test: (String, String)): Unit = {
        import SampleProject7.Values._

        val (beforeTxt, expectedTxt) = test

        val before    = SampleProject7.filterParser(beforeTxt)
        val removal   = Filter.Valid.remove(
                          fields   = Set(),
                          reqTypes = Set(mf),
                        )
        val actual    = removal(before).toOption
        val actualTxt = actual.fold("")(Filter.Valid.toText(SampleProject7.project.config, _))

        assertEq(actualTxt, expectedTxt)
      }

      "any2"          - test("DD | MF | FR" -> "DD | FR")
      "any1"          - test("DD | MF"      -> "DD")
      "any0"          - test("MF | MF"      -> "")
      "all2"          - test("DD MF FR"     -> "")
      "all1"          - test("DD MF"        -> "")
      "all0"          - test("MF MF"        -> "")
      "andNot"        - test("DD -MF"       -> "DD")
      "orNot"         - test("DD | -MF"     -> "DD")
      "notAndAnd"     - test("-(MF MF) FR"  -> "FR")
      "notNotAndAnd"  - test("--(MF MF) FR" -> "")
    }
  }
}
