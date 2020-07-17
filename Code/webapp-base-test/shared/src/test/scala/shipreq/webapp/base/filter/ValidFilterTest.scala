package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.webapp.base.data.SpecialBuiltInField._
import shipreq.webapp.base.data.{Project, ReqTypePos}
import shipreq.webapp.base.event.{CustomTextFieldGD, Event}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import sourcecode.Line
import utest._

object ValidFilterTest extends TestSuite {

  private val PF = Filter.Potential
  private val VF = Filter.Valid

  private def assertTranslation(pf: Filter.Potential, p: Project = SampleProject6.project)(expect: Filter.Valid)(implicit l: Line): Unit =
    assertEq(Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)), \/-(expect))

  private def assertTranslationFails(pf: Filter.Potential, p: Project = SampleProject6.project)(errFrag: String)(implicit l: Line): Unit =
    Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)) match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  private def assertValidToText(vf: Filter.Valid, p: Project = SampleProject6.project)(expect: String)(implicit l: Line): Unit = {
    val text = Filter.Valid.toText(p.config, vf)
    assertEq(text, expect)
  }

  private implicit def liftFieldAttr(a: FilterAst.FieldAttr): VF.FieldCriteria =
    VF.FieldCriteria.Attr(a)

  private def poses(i1: Int, in: Int*): VF.FieldCriteria =
    VF.FieldCriteria.ReqTypePosSet(NonEmptySet(i1, in: _ *).map(ReqTypePos))

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
        def posNA = "You can't specify values"
        "exact"    - assertTranslation(PF.fieldProp("Description", "blank"))(VF.fieldProp(\/-(descField), Blank))
        "caseWrong"- assertTranslation(PF.fieldProp("descriPTION", "blank"))(VF.fieldProp(\/-(descField), Blank))
        "title"    - assertTranslation(PF.fieldProp("TITLE", "BLANK"))(VF.fieldProp(-\/(Title), Blank))
        "impField" - assertTranslation(PF.fieldProp("MF", "BLANK"))(VF.fieldProp(\/-(mfField), Blank))
        "posMF"    - assertTranslation(PF.fieldProp("MF", "1,4-6,9,9,2"))(VF.fieldProp(\/-(mfField), poses(1,4,5,6,9,2)))
        "posMFx"   - assertTranslationFails(PF.fieldProp("MF", "1,x4-6,9,9,2"))("isn't a legal set")
        "posTitle" - assertTranslationFails(PF.fieldProp("Title", "1,4-6,9,9,2"))(posNA)
        "posTxt"   - assertTranslationFails(PF.fieldProp("Description", "1,4-6,9,9,2"))(posNA)
        "posTag"   - assertTranslationFails(PF.fieldProp("Priority", "1,4-6,9,9,2"))(posNA)
      }
    }

    "validToText" - {
      "field" - {
        "title"     - assertValidToText(VF.fieldProp(-\/(Title), Blank))("field:Title=blank")
        "customTxt" - assertValidToText(VF.fieldProp(\/-(descField), Blank))("field:Description=blank")
        "customImp" - assertValidToText(VF.fieldProp(\/-(mfField), Blank))("field:MF=blank")
        "quotes"    - {
          val e = Event.FieldCustomTextUpdate(descField, CustomTextFieldGD.nev(CustomTextFieldGD.ValueForName("ok good")))
          val p = applyEventsSuccessfully(SampleProject6.project, e)
          assertValidToText(VF.fieldProp(\/-(descField), Blank), p)("field:\"ok good\"=blank")
        }
        "pos" - assertValidToText(VF.fieldProp(\/-(mfField), poses(1,3,4,5,6,9)))("field:MF=1,3-6,9")
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
