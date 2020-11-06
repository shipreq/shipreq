package shipreq.webapp.member.project.filter

import shipreq.webapp.member.project.data.SpecialBuiltInField._
import shipreq.webapp.member.project.data.{Project, ReqTypePos}
import shipreq.webapp.member.project.event.{CustomTextFieldGD, Event}
import shipreq.webapp.member.project.filter.Filter.Implicits._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.{SampleProject6, SampleProject7}
import sourcecode.Line
import utest._

object ValidFilterTest extends TestSuite {

  private val PF = Filter.Potential
  private val VF = Filter.Valid

  private def assertTranslation(pf: Filter.Potential, p: Project = SampleProject6.project)(expect: Filter.Valid)(implicit l: Line): Unit =
    assertEq(Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)), \/-(expect))

  private def assertTranslationFails(pf: Filter.Potential, p: Project = SampleProject6.project)(errFrag: String)(implicit l: Line): Unit =
    Filter.Potential.validate(pf, FilterAlgebra.validate(p.config)) match {
      case -\/(e) => assertContainsCI(e.value, errFrag)
      case \/-(v) => fail(s"Expected an error containing '$errFrag'. Got: $v")
    }

  private def assertValidToText(vf: Filter.Valid, p: Project = SampleProject6.project)(expect: String)(implicit l: Line): Unit = {
    val text = Filter.Valid.toText(p.config, vf)
    assertEq(text, expect)
  }

  private implicit def liftFieldAttr(a: FilterAst.FieldAttr): VF.FieldCriteria =
    FilterAst.FieldCriteria.Attr(a)

  private def poses(i1: Int, in: Int*): VF.FieldCriteria =
    FilterAst.FieldCriteria.ReqTypePosSet(NonEmptySet(i1, in: _ *).map(ReqTypePos))

  override def tests = Tests {
    import shipreq.webapp.member.test.project.UnsafeTypes._
    import shipreq.webapp.member.test.project.SampleProject6.Values._
    import FilterAst.FieldAttr._

    "fromPotential" - {
      "reqType" - {
        "ok"  - assertTranslation(PF.reqType("MF"))(VF.reqType(mf))
        "bad" - assertTranslationFails(PF.reqType("XWE"))("unknown type")
      }
      "field" - {
        import FilterAst.FieldCriteria._
        import shipreq.webapp.member.test.project.UnsafeTypes.{autoHashRefKey => _, _}
        def posNA =   "You can't specify values"
        "exact"      - assertTranslation(PF.fieldProp("Description", Attr("blank")))(VF.fieldProp(\/-(descField), Blank))
        "caseWrong"  - assertTranslation(PF.fieldProp("descriPTION", Attr("blank")))(VF.fieldProp(\/-(descField), Blank))
        "title"      - assertTranslation(PF.fieldProp("TITLE", Attr("BLANK")))(VF.fieldProp(-\/(Title), Blank))
        "impField"   - assertTranslation(PF.fieldProp("MF", Attr("BLANK")))(VF.fieldProp(\/-(mfField), Blank))
        "posMF"      - assertTranslation(PF.fieldProp("MF", ReqTypePosSet(NonEmptySet(1, 3))))(VF.fieldProp(\/-(mfField), poses(1,3)))
        "posTitle"   - assertTranslationFails(PF.fieldProp("Title", ReqTypePosSet(NonEmptySet(1, 3))))(posNA)
        "posTxt"     - assertTranslationFails(PF.fieldProp("Description", ReqTypePosSet(NonEmptySet(1, 3))))(posNA)
        "posTag"     - assertTranslationFails(PF.fieldProp("Priority", ReqTypePosSet(NonEmptySet(1, 3))))(posNA)
        "queryMF"    - assertTranslation(PF.fieldProp("MF", Query(PF.text("x"))))(VF.fieldProp(\/-(mfField), Query(VF.text("x"))))
        "queryTitle" - assertTranslationFails(PF.fieldProp("Title", Query(PF.text("x"))))(posNA)
        "queryTxt"   - assertTranslationFails(PF.fieldProp("Description", Query(PF.text("x"))))(posNA)
        "queryTag"   - assertTranslationFails(PF.fieldProp("Priority", Query(PF.text("x"))))(posNA)
      }
      "scoped1" - {
        import FilterAst.Scope.Derivation
        val xp = PF.text("x")
        val xv = VF.text("x")

        "basic" - assertTranslation(
          PF.scoped1(false, NonEmptyVector(Derivation(None)), xp))(
          VF.scoped1(false, NonEmptySet(Derivation(None)), xv))

        "main" - assertTranslation(
          PF.scoped1(true, NonEmptyVector(Derivation(None)), xp))(
          VF.scoped1(true, NonEmptySet(Derivation(None)), xv))

        "fieldTag" - assertTranslation(
          PF.scoped1(false, NonEmptyVector(Derivation(Some("Status"))), xp))(
          VF.scoped1(false, NonEmptySet(Derivation(Some(statusField))), xv))

        "fieldText"  - assertTranslationFails(PF.scoped1(false, NonEmptyVector(Derivation(Some("Notes"))), xp))("")
        "fieldImp"   - assertTranslationFails(PF.scoped1(false, NonEmptyVector(Derivation(Some("Major Feature"))), xp))("")
        "fieldTitle" - assertTranslationFails(PF.scoped1(false, NonEmptyVector(Derivation(Some("Title"))), xp))("")
        "fieldBad"   - assertTranslationFails(PF.scoped1(false, NonEmptyVector(Derivation(Some("x"))), xp))("")
      }
      "scoped2" - {
        import FilterAst.Scope.Derivation
        val xp = PF.text("x")
        val xv = VF.text("x")
        val yp = PF.text("y")
        val yv = VF.text("y")

        "basic" - assertTranslation(
          PF.scoped2(NonEmptyVector(Derivation(None)), xp, yp))(
          VF.scoped2(NonEmptySet(Derivation(None)), xv, yv))

        "fieldTag" - assertTranslation(
          PF.scoped2(NonEmptyVector(Derivation(Some("Status"))), xp, yp))(
          VF.scoped2(NonEmptySet(Derivation(Some(statusField))), xv, yv))

        "fieldText"  - assertTranslationFails(PF.scoped2(NonEmptyVector(Derivation(Some("Notes"))), xp, yp))("")
        "fieldImp"   - assertTranslationFails(PF.scoped2(NonEmptyVector(Derivation(Some("Major Feature"))), xp, yp))("")
        "fieldTitle" - assertTranslationFails(PF.scoped2(NonEmptyVector(Derivation(Some("Title"))), xp, yp))("")
        "fieldBad"   - assertTranslationFails(PF.scoped2(NonEmptyVector(Derivation(Some("x"))), xp, yp))("")
      }
    }

    "validToText" - {
      "field" - {
        import FilterAst.FieldCriteria._
        "title"     - assertValidToText(VF.fieldProp(-\/(Title), Blank))("field:Title=blank")
        "customTxt" - assertValidToText(VF.fieldProp(\/-(descField), Blank))("field:Description=blank")
        "customImp" - assertValidToText(VF.fieldProp(\/-(mfField), Blank))("field:MF=blank")
        "quotes"    - {
          val e = Event.FieldCustomTextUpdate(descField, CustomTextFieldGD.nev(CustomTextFieldGD.ValueForName("ok good")))
          val p = applyEventsSuccessfully(SampleProject6.project, e)
          assertValidToText(VF.fieldProp(\/-(descField), Blank), p)("field:\"ok good\"=blank")
        }
        "pos" - assertValidToText(VF.fieldProp(\/-(mfField), poses(1,3,4,5,6,9)))("field:MF=1,3-6,9")
        "query1" - assertValidToText(VF.fieldProp(\/-(mfField), Query(VF.text("x"))))("field:MF=(x)")
        "query2" - assertValidToText(VF.fieldProp(\/-(mfField), Query(VF.anyOf(VF.text("x"), VF.text("y")))))("field:MF=(x | y)")
      }
    }

    "remove" - {
      def test(test: (String, String)): Unit = {
        import shipreq.webapp.member.test.project.SampleProject7.Values._

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
