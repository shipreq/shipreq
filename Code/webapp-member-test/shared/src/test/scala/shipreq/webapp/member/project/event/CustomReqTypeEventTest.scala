package shipreq.webapp.member.project.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview._
import shipreq.webapp.member.project.event.ApplyEventTestFns._
import shipreq.webapp.member.project.event.CustomReqTypeGD._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.NoInitialEvents._
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.project.sort.SortMethod._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

trait CustomReqTypeEvents {

  val mfName = "Major Feature"
  type CE = CustomReqTypeCreate
  val c1  = CustomReqTypeCreate(1, nev(Mnemonic("MF"), Name(mfName), Implication(Mandatory), Description(None)))
  val c2  = CustomReqTypeCreate(2, nev(Mnemonic("FR"), Name("Functional Req"), Implication(Optional), Description("Stuff!")))
  val u1  = CustomReqTypeUpdate(1, nev(Mnemonic("M")))
  val sd1 = CustomReqTypeDeleteSoft(1)
  val hd1 = CustomReqTypeDeleteHard(1)
  val r1  = CustomReqTypeRestore(1)
  val gr1 = GenericReqCreate(1.GR, 1, GenericReqGD.emptyValues)
}

object CustomReqTypeEventSharedTests extends SharedTests with CustomReqTypeEvents {
  override def setId(c: CE, i: Int) = c.copy(id = i)
  override def copyId(to: CE, from: CE) = to.copy(id = from.id)
  override def prepForSoftDelete(es: Event*) = c1 +: gr1 +: es
}

object CustomReqTypeEventTest extends TestSuite with CustomReqTypeEvents {
  import StaticReqType.{UseCase => uc}
  import SortCriterion.SyntaxHelpers._

  implicit class CustomReqTypeCreateExt(private val a: CustomReqTypeCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  private def sv1   = SavedView.Id(1)
  private def tag1  = 101.AT
  private def impf1 = 101.CFImp
  private def impf2 = 102.CFImp
  private def col1  = Column.CustomField(impf1)
  private def col2  = Column.CustomField(impf2)

  private def createImpF1  = FieldCustomImpCreate(impf1, 1, CustomImpFieldGD(FieldReqTypeRules.optional.mandatory(2, uc)))
  private def createImpF2  = FieldCustomImpCreate(impf2, 2, CustomImpFieldGD(FieldReqTypeRules.optional.mandatory(1, 2, uc)))

  private def createTag1  = ApplicableTagCreate(tag1, ApplicableTagGD("a", ∅, ∅, notReqTypes(1, 2, uc), ∅, ∅))

  private def columnsBefore      = Column.mandatory.toNEV :+ col1 :+ col2
  private def columnsAfter       = Column.mandatory.toNEV :+ col2
  private def sortCriteriaBefore = SortCriteria(Vector(col1 / AscThenBlanks, col2 / DescThenBlanks), SortCriteria.defaultConclusive)
  private def sortCriteriaAfter  = SortCriteria(Vector(col2 / DescThenBlanks), SortCriteria.defaultConclusive)
  private def filterBefore       = Filter.Valid.anyOf(Filter.Valid.reqType(1), Filter.Valid.reqType(2))
  private def filterAfter        = Filter.Valid.reqType(2)

  private def createSV =
    SavedViewCreateV1(
      id         = sv1,
      name       = SavedView.Name("yes"),
      columns    = columnsBefore,
      order      = sortCriteriaBefore,
      filterDead = HideDead,
      filter     = Some(filterBefore),
    )

  override def tests = Tests {

    "create" - {
      "needName" - assertFail("Name")    (c1.mod(_ - Name))
      "needMne"  - assertFail("Mnemonic")(c1.mod(_ - Mnemonic))
      "needImp"  - assertFail("Imp")     (c1.mod(_ - Implication))
      "needDesc" - assertFail("Desc")    (c1.mod(_ - Description))
      "badName"  - assertFail("blank")   (c1.mod(_ + Name("")))
      "badMne"   - assertFail("Mnemonic")(c1.mod(_ + Mnemonic("?")))
      "dupName"  - assertFail("unique")  (c1, c2.mod(_ + Name(mfName)))
      "dupMne"   - assertFail("unique")  (c1, c2.mod(_ + Mnemonic("MF")))
    }

    "update" - {

      def assertNoRetention(events: Event*): Unit = {
        var es = c1 +: c2 +: events.toVector :+ u1
        def p() = _assertPass(es: _*)
        def r() = p().config.reqTypes.custom.get(1).get
        assertEq(r(), CustomReqType.v1(1, "M", Set(), mfName, Mandatory, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r(), CustomReqType.v1(1, "X", Set(), "xxx", Mandatory, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("MF"), Implication(Optional)))
        assertEq(r(), CustomReqType.v1(1, "MF", Set(), "xxx", Optional, Live))
      }

      def assertRetention(events: Event*): Unit = {
        var es = c1 +: c2 +: events.toVector :+ u1
        def p() = _assertPass(es: _*)
        def r() = p().config.reqTypes.custom.get(1).get
        assertEq(r(), CustomReqType.v1(1, "M", Set("MF"), mfName, Mandatory, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r(), CustomReqType.v1(1, "X", Set("MF", "M"), "xxx", Mandatory, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("MF"), Implication(Optional)))
        assertEq(r(), CustomReqType.v1(1, "MF", Set("M", "X"), "xxx", Optional, Live))
      }

      "notInUse"   - assertNoRetention()
      "liveReq"    - assertRetention(gr1)
      "deadReq"    - assertRetention(gr1, ReqsDelete.one(1.GR))
      "exReq"      - assertRetention(gr1, GenericReqTypeSet(1.GR, 2))
      "impField"   - assertNoRetention(createImpF1, createImpF2)
      "savedViews" - assertNoRetention(createImpF1, createImpF2, createSV)
      "fieldRules" - assertNoRetention(createImpF1, createImpF2)
      "tagRules"   - assertNoRetention(createTag1)

      "badName"    - assertFail("blank")   (c1, CustomReqTypeUpdate(1, nev(Name(""))))
      "badMne"     - assertFail("Mnemonic")(c1, CustomReqTypeUpdate(1, nev(Mnemonic("?"))))
      "dupName"    - assertFail("unique")  (c1, c2, CustomReqTypeUpdate(2, nev(Name(mfName))))
      "dupMne"     - assertFail("unique")  (c1, c2, CustomReqTypeUpdate(2, nev(Mnemonic("MF"))))
    }

    "softDelete" - {
      def test(events: Event*): Unit = {
        val es = c1 +: c2 +: events.toVector
        val p1 = _assertPass(es: _*)
        val p2 = applyEventSuccessfully(p1, sd1)
        val p3 = applyEventSuccessfully(p2, r1)
        assertEq(p2.config.reqTypes.custom.need(1).live, Dead)
        assertEq(p3, p1)
      }

      "notInUse"   - test()
      "liveReq"    - test(gr1)
      "deadReq"    - test(gr1, ReqsDelete.one(1.GR))
      "exReq"      - test(gr1, GenericReqTypeSet(1.GR, 2))
      "impField"   - test(createImpF1, createImpF2)
      "savedViews" - test(createImpF1, createImpF2, createSV)
      "fieldRules" - test(createImpF1, createImpF2)
      "tagRules"   - test(createTag1)
    }

    "hardDelete" - {

      def assertFail(events: Event*): Unit = {
        val es = c1 +: c2 +: events.toVector
        val p = _assertPass(es: _*)
        assertEventFails(p, hd1)
      }

      def assertPassP(events: Event*): Project = {
        val es = c1 +: c2 +: events.toVector :+ hd1
        val p = _assertPass(es: _*)
        assertEq(p.config.reqTypes.get(1), None)
        assertNotEq(p.config.reqTypes.get(2), None)
        p
      }

      def assertPass(events: Event*): Unit =
        assertPassP(events: _*)

      "notInUse"  - assertPass()
      "liveReq"   - assertFail(gr1)
      "deadReq"   - assertFail(gr1, ReqsDelete.one(1.GR))
      "exReq"     - assertFail(gr1, GenericReqTypeSet(1.GR, 2))

      "impField" - {
        val p = assertPassP(createImpF1, createImpF2)
        assertEq(p.config.fields.customFields.get(impf1), None)
      }

      "savedViews" - {
        val p = assertPassP(createImpF1, createImpF2, createSV)
        val sv = p.savedViews.get.get(sv1).get
        assertEq(sv.view, View(
          columns        = columnsAfter,
          order          = sortCriteriaAfter,
          filterDead     = HideDead,
          filter         = Some(filterAfter),
          impGraphConfig = None,
        ))
      }

      "fieldRules" - {
        val p = assertPassP(createImpF1, createImpF2)
        assertEq(p.config.fields.custom(impf2), CustomField.Implication(
          id                = impf2,
          reqTypeId         = 2,
          fieldReqTypeRules = FieldReqTypeRules.optional.mandatory(2, uc),
          liveExplicitly    = Live,
        ))
      }

      "tagRules" - {
        val p = assertPassP(createTag1)
        val t = p.config.tags.needApplicableTag(tag1)
        assertEq(t.applicableReqTypes, notReqTypes(2, uc))
      }
    }

  }
}
