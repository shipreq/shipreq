package shipreq.webapp.member.project.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.member.project.data
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.ApplyEventTestFns._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.NoInitialEvents._
import shipreq.webapp.member.project.event.RetiredGenericData.CustomReqTypeGDv1._
import shipreq.webapp.member.project.event.RetiredGenericData._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes.AutoNES._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

trait CustomReqTypeEventsV1 {
  val mfName = "Major Feature"
  type CE = CustomReqTypeCreateV1
  val c1  = CustomReqTypeCreateV1(1, nev(Mnemonic("MF"), Name(mfName), Implication(Mandatory)))
  val c2  = CustomReqTypeCreateV1(2, nev(Mnemonic("FR"), Name("Functional Req"), Implication(Optional)))
  val u1  = CustomReqTypeUpdateV1(1, nev(Mnemonic("M")))
  val sd1 = CustomReqTypeDelete(1)
  val r1  = CustomReqTypeRestore(1)
  val use1 = GenericReqCreate(1.GR, 1, GenericReqGD.emptyValues)
}

object CustomReqTypeEventV1SharedTests extends SharedTests with CustomReqTypeEventsV1 {
  override def setId(c: CE, i: Int) = c.copy(id = i)
  override def copyId(to: CE, from: CE) = to.copy(id = from.id)
  override def prepForSoftDelete(es: Event*) = c1 +: use1 +: es
}

object CustomReqTypeEventV1Test extends TestSuite with CustomReqTypeEventsV1 {

  implicit class CustomReqTypeCreateExt(private val a: CustomReqTypeCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needName" - assertFail("Name")    (c1.mod(_ - Name))
      "needMne"  - assertFail("Mnemonic")(c1.mod(_ - Mnemonic))
      "needImp"  - assertFail("Imp")     (c1.mod(_ - Implication))
      "badName"  - assertFail("blank")   (c1.mod(_ + Name("")))
      "badMne"   - assertFail("Mnemonic")(c1.mod(_ + Mnemonic("?")))
      "dupName"  - assertFail("unique")  (c1, c2.mod(_ + Name(mfName)))
      "dupMne"   - assertFail("unique")  (c1, c2.mod(_ + Mnemonic("MF")))
    }

    "update" - {
      "notInUse" - {
        var es = Vector(c1, u1)
        def r = _assertPass(es: _*).config.reqTypes.custom.get(1).get
        assertEq(r, CustomReqType.v1(1, "M", Set(), mfName, Mandatory, Live))

        es :+= CustomReqTypeUpdateV1(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r, CustomReqType.v1(1, "X", Set(), "xxx", Mandatory, Live))

        es :+= CustomReqTypeUpdateV1(1, nev(Mnemonic("MF"), Implication(Optional)))
        assertEq(r ,CustomReqType.v1(1, "MF", Set(), "xxx", Optional, Live))
      }
      "inUse" - {
        var es = Vector(c1, use1, u1)
        def r = _assertPass(es: _*).config.reqTypes.custom.get(1).get
        assertEq(r, CustomReqType.v1(1, "M", Set("MF"), mfName, Mandatory, Live))

        es :+= CustomReqTypeUpdateV1(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r, CustomReqType.v1(1, "X", Set("MF", "M"), "xxx", Mandatory, Live))

        es :+= CustomReqTypeUpdateV1(1, nev(Mnemonic("MF"), Implication(Optional)))
        assertEq(r ,CustomReqType.v1(1, "MF", Set("M", "X"), "xxx", Optional, Live))
      }
      "badName"  - assertFail("blank")   (c1, CustomReqTypeUpdateV1(1, nev(Name(""))))
      "badMne"   - assertFail("Mnemonic")(c1, CustomReqTypeUpdateV1(1, nev(Mnemonic("?"))))
      "dupName"  - assertFail("unique")  (c1, c2, CustomReqTypeUpdateV1(2, nev(Name(mfName))))
      "dupMne"   - assertFail("unique")  (c1, c2, CustomReqTypeUpdateV1(2, nev(Mnemonic("MF"))))
    }

    "delete" - {
      def testImpFieldLiveness(imp: Live, exp: Live)(es: Event*): Unit = {
        val p = _assertPass(es: _*)
        val f = p.config.fields.custom(CustomImpFieldEventV1Test.c1.id)
        assertEq("live", imp, f live p.config)
        assertEq("liveExplicitly", exp, f.liveExplicitly)
      }
      "whenLiveImpField" - {
        testImpFieldLiveness(Dead, Live)(c1, CustomImpFieldEventV1Test.c1, use1, sd1)
        testImpFieldLiveness(Live, Live)(c1, CustomImpFieldEventV1Test.c1, use1, sd1, r1)
      }
      "whenDeadImpField" - {
        testImpFieldLiveness(Dead, Dead)(c1, CustomImpFieldEventV1Test.c1, use1, CustomImpFieldEventV1Test.sd1, sd1)
        testImpFieldLiveness(Dead, Dead)(c1, CustomImpFieldEventV1Test.c1, use1, CustomImpFieldEventV1Test.sd1, sd1, r1)
      }
      "hardDelete" - {
        "notInUse" - {
          val p = _assertPass(c1, sd1)
          assertEq(p.config.reqTypes.custom.values.toList, Nil)
        }
        "inUseAsField" - {
          val p = _assertPass(c1, CustomImpFieldEventSharedTests.c1, sd1)
          assertEq(p.config.reqTypes.custom.values.toList, Nil)
          assertEq(p.config.fields.customImpFields.filter(_.reqTypeId == c1.id), Nil)
        }
        "inUseAsFieldApplicability" - {
          def test(before: ApplicableReqTypes, after: FieldReqTypeRules.ForTextField) = {
            import CustomTextFieldGDv1._
            val f  = FieldCustomTextCreateV1(2, nev(Name("R"), Key("r"), Mandatory(false), ApplicableReqTypes(before)))
            val p = _assertPass(c1, c2, f, sd1)
            assertEq(p.config.reqTypes.custom.values.toList.map(_.reqTypeId), c2.id :: Nil)
            assertEq(p.config.fields.customTextFields.size, 1)
            assertEq(p.config.fields.customTextFields.head.fieldReqTypeRules, after)
          }
          "not1"   - test(notReqTypes(1), FieldReqTypeRules.v1(data.Optional, allReqTypes))
          "only1"  - test(onlyReqTypes(1), FieldReqTypeRules.notApplicable)
          "not12"  - test(notReqTypes(1, 2), FieldReqTypeRules.v1(data.Optional, notReqTypes(2)))
          "only12" - test(onlyReqTypes(1, 2), FieldReqTypeRules.v1(data.Optional, onlyReqTypes(2)))
        }
      }

      "deleteRestoreReqsAndReqCodes" - {
        val t     = new EventTester
        t.makeName = (i, e) => s"#$i: $e"
        val reqId = GenericReqId(8)
        val rc    = ApReqCodeId.AndValue(9, "oh.good")
        def test(grLiveImp: Live)(e: Event): Unit =
          t(e) { name =>
            val r = t.p.content.reqs.genericReqs.imap.need(reqId)
            assertEq(s"$name - req.live", r live t.p.config.reqTypes, grLiveImp)
            assertEq(s"$name - req.expLive", r.liveExplicitly, Live)
            assertEq(s"$name - RC.active?", t.p.content.reqCodes.need(rc.value).isActive, grLiveImp is Live)
          }
        t.justApply(c1)
        test(Live)(GenericReqCreate(reqId, c1.id, GenericReqGD.Codes(rc)))
        test(Dead)(sd1)
        test(Live)(r1)
      }
    }
  }
}
