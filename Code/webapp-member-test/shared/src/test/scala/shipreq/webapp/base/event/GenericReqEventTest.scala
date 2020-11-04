package shipreq.webapp.base.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEventTestFns._
import shipreq.webapp.base.event.ContentEventTestHelp._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.test.UnsafeTypes.AutoNES._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.Text.{GenericReqTitle => GRT}
import utest._

object GenericReqEventTest extends TestSuite {
  import GenericReqGD._

  val someTitleGR: GRT.OptionalText =
    GRT(GRT.Literal("Look at "), GRT.WebAddress("https://google.com"))

  val setTitleGR1 = GenericReqTitleSet(1, someTitleGR)

  implicit val init = testHelpInit

  override def tests = Tests {

    "createGenericReq" - {
      "empty" - {
        val p = _assertPass(emptyGR1)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live))
      }

      "title" - {
        val t = GRT.nonEmpty(GRT.Literal("cool"))
        val p = _assertPass(emptyGR1.copy(vs = nev(Title(t))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), t.whole, Live))
      }

      "tags" - {
        val t = NonEmptySet(at1)
        val p = _assertPass(emptyGR1.copy(vs = nev(Tags(t))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), tags = t.whole)
      }

      "impSrc" - {
        val v = NonEmptySet[ReqId](emptyGR1.id)
        val p = _assertPass(emptyGR1, GenericReqCreate(5, mf, nev(ImpSrcs(v))))
        assertGR(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), impliedBy = v.whole)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), implies = Set(5))
      }

      "impTgt" - {
        val v = NonEmptySet[ReqId](emptyGR1.id)
        val p = _assertPass(emptyGR1, GenericReqCreate(5, mf, nev(ImpTgts(v))))
        assertGR(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), implies = v.whole)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), impliedBy = Set(5))
      }

      "reqCodes" - {
        val rcs = NonEmptySet[ApReqCodeId.AndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(emptyGR1.copy(vs = nev(Codes(rcs))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), reqCodes = rcs.whole.map(_.value))
        assertEq(p.content.reqCodes.apReqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      def customText: Event.NonEmptyCustomTextMap = NonEmpty.force(Map(cf1 -> "1", cf2 -> "2"))
      def createReqWithCustomText = emptyGR1.copy(vs = CustomText(customText))
      "customText" - {
        val p = _assertPass(createReqWithCustomText)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), customText = customText)
      }

      "customTextOnDeadField" - {
        assertFail("dead")(FieldCustomDelete(cf1), createReqWithCustomText)
      }

//      // This should technically fail - allow it for now
//      'customTextOnNonApplicableField {
//        import CustomTextFieldGD._
//        val makeNA = FieldCustomTextUpdate(cf1, ReqTypes(onlyReqTypes(fr)))
//        assertFail("")(makeNA, createReqWithCustomText)
//      }

      "badId"           - assertBadIdsRejected(i => emptyGR1.copy(id = i))
      "idInUseByGR"     - assertFail("exists")(emptyGR1, emptyGR1)
      "idInUseByUC"     - assertFail("unique req id")(createUC(emptyGR1.id.value.UC, 1), emptyGR1)
      "reqTypeNotFound" - assertFail("found")(emptyGR1.copy(rt = 666))
      "reqTypeDead"     - assertFail("dead")(emptyGR1, CustomReqTypeDelete(mf), emptyGR3)
      "tagNotFound"     - assertFail("tag")(emptyGR1.copy(vs = nev(Tags(6.AT))))
      "tagIsGroup"      - assertFail("tag")(emptyGR1.copy(vs = nev(Tags(tg1.value.AT))))
      // tagIsDead - allow it
      "impSrcNotFound"     - assertFail("")(emptyGR1.copy(vs = nev(ImpSrcs(123))))
      "impTgtNotFound"     - assertFail("")(emptyGR1.copy(vs = nev(ImpTgts(123))))
      "impSrcSelf"         - assertFail("")(emptyGR1.copy(vs = nev(ImpSrcs(1))))
      "impTgtSelf"         - assertFail("")(emptyGR1.copy(vs = nev(ImpTgts(1))))
      "impCycle"           - assertFail("")(emptyGR1, impliedGR2, GenericReqCreate(3, mf, nev(ImpSrcs(2), ImpTgts(1))))
      "codeBad"            - assertFail("")(emptyGR1.copy(vs = nev(Codes(8 -> "!"))))
      "codeBadCaps"        - assertFail("")(emptyGR1.copy(vs = nev(Codes(8 -> "NO"))))
      "codeIdInUseByGR"    - assertFail("")(createGR(1, codes = Set(5 -> "a"))      , createGR(2, codes = Set(5 -> "b")))
      "codeIdInUseByUC"    - assertFail("")(createUC(1.UC, 1, codes = Set(5 -> "a")), createGR(2, codes = Set(5 -> "b")))
      "codeIdInUseByRCG"   - assertFail("")(createRCG(5, "a")                       , createGR(2, codes = Set(5 -> "b")))
      "codeInUseByGR"      - assertFail("")(createGR(1, codes = Set(5 -> "a"))      , createGR(2, codes = Set(6 -> "a")))
      "codeInUseByUC"      - assertFail("")(createUC(1.UC, 1, codes = Set(5 -> "a")), createGR(2, codes = Set(6 -> "a")))
      "codeInUseByRCG"     - assertFail("")(createRCG(5, "a")                       , createGR(2, codes = Set(6 -> "a")))
    }

    "delete" - {
      implicit val init = ContentEventTest.init.add(createRCG1, emptyGR1, createGR(5), createRCG2)
      "reqNotFound"   - assertFail("not found")(ReqsDelete(9, 2, ∅))
      "groupNotFound" - assertFail("not found")(ReqsDelete(1, 9, ∅))
      "reqDead"       - assertFail("dead")(delGR(5), ReqsDelete(5, 2, ∅))
      "groupDead"     - assertFail("is not an ActiveGroup.")(delRCG2, ReqsDelete(5, 2, ∅))
      "ok" - {
        val p = _assertPass(ReqsDelete(5, 2, ∅))
        assertEq("RC#1", p.content.reqCodes.need(RCG1_code).isActive, true)
        assertEq("RC#2", p.content.reqCodes.need(RCG2_code).isActive, false)
        assertEq("GR #1", p.content.reqs.genericReqs.imap.need(1).liveExplicitly, Live)
        assertEq("GR #5", p.content.reqs.genericReqs.imap.need(5).liveExplicitly, Dead)
      }
    }

    "restore" - {
      "live"     - assertFail("is live")(emptyGR1, restoreGR1)
      "live2"    - assertFail("is live")(emptyGR1, delGR1, restoreGR1, restoreGR1)
      "ok"       - assertPass(emptyGR1, delGR1, restoreGR1)
      "ok2"      - assertPass(emptyGR1, delGR1, restoreGR1, delGR1, restoreGR1)
      "notFound" - assertFail("not found")(restoreGR1)
    }

    "setGenericReqType" - {
      "ok" - {
        var es = Vector[Event](emptyGR3, emptyGR1)
        def test(e: Event)(expect: PubidC): Unit = {
          es :+= e
          val p = _assertPass(es: _*)
          val d = p.content.reqs
          assertEq(d.genericReqs.imap.size, 2)
          assertEq(d.genericReqs.imap.get(1).get.pubid, expect)
        }
        test(GenericReqTypeSet(1, fr))(PubidT(fr, 1))
        test(GenericReqTypeSet(1, mf))(PubidT(mf, 2))
        test(GenericReqTypeSet(1, fr))(PubidT(fr, 1))
        test(GenericReqTypeSet(1, mf))(PubidT(mf, 2))
      }
      "reqNotFound"     - assertFail("found")(GenericReqTypeSet(1, fr))
      "reqIsDead"       - assertFail("dead")(emptyGR1, delGR1, GenericReqTypeSet(1, fr))
      "reqTypeNotFound" - assertFail("found")(emptyGR1, GenericReqTypeSet(1, 321))
      "reqTypeIsDead"   - assertFail("dead")(emptyGR1, createGR(8, fr), CustomReqTypeDelete(fr), GenericReqTypeSet(1, fr))
    }

    "setGenericReqTitle" - {
      "ok" - {
        val p = _assertPass(emptyGR1, setTitleGR1)
        assertEq(p.content.reqs.genericReqs.imap.get(1).get.title, someTitleGR)
      }
      "reqNotFound" - assertFail("found")(setTitleGR1)
      "reqIsDead"   - assertFail("dead")(emptyGR1, delGR1, setTitleGR1)
    }

  }
}
