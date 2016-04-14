package shipreq.webapp.base.event

import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Text.{GenericReqTitle => GRT}
import ApplyEventTestFns._
import AutoNES._
import ContentEventTestHelp._

object GenericReqEventTest extends TestSuite {
  import CreateGenericReqGD._

  val someTitleGR: GRT.OptionalText =
    Vector(GRT.Literal("Look at "), GRT.WebAddress("https://google.com"))

  val setTitleGR1 = SetGenericReqTitle(1, someTitleGR)

  implicit val init = testHelpInit

  override def tests = TestSuite {

    'createGenericReq {
      'empty {
        val p = _assertPass(emptyGR1)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live))
      }

      'title {
        val t = NonEmptyVector(GRT.Literal("cool"))
        val p = _assertPass(emptyGR1.copy(vs = nev(Title(t))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), t.whole, Live))
      }

      'tags {
        val t = NonEmptySet(at1)
        val p = _assertPass(emptyGR1.copy(vs = nev(Tags(t))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), tags = t.whole)
      }

      'impSrc {
        val v = NonEmptySet[ReqId](emptyGR1.id)
        val p = _assertPass(emptyGR1, CreateGenericReq(5, mf, nev(ImpSrcs(v))))
        assertGR(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), impliedBy = v.whole)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), implies = Set(5))
      }

      'impTgt {
        val v = NonEmptySet[ReqId](emptyGR1.id)
        val p = _assertPass(emptyGR1, CreateGenericReq(5, mf, nev(ImpTgts(v))))
        assertGR(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), implies = v.whole)
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), impliedBy = Set(5))
      }

      'reqCodes {
        val rcs = NonEmptySet[ReqCode.IdAndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(emptyGR1.copy(vs = nev(ReqCodes(rcs))))
        assertGR(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), reqCodes = rcs.whole.map(_.value))
        assertEq(p.reqCodes.reqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      'badId           - assertBadIdsRejected(i => emptyGR1.copy(id = i))
      'idInUseByGR     - assertFail("exists")(emptyGR1, emptyGR1)
      'idInUseByUC     - assertFail("unique req id")(createUC(emptyGR1.id.value.UC, 1), emptyGR1)
      'reqTypeNotFound - assertFail("found")(emptyGR1.copy(rt = 666))
      'reqTypeDead     - assertFail("dead")(DeleteCustomReqType(mf, Delete), emptyGR1)
      'tagNotFound     - assertFail("tag")(emptyGR1.copy(vs = nev(Tags(6.AT))))
      'tagIsGroup      - assertFail("tag")(emptyGR1.copy(vs = nev(Tags(tg1.value.AT))))
      // tagIsDead - allow it
      'impSrcNotFound     - assertFail("")(emptyGR1.copy(vs = nev(ImpSrcs(123))))
      'impTgtNotFound     - assertFail("")(emptyGR1.copy(vs = nev(ImpTgts(123))))
      'impSrcSelf         - assertFail("")(emptyGR1.copy(vs = nev(ImpSrcs(1))))
      'impTgtSelf         - assertFail("")(emptyGR1.copy(vs = nev(ImpTgts(1))))
      'impCycle           - assertFail("")(emptyGR1, impliedGR2, CreateGenericReq(3, mf, nev(ImpSrcs(2), ImpTgts(1))))
      'codeBad            - assertFail("")(emptyGR1.copy(vs = nev(ReqCodes(8 -> "!"))))
      'codeBadCaps        - assertFail("")(emptyGR1.copy(vs = nev(ReqCodes(8 -> "NO"))))
      'codeIdInUseByGR    - assertFail("")(createGR(1, codes = Set(5 -> "a"))      , createGR(2, codes = Set(5 -> "b")))
      'codeIdInUseByUC    - assertFail("")(createUC(1.UC, 1, codes = Set(5 -> "a")), createGR(2, codes = Set(5 -> "b")))
      'codeIdInUseByRCG   - assertFail("")(createRCG(5, "a")                       , createGR(2, codes = Set(5 -> "b")))
      'codeInUseByGR      - assertFail("")(createGR(1, codes = Set(5 -> "a"))      , createGR(2, codes = Set(6 -> "a")))
      'codeInUseByUC      - assertFail("")(createUC(1.UC, 1, codes = Set(5 -> "a")), createGR(2, codes = Set(6 -> "a")))
      'codeInUseByRCG     - assertFail("")(createRCG(5, "a")                       , createGR(2, codes = Set(6 -> "a")))
    }

    'delete {
      implicit val init = ContentEventTest.init.add(createRCG1, emptyGR1, createGR(5), createRCG2)
      'reqNotFound   - assertFail("not found")(DeleteReqs(9, 2, ∅))
      'groupNotFound - assertFail("not found")(DeleteReqs(1, 9, ∅))
      'reqDead       - assertFail("dead")(delGR(5), DeleteReqs(5, 2, ∅))
      'groupDead     - assertFail("is not an ActiveGroup.")(delRCG2, DeleteReqs(5, 2, ∅))
      'ok {
        val p = _assertPass(DeleteReqs(5, 2, ∅))
        assertEq("RC#1", p.reqCodes(RCG1_code).isActive, true)
        assertEq("RC#2", p.reqCodes(RCG2_code).isActive, false)
        assertEq("GR #1", p.reqs.genericReqs.need(1).liveExplicitly, Live)
        assertEq("GR #5", p.reqs.genericReqs.need(5).liveExplicitly, Dead)
      }
    }

    'restore {
      'live     - assertFail("is live")(emptyGR1, restoreGR1)
      'live2    - assertFail("is live")(emptyGR1, delGR1, restoreGR1, restoreGR1)
      'ok       - assertPass(emptyGR1, delGR1, restoreGR1)
      'ok2      - assertPass(emptyGR1, delGR1, restoreGR1, delGR1, restoreGR1)
      'notFound - assertFail("not found")(restoreGR1)
    }

    'setGenericReqType {
      'ok {
        var es = Vector[Event](emptyGR3, emptyGR1)
        def test(e: Event)(expect: PubidC): Unit = {
          es :+= e
          val p = _assertPass(es: _*)
          val d = p.reqs
          assertEq(d.genericReqs.size, 2)
          assertEq(d.genericReqs.get(1).get.pubid, expect)
        }
        test(SetGenericReqType(1, fr))(PubidT(fr, 1))
        test(SetGenericReqType(1, mf))(PubidT(mf, 2))
        test(SetGenericReqType(1, fr))(PubidT(fr, 1))
        test(SetGenericReqType(1, mf))(PubidT(mf, 2))
      }
      'reqNotFound     - assertFail("found")(SetGenericReqType(1, fr))
      'reqIsDead       - assertFail("dead")(emptyGR1, delGR1, SetGenericReqType(1, fr))
      'reqTypeNotFound - assertFail("found")(emptyGR1, SetGenericReqType(1, 321))
      'reqTypeIsDead   - assertFail("dead")(emptyGR1, DeleteCustomReqType(fr, Delete), SetGenericReqType(1, fr))
    }

    'setGenericReqTitle {
      'ok {
        val p = _assertPass(emptyGR1, setTitleGR1)
        assertEq(p.reqs.genericReqs.get(1).get.title, someTitleGR)
      }
      'reqNotFound - assertFail("found")(setTitleGR1)
      'reqIsDead   - assertFail("dead")(emptyGR1, delGR1, setTitleGR1)
    }

  }
}
