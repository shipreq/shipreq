package shipreq.webapp.base.event

import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Text.{UseCaseTitle =>  UCT, UseCaseStep => UCST}
import ApplyEventTestFns._
import ContentEventTestHelp._

object UseCaseEventTest extends TestSuite {
  import CreateUseCaseGD._

  implicit def autoUseCaseId(i: Int) = UseCaseId(i)

  val someTitleUC: UCT.OptionalText =
    Vector(UCT.Literal("Look at "), UCT.WebAddress("https://google.com"))

  val setTitleUC1 = SetUseCaseTitle(1, someTitleUC)

  implicit val init = testHelpInit

  override def tests = TestSuite {

    'createUseCase {
      def expect(id    : UseCaseId        = emptyUC1.id,
                 pos   : ReqTypePos       = 1,
                 title : UCT.OptionalText = ∅,
                 stepId: UseCaseStepId    = emptyUC1.stepId): UseCase =
        UseCase.empty(id, pos, title, stepId)

      'empty {
        val p = _assertPass(emptyUC1)
        assertUC(p, 1)(expect())
      }

      'title {
        val t = NonEmptyVector(UCT.Literal("cool"))
        val p = _assertPass(emptyUC1.copy(vs = nev(Title(t))))
        assertUC(p, 1)(expect(title = t.whole))
      }

      'tags {
        val t = NonEmptySet(at1)
        val p = _assertPass(emptyUC1.copy(vs = nev(Tags(t))))
        assertUC(p, 1)(expect(), tags = t.whole)
      }

      'impSrc {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, CreateUseCase(5, 7, nev(ImpSrcs(v))))
        assertUC(p, 5)(expect(5, 7), impliedBy = v.whole)
        assertUC(p, 1)(expect(), implies = Set(5.UC))
      }

      'impTgt {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, CreateUseCase(5, 7, nev(ImpTgts(v))))
        assertUC(p, 5)(expect(5, 7), implies = v.whole)
        assertUC(p, 1)(expect(), impliedBy = Set(5.UC))
      }

      'reqCodes {
        val rcs = NonEmptySet[ReqCode.IdAndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(emptyUC1.copy(vs = nev(ReqCodes(rcs))))
        assertUC(p, 1)(expect(), reqCodes = rcs.whole.map(_.value))
        assertEq(p.reqCodes.reqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      'badId           - List(0, -1).foreach(i => assertFail("id")(emptyUC1.copy(id = i)))
      'idInUseByGR     - assertFail("exists")(createGR(emptyUC1.id.value.GR), emptyUC1)
      'idInUseByUC     - assertFail("exists")(emptyUC1, emptyUC1)
      'tagNotFound     - assertFail("tag")(emptyUC1.copy(vs = nev(Tags(6.AT))))
      'tagIsGroup      - assertFail("tag")(emptyUC1.copy(vs = nev(Tags(tg1.value.AT))))
      // tagIsDead - allow it
      'impSrcNotFound     - assertFail("")(emptyUC1.copy(vs = nev(ImpSrcs(123.UC))))
      'impTgtNotFound     - assertFail("")(emptyUC1.copy(vs = nev(ImpTgts(123.UC))))
      'impSrcSelf         - assertFail("")(emptyUC1.copy(vs = nev(ImpSrcs(1.UC))))
      'impTgtSelf         - assertFail("")(emptyUC1.copy(vs = nev(ImpTgts(1.UC))))
      'impCycle           - assertFail("")(emptyUC1, impliedUC2, CreateUseCase(3, 9, nev(ImpSrcs(2.UC), ImpTgts(1.UC))))
      'codeBad            - assertFail("")(emptyUC1.copy(vs = nev(ReqCodes(8 -> "!"))))
      'codeBadCaps        - assertFail("")(emptyUC1.copy(vs = nev(ReqCodes(8 -> "NO"))))
      'codeIdInUseByGR    - assertFail("")(createGR(1, codes = Set(5 -> "a"))   , createUC(2, 2, codes = Set(5 -> "b")))
      'codeIdInUseByUC    - assertFail("")(createUC(1, 1, codes = Set(5 -> "a")), createUC(2, 2, codes = Set(5 -> "b")))
      'codeIdInUseByGrp   - assertFail("")(createRCG(5, "a")                    , createUC(2, 2, codes = Set(5 -> "b")))
      'codeInUseByGR      - assertFail("")(createGR(1, codes = Set(5 -> "a"))   , createUC(2, 2, codes = Set(6 -> "a")))
      'codeInUseByUC      - assertFail("")(createUC(1, 1, codes = Set(5 -> "a")), createUC(2, 2, codes = Set(6 -> "a")))
      'codeInUseByGrp     - assertFail("")(createRCG(5, "a")                    , createUC(2, 2, codes = Set(6 -> "a")))
    }

    'setUseCaseTitle {
      'ok {
        val p = _assertPass(emptyUC1, setTitleUC1)
        assertEq(p.reqs.genericReqs.get(1).get.title, someTitleUC)
      }
      'reqNotFound - assertFail("found")(setTitleUC1)
      'reqIsDead   - assertFail("dead")(emptyUC1, delReq1, setTitleUC1)
    }

    'addUseCaseStep {
      // uc must exist

      // stepId unique
      // stepId valid

      // at must exist ifDefined

      // mustn't exceed max length
      // mustn't exceed max depth
    }

    'shiftUseCaseStepLeft {
      // step must exist
      // must be able to shift left
    }

    'shiftUseCaseStepRight {
      // step must exist
      // must be able to shift right
      // mustn't be NC root
    }

    'deleteUseCaseStep {
      // step must exist
      // mustn't be NC root
    }

    'setUseCaseStepText {
      // step must exist
    }
  }
}
