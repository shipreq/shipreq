package shipreq.webapp.base.event

import shipreq.webapp.base.AppConsts
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Text.{UseCaseTitle =>  UCT, UseCaseStep => UCST}
import ApplyEventTestFns._
import ContentEventTestHelp._
import StaticField.{NormalAltStepTree => NCAC, ExceptionStepTree => EC}

object UseCaseEventTest extends TestSuite {
  import CreateUseCaseGD._

  implicit def autoUseCaseId(i: Int) = UseCaseId(i)

  val someTitleUC: UCT.OptionalText =
    Vector(UCT.Literal("Look at "), UCT.WebAddress("https://google.com"))

  val setTitleUC1 = SetUseCaseTitle(1, someTitleUC)

  val someStepText: UCST.OptionalText =
    Vector(UCST.Literal("blah "), UCST.WebAddress("https://omfg.com"))

  val setStepTitle4 = SetUseCaseStepText(4, someStepText)

  implicit val init = testHelpInit

  def expect(id    : UseCaseId        = emptyUC1.id,
             pos   : ReqTypePos       = 1,
             title : UCT.OptionalText = ∅,
             stepId: UseCaseStepId    = emptyUC1.stepId): UseCase =
    UseCase.empty(id, pos, title, stepId)

  val UC1 = expect()
  def V0 = Vector1(0)

  def addStepTo1 = AddUseCaseStep(4, 1, NCAC, ∅)

  def maxLenRange = 0 to AppConsts.useCaseStepsMaxLength + 1

  def testSteps(es: ActiveEvent*)(nc: String*)(e: String*): Unit = {
    var es2 = es.toList
    if (!es2.exists(_ eq emptyUC1))
      es2 = emptyUC1 :: es2
    val p = _assertPass(es2: _*)
    assertUC(p, 1)(UC1)
    assertAllUcSteps(p needUC 1)(nc: _*)(e: _*)
  }

  override def tests = TestSuite {

    'createUseCase {
      'empty {
        val p = _assertPass(emptyUC1)
        assertUC(p, 1)(UC1)
      }

      'title {
        val t = NonEmptyVector(UCT.Literal("cool"))
        val p = _assertPass(emptyUC1.copy(vs = nev(Title(t))))
        assertUC(p, 1)(expect(title = t.whole))
      }

      'tags {
        val t = NonEmptySet(at1)
        val p = _assertPass(emptyUC1.copy(vs = nev(Tags(t))))
        assertUC(p, 1)(UC1, tags = t.whole)
      }

      'impSrc {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, CreateUseCase(5, 7, nev(ImpSrcs(v))))
        assertUC(p, 5)(expect(5, 7), impliedBy = v.whole)
        assertUC(p, 1)(UC1, implies = Set(5.UC))
      }

      'impTgt {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, CreateUseCase(5, 7, nev(ImpTgts(v))))
        assertUC(p, 5)(expect(5, 7), implies = v.whole)
        assertUC(p, 1)(UC1, impliedBy = Set(5.UC))
      }

      'reqCodes {
        val rcs = NonEmptySet[ReqCode.IdAndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(emptyUC1.copy(vs = nev(ReqCodes(rcs))))
        assertUC(p, 1)(UC1, reqCodes = rcs.whole.map(_.value))
        assertEq(p.reqCodes.reqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      'badId           - assertBadIdsRejected(i => emptyUC1.copy(id = i))
      'idInUseByGR     - assertFail("exists")(createGR(emptyUC1.id.value.GR), emptyUC1)
      'idInUseByUC     - assertFail("exists")(emptyUC1, emptyUC1.copy(stepId = 234))
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
      'badStepId          - assertBadIdsRejected(i => emptyUC1.copy(stepId = i))
      'stepIdInUse        - assertFail("exists")(emptyUC1, emptyUC1.copy(id = 98))
    }

    'setUseCaseTitle {
      'ok {
        val p = _assertPass(emptyUC1, setTitleUC1)
        assertEq(p.reqs.genericReqs.get(1).get.title, someTitleUC)
      }
      'ucNotFound - assertFail("found")(setTitleUC1)
      'ucIsDead   - assertFail("dead")(emptyUC1, delReq1, setTitleUC1)
    }

    'addUseCaseStep {
      def maxLenEvs(f: Int => AddUseCaseStep): List[ActiveEvent] =
        emptyUC1 :: maxLenRange.iterator.map(i => f(100 + i)).toList

      'okTailNCAC   - testSteps(AddUseCaseStep(4, 1, NCAC, ∅))("0", "1")()
      'okTailEC     - testSteps(AddUseCaseStep(4, 1, EC, ∅))("0")("0")
      'okInsertNCAC - testSteps(AddUseCaseStep(4, 1, NCAC, V0))("0", "0.0")()
      'okInsertEC   - testSteps(AddUseCaseStep(4, 1, EC, ∅), AddUseCaseStep(5, 1, EC, V0))("0")("0", "0.0")
      'ucNotFound   - assertFail("found")(addStepTo1)
      'ucDead       - assertFail("dead")(emptyUC1, delReq1, addStepTo1)
      'badId        - assertBadIdsRejected(AddUseCaseStep(_, 1, NCAC, ∅))(init.add(emptyUC1))
      'idInUse      - assertFail("exists")(emptyUC1, addStepTo1, addStepTo1)
      'locNotFound  - assertFail("???")(emptyUC1, AddUseCaseStep(5, 1, EC, V0))
      'maxLenAtRoot - assertFail("???")(maxLenEvs(AddUseCaseStep(_, 1, EC, ∅)): _*)
      'maxLenAtL0   - assertFail("???")(maxLenEvs(AddUseCaseStep(_, 1, NCAC, V0)): _*)
    }

    'shiftUseCaseStepLeft {
      def add = AddUseCaseStep(5, 1, NCAC, V0)
      def shift = ShiftUseCaseStepLeft(5)

      'ok           - testSteps(add, shift)("0", "1")()
      'stepNotFound - assertFail("found")(shift)
      'ucDead       - assertFail("dead")(emptyUC1, add, delReq1, shift)
      'notRoot      - assertFail("???")(emptyUC1, ShiftUseCaseStepLeft(1))
      'notLvl0      - assertFail("???")(emptyUC1, add, shift, shift)
    }

    'shiftUseCaseStepRight {
      def add = AddUseCaseStep(5, 1, NCAC, ∅)
      def shift = ShiftUseCaseStepRight(5)

      'ok           - testSteps(add, shift)("0", "0.0")()
      'stepNotFound - assertFail("found")(shift)
      'ucDead       - assertFail("dead")(emptyUC1, add, delReq1, shift)
      'notRoot      - assertFail("???")(emptyUC1, ShiftUseCaseStepRight(1))
      'noParent     - assertFail("???")(emptyUC1, add, shift, shift)

      // UC-8.0.1.a.i.1
      'maxDepthNCAC {
        var es = Vector.empty[ActiveEvent]
        def addShift(id: UseCaseStepId, level: Int) = {
          es :+= AddUseCaseStep(id, 1, NCAC, Vector.fill(level)(0))
          if (level > 1)
            es :+= ShiftUseCaseStepRight(id)
        }
        es :+= emptyUC1 // 1.0
        addShift(10, 1) // 1.0.1
        addShift(11, 2) // 1.0.1.a
        addShift(12, 3) // 1.0.1.a.i
        addShift(13, 4) // 1.0.1.a.i.1
        testSteps(es: _*)((1 to 5) map (List.fill(_)(0) mkString "."): _*)()

        addShift(99, 5) // over
        assertFail("???")(es: _*)
      }

      // UC-8.E.1.a.i.1
      'maxDepthEC {
        var es = Vector.empty[ActiveEvent]
        def addShift(id: UseCaseStepId, level: Int) = {
          es :+= AddUseCaseStep(id, 1, EC, Vector.fill(level)(0))
          if (level > 1)
            es :+= ShiftUseCaseStepRight(id)
        }
        es :+= emptyUC1 // 1.E
        addShift(10, 0) // 1.E.1
        addShift(11, 1) // 1.E.1.a
        addShift(12, 2) // 1.E.1.a.i
        addShift(13, 3) // 1.E.1.a.i.1
        testSteps(es: _*)("0")((1 to 4) map (List.fill(_)(0) mkString "."): _*)

        addShift(99, 4) // over
        assertFail("???")(es: _*)
      }
    }

    'deleteUseCaseStep {
      def create3 = Vector[ActiveEvent](
        emptyUC1,                                                           // 1.0
        AddUseCaseStep(7, 1, NCAC, V0),                                     // 1.0.1
        AddUseCaseStep(8, 1, NCAC, Vector(0, 0)), ShiftUseCaseStepRight(8)) // 1.0.1.a

      'okLeaf       - testSteps(create3 :+ DeleteUseCaseStep(8): _*)("0", "0.0")()
      'okParent     - testSteps(create3 :+ DeleteUseCaseStep(7): _*)("0")()
      'stepNotFound - assertFail("found")(DeleteUseCaseStep(7))
      'ucDead       - assertFail("dead")(create3 :+ delReq1 :+ DeleteUseCaseStep(8): _*)
      'notRootN     - assertFail("???")(emptyUC1, DeleteUseCaseStep(1))
      'okRootE      - testSteps(AddUseCaseStep(6, 1, EC, ∅), DeleteUseCaseStep(6))("0")()
    }

    'setUseCaseStepText {
      'ok {
        val p = _assertPass(emptyUC1, addStepTo1, setStepTitle4)
        assertEq(p.reqs.useCases.allSteps.need(4).step.title, someStepText)
      }
      'stepNotFound - assertFail("found")(setStepTitle4)
      'ucIsDead     - assertFail("dead")(emptyUC1, addStepTo1, delReq1, setStepTitle4)
    }

    // TODO Test step flow changes
  }
}
