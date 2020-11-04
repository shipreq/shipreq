package shipreq.webapp.base.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.base.util._
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.member.text.Text
import shipreq.webapp.member.text.Text.{UseCaseStep => UCST, UseCaseTitle => UCT}
import utest._

object UseCaseEventTest extends TestSuite {
  import ApplyEventTestFns._
  import ContentEventTestHelp._
  import Event._
  import StaticField.{NormalAltStepTree => NCAC, ExceptionStepTree => EC}
  import UnsafeTypes._
  import UnsafeTypes.AutoNES._
  import UseCaseGD._

  implicit def autoUseCaseId(i: Int) = UseCaseId(i)
  implicit def autoUseCaseIdNes(i: Int) = NonEmptySet[ReqId](UseCaseId(i))

  val someTitleUC: UCT.OptionalText =
    UCT(UCT.Literal("Look at "), UCT.WebAddress("https://google.com"))

  val setTitleUC1 = UseCaseTitleSet(1, someTitleUC)

  val someStepText: UCST.OptionalText =
    UCST(UCST.Literal("blah "), UCST.WebAddress("https://omfg.com"))

  val ^ = UseCaseStepGD
  val setStepTitle4 = UseCaseStepUpdate(4, ^.Title(someStepText))

  implicit val init = testHelpInit

  def expect(id    : UseCaseId        = emptyUC1.id,
             pos   : ReqTypePos       = 1,
             title : UCT.OptionalText = ∅,
             stepId: UseCaseStepId    = emptyUC1.stepId): UseCase =
    UseCase.empty(id, pos, title, stepId)

  val UC1 = expect()
  def V0 = Vector1(0)

  implicit def autoVectorToParentLoc(v: Vector[Int]): VectorTree.ParentLocation =
    VectorTree.ParentLocation fromVector v

  def addStepTo1 = UseCaseStepCreate(4, 1, NCAC, ∅)

  def maxLenRange = 0 to WebappConfig.useCaseStepsMaxLength

  def testSteps(es: ActiveEvent*)(nc: String*)(e: String*)(implicit l: sourcecode.Line): Unit = {
    var es2 = es.toList
    if (!es2.exists(_ eq emptyUC1))
      es2 = emptyUC1 :: es2
    val p = _assertPass(es2: _*)
    assertUC(p, 1)(UC1, ignoreSteps = true)
    assertAllUcSteps(p needUC 1)(nc: _*)(e: _*)
  }

  override def tests = Tests {

    "createUseCase" - {
      "empty" - {
        val p = _assertPass(emptyUC1)
        assertUC(p, 1)(UC1)
      }

      "title" - {
        val t = UCT.nonEmpty(UCT.Literal("cool"))
        val p = _assertPass(emptyUC1.copy(vs = nev(Title(t))))
        assertUC(p, 1)(expect(title = t.whole))
      }

      "tags" - {
        val t = NonEmptySet(at1)
        val p = _assertPass(emptyUC1.copy(vs = nev(Tags(t))))
        assertUC(p, 1)(UC1, tags = t.whole)
      }

      "impSrc" - {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, UseCaseCreate(5, 7, nev(ImpSrcs(v))))
        assertUC(p, 5)(expect(5, 2, stepId = 7), impliedBy = v.whole)
        assertUC(p, 1)(UC1, implies = Set(5.UC))
      }

      "impTgt" - {
        val v = NonEmptySet[ReqId](emptyUC1.id)
        val p = _assertPass(emptyUC1, UseCaseCreate(5, 7, nev(ImpTgts(v))))
        assertUC(p, 5)(expect(5, 2, stepId = 7), implies = v.whole)
        assertUC(p, 1)(UC1, impliedBy = Set(5.UC))
      }

      "reqCodes" - {
        val rcs = NonEmptySet[ApReqCodeId.AndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(emptyUC1.copy(vs = nev(Codes(rcs))))
        assertUC(p, 1)(UC1, reqCodes = rcs.whole.map(_.value))
        assertEq(p.content.reqCodes.apReqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      def customText: Event.NonEmptyCustomTextMap = NonEmpty.force(Map(cf1 -> "1", cf2 -> "2"))
      def createReqWithCustomText = emptyUC1.copy(vs = CustomText(customText))
      "customText" - {
        val p = _assertPass(createReqWithCustomText)
        assertUC(p, 1)(UC1, customText = customText)
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

      "badId"           - assertBadIdsRejected(i => emptyUC1.copy(id = i))
      "idInUseByGR"     - assertFail("unique req id")(createGR(emptyUC1.id.value.GR), emptyUC1)
      "idInUseByUC"     - assertFail("exists")(emptyUC1, emptyUC1.copy(stepId = 234))
      "tagNotFound"     - assertFail("tag")(emptyUC1.copy(vs = nev(Tags(6.AT))))
      "tagIsGroup"      - assertFail("tag")(emptyUC1.copy(vs = nev(Tags(tg1.value.AT))))
      // tagIsDead - allow it
      "impSrcNotFound"     - assertFail("")(emptyUC1.copy(vs = nev(ImpSrcs(123.UC))))
      "impTgtNotFound"     - assertFail("")(emptyUC1.copy(vs = nev(ImpTgts(123.UC))))
      "impSrcSelf"         - assertFail("")(emptyUC1.copy(vs = nev(ImpSrcs(1.UC))))
      "impTgtSelf"         - assertFail("")(emptyUC1.copy(vs = nev(ImpTgts(1.UC))))
      "impCycle"           - assertFail("")(emptyUC1, impliedUC2, UseCaseCreate(3, 9, nev(ImpSrcs(2.UC), ImpTgts(1.UC))))
      "codeBad"            - assertFail("")(emptyUC1.copy(vs = nev(Codes(8 -> "!"))))
      "codeBadCaps"        - assertFail("")(emptyUC1.copy(vs = nev(Codes(8 -> "NO"))))
      "codeIdInUseByGR"    - assertFail("")(createGR(1, codes = Set(5 -> "a"))   , createUC(2, 2, codes = Set(5 -> "b")))
      "codeIdInUseByUC"    - assertFail("")(createUC(1, 1, codes = Set(5 -> "a")), createUC(2, 2, codes = Set(5 -> "b")))
      "codeIdInUseByGrp"   - assertFail("")(createRCG(5, "a")                    , createUC(2, 2, codes = Set(5 -> "b")))
      "codeInUseByGR"      - assertFail("")(createGR(1, codes = Set(5 -> "a"))   , createUC(2, 2, codes = Set(6 -> "a")))
      "codeInUseByUC"      - assertFail("")(createUC(1, 1, codes = Set(5 -> "a")), createUC(2, 2, codes = Set(6 -> "a")))
      "codeInUseByGrp"     - assertFail("")(createRCG(5, "a")                    , createUC(2, 2, codes = Set(6 -> "a")))
      "badStepId"          - assertBadIdsRejected(i => emptyUC1.copy(stepId = i))
      "stepIdInUse"        - assertFail("exists")(emptyUC1, emptyUC1.copy(id = 98))
    }

    "delete" - {
      implicit val init = ContentEventTest.init.add(createRCG1, emptyUC1, createUC(5, 9), createRCG2)
      "reqNotFound"   - assertFail("not found")(ReqsDelete(9, 2, ∅))
      "groupNotFound" - assertFail("not found")(ReqsDelete(1, 9, ∅))
      "reqDead"       - assertFail("dead")(delUC(5), ReqsDelete(5, 2, ∅))
      "groupDead"     - assertFail("is not an ActiveGroup.")(delRCG2, ReqsDelete(5, 2, ∅))
      "ok" - {
        val p = _assertPass(ReqsDelete(5, 2, ∅))
        assertEq("RC#1", p.content.reqCodes.need(RCG1_code).isActive, true)
        assertEq("RC#2", p.content.reqCodes.need(RCG2_code).isActive, false)
        assertEq("UC #1", p.content.reqs.useCases.imap.need(1).liveExplicitly, Live)
        assertEq("UC #5", p.content.reqs.useCases.imap.need(5).liveExplicitly, Dead)
      }
    }

    "restore" - {
      "live"     - assertFail("is live")(emptyUC1, restoreUC1)
      "live2"    - assertFail("is live")(emptyUC1, delUC1, restoreUC1, restoreUC1)
      "ok"       - assertPass(emptyUC1, delUC1, restoreUC1)
      "ok2"      - assertPass(emptyUC1, delUC1, restoreUC1, delUC1, restoreUC1)
      "notFound" - assertFail("not found")(restoreUC1)
    }

    "setUseCaseTitle" - {
      "ok" - {
        val p = _assertPass(emptyUC1, setTitleUC1)
        assertEq(p.content.reqs.useCases.imap.need(1).title, someTitleUC)
      }
      "ucNotFound" - assertFail("found")(setTitleUC1)
      "ucIsDead"   - assertFail("dead")(emptyUC1, delUC1, setTitleUC1)
    }

    "addUseCaseStep" - {
      def maxLenEvs(f: Int => UseCaseStepCreate): List[ActiveEvent] =
        emptyUC1 :: maxLenRange.iterator.map(i => f(100 + i)).toList

      "okTailNCAC"   - testSteps(UseCaseStepCreate(4, 1, NCAC, ∅))("0", "1")()
      "okTailEC"     - testSteps(UseCaseStepCreate(4, 1, EC, ∅))("0")("0")
      "okInsertNCAC" - testSteps(UseCaseStepCreate(4, 1, NCAC, V0))("0", "0.0")()
      "okInsertEC"   - testSteps(UseCaseStepCreate(4, 1, EC, ∅), UseCaseStepCreate(5, 1, EC, V0))("0")("0", "0.0")
      "ucNotFound"   - assertFail("not found")(addStepTo1)
      "ucDead"       - assertFail("dead")(emptyUC1, delUC1, addStepTo1)
      "badId"        - assertBadIdsRejected(UseCaseStepCreate(_, 1, NCAC, ∅))(init.add(emptyUC1))
      "idInUse"      - assertFail("exists")(emptyUC1, addStepTo1, addStepTo1)
      "locNotFound"  - assertFail("cannot")(emptyUC1, UseCaseStepCreate(5, 1, EC, V0))
      "maxLenAtRoot" - assertFail("exceeds limit")(maxLenEvs(UseCaseStepCreate(_, 1, EC, ∅)): _*)
      "maxLenAtL0"   - assertFail("exceeds limit")(maxLenEvs(UseCaseStepCreate(_, 1, NCAC, V0)): _*)
    }

    "shiftUseCaseStepLeft" - {
      def add = UseCaseStepCreate(5, 1, NCAC, V0)
      def shift = UseCaseStepShiftLeft(5)

      "ok"           - testSteps(add, shift)("0", "1")()
      "stepNotFound" - assertFail("found")(shift)
      "ucDead"       - assertFail("dead")(emptyUC1, add, delUC1, shift)
      "notRoot"      - assertFail("cannot")(emptyUC1, UseCaseStepShiftLeft(1))
      "notLvl0"      - assertFail("cannot")(emptyUC1, add, shift, shift)
    }

    "shiftUseCaseStepRight" - {
      def add = UseCaseStepCreate(5, 1, NCAC, ∅)
      def shift = UseCaseStepShiftRight(5)

      "ok"           - testSteps(add, shift)("0", "0.0")()
      "stepNotFound" - assertFail("found")(shift)
      "ucDead"       - assertFail("dead")(emptyUC1, add, delUC1, shift)
      "notRoot"      - assertFail("cannot")(emptyUC1, UseCaseStepShiftRight(1))
      "noParent"     - assertFail("cannot")(emptyUC1, add, shift, shift)

      // UC-8.0.1.a.i.1
      "maxDepthNCAC" - {
        var es = Vector.empty[ActiveEvent]
        def addShift(id: UseCaseStepId, level: Int) = {
          es :+= UseCaseStepCreate(id, 1, NCAC, Vector.fill(level)(0))
          if (level > 1)
            es :+= UseCaseStepShiftRight(id)
        }
        es :+= emptyUC1 // 1.0
        addShift(10, 1) // 1.0.1
        addShift(11, 2) // 1.0.1.a
        addShift(12, 3) // 1.0.1.a.i
        addShift(13, 4) // 1.0.1.a.i.1
        testSteps(es: _*)((1 to 5) map (List.fill(_)(0) mkString "."): _*)()

        addShift(99, 5) // over
        assertFail("exceeds limit")(es: _*)
      }

      // UC-8.E.1.a.i.1
      "maxDepthEC" - {
        var es = Vector.empty[ActiveEvent]
        def addShift(id: UseCaseStepId, level: Int) = {
          es :+= UseCaseStepCreate(id, 1, EC, Vector.fill(level)(0))
          if (level > 1)
            es :+= UseCaseStepShiftRight(id)
        }
        es :+= emptyUC1 // 1.E
        addShift(10, 0) // 1.E.1
        addShift(11, 1) // 1.E.1.a
        addShift(12, 2) // 1.E.1.a.i
        addShift(13, 3) // 1.E.1.a.i.1
        testSteps(es: _*)("0")((1 to 4) map (List.fill(_)(0) mkString "."): _*)

        addShift(99, 4) // over
        assertFail("exceeds limit")(es: _*)
      }
    }

    "deleteUseCaseStep" - {

      /** Creates a use case with 3 steps */
      def create3 = Vector[ActiveEvent](
        emptyUC1,                                    // 1.0
        UseCaseStepCreate(7, 1, NCAC, V0),           // 1.0.1
        UseCaseStepCreate(8, 1, NCAC, Vector(0, 0)),
        UseCaseStepShiftRight(8))                    // 1.0.1.a

      "okLeaf"        - testSteps(create3 :+ UseCaseStepDelete(8): _*)("0", "0.0")()
      "okParent"      - testSteps(create3 :+ UseCaseStepDelete(7): _*)("0")()
      "stepNotFound"  - assertFail("found")(UseCaseStepDelete(7))
      "ucDead"        - assertFail("dead")(create3 :+ delUC1 :+ UseCaseStepDelete(8): _*)
      "notRootN"      - assertFail("forbidden")(emptyUC1, UseCaseStepDelete(1))
      "okRootE"       - testSteps(UseCaseStepCreate(6, 1, EC, ∅), UseCaseStepDelete(6))("0")()

      "retainsFlow" - {
        val es = create3 ++ Seq(
          UseCaseStepCreate(9, 1, NCAC, V0),              // add 1.1
          UseCaseStepUpdate(1, ^.FlowOut(nesd()(7))),     // 1.0     → [1.0.1]
          UseCaseStepUpdate(7, ^.FlowOut(nesd()(9))),     // 1.0.1   → [1.1]
          UseCaseStepUpdate(8, ^.FlowOut(nesd()(1,7,9))), // 1.0.1.a → [1.0, 1.0.1, 1.1]
          UseCaseStepUpdate(9, ^.FlowOut(nesd()(1))),     // 1.1     → [1.0]
          UseCaseStepDelete(7))                           // del 1.0.1
        val p = _assertPass(es: _*)
        val e = UseCases.StepFlow.emptyUniDir
                  .addvs(1, Set(7))     // 1.0     → [1.0.1]
                  .addvs(7, Set(9))     // 1.0.1   → [1.1]
                  .addvs(8, Set(1,7,9)) // 1.0.1.a → [1.0, 1.0.1, 1.1]
                  .addvs(9, Set(1))     // 1.1     → [1.0]
        assertEq(p.content.reqs.useCases.stepFlow.forwards, e)
      }

      "hardDelete" - {
        def test(id: UseCaseStepId): Unit =
          assertFail("found")(UseCaseStepRestore(id))(InitialEvents(create3 :+ UseCaseStepDelete(id): _*), implicitly)

        "single" - test(8)
        "subtree" - test(7)
      }

      "softDeleteWhen" - {
        def test(es: ActiveEvent*): Unit =
          testSteps(create3 ++ es :+ UseCaseStepDelete(7): _*)("0")()

        "hasText"        - test(UseCaseStepUpdate(7, ^.Title("asd")))
        "hasFlowIn"      - test(UseCaseStepUpdate(7, ^.FlowIn(nesd()(1))))
        "hasFlowOut"     - test(UseCaseStepUpdate(1, ^.FlowOut(nesd()(7))))
        "refFromUCS"     - test(UseCaseStepUpdate(8, ^.Title(UCST(UCST.UseCaseStepRef(7)))))
        "refFromReq"     - test(createGR(2), GenericReqTitleSet(2, Text.GenericReqTitle(Text.GenericReqTitle.UseCaseStepRef(7))))
        "refFromRCG"     - test(createRCG(1, RCG1_code, Text.CodeGroupTitle(Text.CodeGroupTitle.UseCaseStepRef(7))))
        "childInUseLive" - test(UseCaseStepUpdate(8, ^.FlowIn(nesd()(1))))
        "childInUseDead" - test(UseCaseStepUpdate(8, ^.FlowIn(nesd()(1))), UseCaseStepDelete(8))
      }
    }

    // 'restoreUseCaseStep - tested in EventPropTests

    "updateUseCaseStep" - {
      "title" - {
        "ok" - {
          val p = _assertPass(emptyUC1, addStepTo1, setStepTitle4)
          assertEq(p.content.reqs.useCases.imap.need(1).stepsNA.tree.findValue(_.id.value ==* 4).get.titleExplicitly, someStepText)
        }
        "stepNotFound" - assertFail("found")(setStepTitle4)
        "ucIsDead"     - assertFail("dead")(emptyUC1, addStepTo1, delUC1, setStepTitle4)
      }
      "flow" - {
        def nesd(remove: UseCaseStepId*)(add: UseCaseStepId*) = UnsafeTypes.nesd(remove: _*)(add: _*)
        "ok" - {
          val p = _assertPass(emptyUC1,
            UseCaseStepCreate(2, 1, NCAC, ∅),
            UseCaseStepCreate(3, 1, EC, ∅),
            UseCaseStepCreate(4, 1, EC, ∅),
            UseCaseStepCreate(5, 1, NCAC, ∅),
            UseCaseStepUpdate(4, ^.FlowOut(nesd()(1, 2, 4))),                       //               4→[1,2,4]
            UseCaseStepUpdate(4, ^.FlowOut(nesd(1, 4)())),                          //               4→[2]
            UseCaseStepUpdate(3, ^.FlowIn(nesd()(1, 2, 4))),                        // 1→[3], 2→[3], 4→[2,3]
            UseCaseStepUpdate(3, ^.FlowIn(nesd(2)())),                              // 1→[3],        4→[2,3]
            UseCaseStepUpdate(5, ^.nev(^.FlowIn(nesd()(1)),^.FlowOut(nesd()(2))) )) // 1→[3,5],      4→[2,3], 5→[2]
          val e = UseCases.StepFlow.emptyUniDir.addPairs(
            1 -> 3,
            1 -> 5,
            4 -> 2,
            4 -> 3,
            5 -> 2)
          assertEq(p.content.reqs.useCases.stepFlow.forwards, e)
        }
        "subjStepNotFound" - assertFail("not found")(emptyUC1, UseCaseStepUpdate(9, ^.FlowIn (nesd()(1))))
        "iAddStepNotFound" - assertFail("not found")(emptyUC1, UseCaseStepUpdate(1, ^.FlowIn (nesd()(9))))
        "iDelStepNotFound" - assertFail("not found")(emptyUC1, UseCaseStepUpdate(1, ^.FlowIn (nesd(9)())))
        "oAddStepNotFound" - assertFail("not found")(emptyUC1, UseCaseStepUpdate(1, ^.FlowOut(nesd()(9))))
        "oDelStepNotFound" - assertFail("not found")(emptyUC1, UseCaseStepUpdate(1, ^.FlowOut(nesd(9)())))
        // 'oDelStepIsNoop
        // 'iDelStepIsNoop
      }
    }
  }
}
