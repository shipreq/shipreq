package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import scalaz.\&/
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.text.Text
import ApplyEventTestFns._
import AutoNES._
import ContentEventTestHelp._
import Event._
import MTrie.Ops
import Text.{GenericReqTitle => GRT, CustomTextField => CTF, InlineIssueDesc => IID, CodeGroupTitle}

// TODO Test atom validity in all events that accept text

/**
 * Events that:
 * - apply to all kinds of requirements.
 * - pertain to ReqCodes.
 */
object ContentEventTest extends TestSuite {
  import GenericReqGD._

  val someCTF1: CTF.NonEmptyText =
    NonEmptyVector(CTF.Literal("hi!"), CTF.blankLine, CTF.Literal("bye."))

//  val someCTF2: CTF.OptionalText =
//    Vector(CTF.Literal("hi again!"), CTF.blankLine, CTF.Literal("bye again."))

  implicit val init = testHelpInit

  class ScriptTester(namePrefix: String) extends EventTester {
    makeName = (i, e) => s"Step $namePrefix.$i (${e.getClass.getSimpleName})"

    def fmtRCs(rc: ReqCodes): Set[String] =
      rc.trie.cataV(Set.empty[String]) { (q, p, d) =>
        var n = Set.empty[String]
        def add(typ: String, id: ReqCodeId, tgt: AnyRef) = {
          val t = Option(tgt) match {
            case Some(x: ReqId)        => s"Req(#${x.value.toChar.toString})"
            case Some(g: CodeGroup) => "Grp"
            case None                  => ""
            case Some(_)               => ???
          }
          n += s"$typ[#${id.value}$t]"
        }
        d match {
          case a: ReqCode.ActiveReq   => add("AD", a.id, a.reqId)
          case a: ReqCode.ActiveGroup => add("AD", a.id, a.group)
          case _: ReqCode.Inactive    => ()
        }
        for {(req, ids) <- d.reqInactive.m; id <- ids} add("RR", id, req)
        for (g <- d.deadGroup) add("RG", g.id, null)
        q ++ n.map(p.reduceMapLeft1(_.value)(_ + "." + _) + ": " + _)
      }

    def test(e: Event) = new Ah(e)
    class Ah(e: Event) {
      def apply(expected: Set[String], more: String*): Unit = apply((expected ++ more).toSeq: _*)
      def apply(expected: String*): Unit =
        ScriptTester.this(e) { name =>
          val act: Set[String] = fmtRCs(p.content.reqCodes)
          val exp: Set[String] = expected.map(_.replaceFirst(" +:", ":")).toSet
          if (act != exp) {
            val same = act & exp
//            def norm(s: Set[String]) = same.toVector.sorted ++ (s &~ same).toVector.sorted
            def norm(s: Set[String]) = (s &~ same).toVector.sorted
            assertEq(name, norm(act), norm(exp))
          }
        }
    }
  }

  val reqA = GenericReqId(97)
  val patchA = PatchReqCodeB(reqA)

  val reqB = GenericReqId(98)
  val patchB = PatchReqCodeB(reqB)

  val reqC = GenericReqId(99)

  def contentIds(reqIds: ReqId*)(reqCodeIds: ReqCodeId*): NonEmptySet[ReqId] \&/ NonEmptySet[ReqCodeId] =
    (NonEmptySet.option(reqIds.toSet), NonEmptySet.option(reqCodeIds.toSet)) match {
      case (Some(a), None)    => \&/.This(a)
      case (None,    Some(b)) => \&/.That(b)
      case (Some(a), Some(b)) => \&/.Both(a, b)
      case (None,    None)    => sys.error("At least 1 ID required.")
    }

  val createRefToCode3 = GenericReqCreate(500, mf, nev(
    Title(NonEmptyVector(GRT.Literal("Ref to #3: "), GRT.CodeRef(3)))))

  // As above but hides the ref in an IssueDesc
  val createRefToCode3I = GenericReqCreate(500, mf, nev(
    Title(NonEmptyVector(GRT.Issue(issueType1, Vector(
      IID.Literal("Ref to #3: "), IID.CodeRef(3)))))))

  val delA              = delGR(reqA)
  val delB              = delGR(reqB)
  val restoreA          = restoreGR(reqA)
  val restoreCode3From1 = patchCodes(1, restore = Set(3))
  val removeCode3From1  = patchCodes(1, remove = Set(3))

  override def tests = Tests {

    'createCodeGroup {
      'badId          - assertBadIdsRejected(createRCG(_, "hi"))
      'badCode        - assertFail("code")  (createRCG(1, "!!"))
      'codeInCaps     - assertFail("code")  (createRCG(1, "NO"))
      'idInUseByGR    - assertFail("")      (createGR(9, codes = Set(1 -> "a"))      , createRCG(1, "b"))
      'idInUseByUC    - assertFail("")      (createUC(9.UC, 9, codes = Set(1 -> "a")), createRCG(1, "b"))
      'idInUseByRCG   - assertFail("")      (createRCG(1, "a")                       , createRCG(1, "b"))
      'codeInUseByGR  - assertFail("in use")(createGR(9, codes = Set(1 -> "a"))      , createRCG(2, "a"))
      'codeInUseByUC  - assertFail("in use")(createUC(9.UC, 9, codes = Set(1 -> "a")), createRCG(2, "a"))
      'codeInUseByRCG - assertFail("in use")(createRCG(1, "a")                       , createRCG(2, "a"))
      'replaceLast    - {
        // Adding a new RCG should clear out .lastGroup
        val p = _assertPass(createRCG(1, "abc.def", "old"), delRCG1, createRCG(2, "abc.def", "new"))
        val d = assertSoleReqCode(p, "abc.def")
        assertEq(d, ReqCode.ActiveGroup(LiveCodeGroup(2, "new"), ReqCode.emptyReqInactive))
      }
    }

    'updateCodeGroup {
      'title {
        import CodeGroupGD._
        val t = Vector(CodeGroupTitle.Literal("hi there"))
        val p = _assertPass(createRCG(1, "a"), CodeGroupUpdate(1, nev(Title(t))))
        assertEq(p.content.reqCodes.groups.head.title, t)
      }

      'code {
        import CodeGroupGD._
        val p = _assertPass(createRCG(1, "hehe.grr", "Ze Title"), CodeGroupUpdate(1, nev(Code("fine.then"))))
        val d = assertSoleReqCode(p, "fine.then")
        assertEq(d, ReqCode.ActiveGroup(LiveCodeGroup(1, "Ze Title"), ReqCode.emptyReqInactive))
      }

      'badCode    - assertFail("code")     (createRCG(1, "a"), updateRCGCode(1, "!!"))
      'codeInCaps - assertFail("code")     (createRCG(1, "a"), updateRCGCode(1, "NO"))
      'idNotFound - assertFail("not found")(updateRCGCode(666, "new"))
      'idIsReq    - assertFail("group")    (createGR(1, codes = Set(1 -> "a")), updateRCGCode(1, "b"))

      'tgtCodeInUseByReq -
        assertFail("in use")(createRCG(1, "old"), createGR(2, codes = Set(3 -> "new")), updateRCGCode(1, "new"))

      'tgtCodeInUseByRCG -
        assertFail("in use")(createRCG(1, "old"), createRCG(2, "new"), updateRCGCode(1, "new"))

      // TODO Need a test here similar to createCodeGroup.replaceLast?
    }

    'patchCodes {
      // positive tests are in the script tests below

      'reqIdNotFound     - assertFail("")(patchA(add = Set(1 -> "mm")))
      'reqDead           - assertFail("live")(emptyGR1, delGR1, patchCodes(1, add = Set(5 -> "yay")))
      'addcodeSym        - assertFail("")(emptyGR1, patchCodes(1, add = Set(7 -> "!!")))
      'addcodeCaps       - assertFail("")(emptyGR1, patchCodes(1, add = Set(7 -> "NO")))
      'addIdInUseByGR    - assertFail("")(emptyGR1, createGR(2, codes = Set(3 -> "x")), patchCodes(1, add = Set(3 -> "y")))
      'addIdInUseByRCG   - assertFail("")(emptyGR1, createRCG(3, "x"),                  patchCodes(1, add = Set(3 -> "y")))
      'addCodeInUseByGR  - assertFail("")(emptyGR1, createGR(2, codes = Set(3 -> "x")), patchCodes(1, add = Set(9 -> "x")))
      'addCodeInUseByRCG - assertFail("")(emptyGR1, createRCG(3, "x"),                  patchCodes(1, add = Set(9 -> "x")))
      'removeNotFound    - assertFail("")(emptyGR1,                                     removeCode3From1)
      'removeOtherReqs   - assertFail("")(emptyGR1, createGR(2, codes = Set(3 -> "x")), removeCode3From1)
      'removeGrps        - assertFail("")(emptyGR1, createRCG(3, "x"),                  removeCode3From1)
      'removeDeadOwn     - assertFail("")(createGR(1, codes = Set(3 -> "x")), removeCode3From1, removeCode3From1)

      'restoreNotFound      - assertFail("")(emptyGR1,                                     restoreCode3From1)
      'restoreLiveOwn       - assertFail("")(createGR(1, codes = Set(3 -> "x")),         restoreCode3From1)
      'restoreLiveOtherReqs - assertFail("")(emptyGR1, createGR(2, codes = Set(3 -> "x")), restoreCode3From1)
      'restoreLiveGrps      - assertFail("")(emptyGR1, createRCG(3, "x"),                  restoreCode3From1)

      'restoreDeadOtherReqs -
        assertFail("")(emptyGR1, createGR(2, codes = Set(3 -> "x")), createRefToCode3, patchCodes(2, remove = Set(3)), restoreCode3From1)

      'restoreDeadGrps -
        assertFail("")(emptyGR1, createRCG3, createRefToCode3, delRCG3, restoreCode3From1)

      // fail when same ID in remove/restore
      // fail when same ID in add/restore
    }

    // See Design/req_codes.ods
    'reqCodes {
      'script1 {
        val tester = new ScriptTester("1")
        import tester.test

        // 1.1: Create RCG
        test(createRCG(3, "a.b.c"))("a.b.c: AD[#3Grp]")

        // 1.2: Create a CodeRef to #3
        test(createRefToCode3I)("a.b.c: AD[#3Grp]")

        // 1.3: Rename 1→1'
        test(updateRCGCode(3, "a.x"))("a.x: AD[#3Grp]")

        // 1.4: Delete RCG
        test(delRCG(3))("a.x: RG[#3]")

        // 1.5: Restore RCG
        test(restoreRCG3)("a.x: AD[#3Grp]")

        // 1.6: Delete RCG
        test(delRCG(3))("a.x: RG[#3]")

        // 1.7: Create RCᵣ
        val createA = GenericReqCreate(reqA, mf, nev(Codes(4 -> "a.x")))
        test(createA)("a.x: AD[#4Req(#a)]", "a.x: RG[#3]")

        // 1.8: Rename RCᵣ
        test(patchA(remove = Set(4), add = Set(4 -> "y")))("y: AD[#4Req(#a)]", "a.x: RG[#3]")

        // 1.9: Restore RCG
        test(restoreRCG3)("y: AD[#4Req(#a)]", "a.x: AD[#3Grp]")
      }

      'script2 {
        val tester = new ScriptTester("2")
        import tester.test

        // 2.1: Create RCᵣ ref
        val createA = GenericReqCreate(reqA, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Ref to self: "), GRT.CodeRef(1))),
          Codes(1 -> "a.b.c")))
        test(createA)("a.b.c: AD[#1Req(#a)]")

        // 2.2: Rename 1→1'
        test(patchA(remove = Set(1), add = Set(1 -> "y.y.z")))("y.y.z: AD[#1Req(#a)]")

        // 2.3: Delete RCᵣ
        test(delA)("y.y.z: RR[#1Req(#a)]")

        // 2.4: Restore RCᵣ
        test(restoreA)("y.y.z: AD[#1Req(#a)]")

        // 2.5: Rename 1→n+1
        test(patchA(add = Set(2 -> "n.a")))("n.a: AD[#2Req(#a)]", "y.y.z: AD[#1Req(#a)]")

        // 2.6: Rename n+1→1
        test(patchA(remove = Set(2)))("y.y.z: AD[#1Req(#a)]")

        // 2.7: Rename 1→n
        val expect27 = Seq("n.b: AD[#3Req(#a)]", "n.c: AD[#4Req(#a)]", "y.y.z: RR[#1Req(#a)]")
        test(patchA(remove = Set(1), add = Set(3 -> "n.b", 4 -> "n.c")))(expect27: _*)

        // 2.8: Create a CodeRef to #3
        test(createRefToCode3)(expect27: _*)

        // 2.9: Rename n→n+1
        test(patchA(restore = Set(1)))(
          "n.b: AD[#3Req(#a)]", "n.c: AD[#4Req(#a)]", "y.y.z: AD[#1Req(#a)]")

        // 2.10: Rename n+1→1'
        test(patchA(remove = Set(1,3,4), add = patchRcAdd0.addvs("aaa", Set(1, 3))))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 2.11: Rename 1→n+1
        test(patchA(add = Set(50 -> "n.d")))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.d: AD[#50Req(#a)]")

        // 2.12: Rename n+1→n+1
        test(patchA(remove = Set(50), add = Set(5 -> "n.ee", 6 -> "n.ef")))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.ee: AD[#5Req(#a)]", "n.ef: AD[#6Req(#a)]")

        // 2.13: Rename n+1→n
        test(patchA(remove = Set(1)))(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.ee: AD[#5Req(#a)]", "n.ef: AD[#6Req(#a)]")

        // 2.14: Rename n→1
        test(patchA(remove = Set(5, 6), restore = Set(1)))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 2.15: Rename 1→n+1
        test(patchA(add = Set(7 -> "n.f")))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.f: AD[#7Req(#a)]")

        // 2.16: Rename n+1→0
        test(patchA(remove = Set(1, 7)))(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 2.17: Restore RCᵣ + n
        test(patchA(restore = Set(1), add = Set(8 -> "n.h")))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.h: AD[#8Req(#a)]")

        // 2.18: Delete RCᵣ + n
        val delA_state = Set("aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.h: RR[#8Req(#a)]")
        test(delA)(delA_state)

        // 2.19: Create RCG
        test(createRCG(9, "aaa"))(delA_state + "aaa: AD[#9Grp]")

        // 2.20: Rename RCG
        test(updateRCGCode(9, "ggg"))(delA_state + "ggg: AD[#9Grp]")

        // 2.21: Create RCᵣ #b
        test(GenericReqCreate(98, mf, nev(Codes(10 -> "aaa"))))(
          delA_state, "ggg: AD[#9Grp]", "aaa: AD[#10Req(#b)]")

        // 2.22: Rename RCᵣ #b
        test(patchB(remove = Set(10), add = Set(10 -> "bbb")))(
          delA_state, "ggg: AD[#9Grp]", "bbb: AD[#10Req(#b)]")

        // 2.23: Restore RCᵣ
        test(restoreA)(
          "ggg: AD[#9Grp]", "bbb: AD[#10Req(#b)]", "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "n.h: AD[#8Req(#a)]")
      }

      // Tests req restoration with reqcode conflict resolution
      'script3a {
        val tester = new ScriptTester("3a")
        import tester.test

        // 3a.1: Create req a
        test(createGR(reqA, codes = Set(1 -> "one", 3 -> "three")))(
          "one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]")

        // 3a.2: Create refs to it
        val refs = GenericReqCreate(500, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #1 and #3: "), GRT.CodeRef(3), GRT.CodeRef(1)))))
        test(refs)("one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]")

        // 3a.3: Delete req a
        test(delA)("one: RR[#1Req(#a)]", "three: RR[#3Req(#a)]")

        // 3a.4: Create req b - usurp the reqcode [three]
        test(createGR(reqB, codes = Set(9 -> "three", 4 -> "four")))(
          "one: RR[#1Req(#a)]", "three: RR[#3Req(#a)]", "three: AD[#9Req(#b)]", "four: AD[#4Req(#b)]")

        // 3a.5: Restore req a
        test(restoreA)("one: AD[#1Req(#a)]", "three_2: AD[#3Req(#a)]", "three: AD[#9Req(#b)]", "four: AD[#4Req(#b)]")

        // 3a.6: Delete req b
        val deadBs = Set("three: RR[#9Req(#b)]", "four: RR[#4Req(#b)]")
        test(delB)(deadBs, "one: AD[#1Req(#a)]", "three_2: AD[#3Req(#a)]")

        // 3a.7: Delete req a
        test(delA)(deadBs, "one: RR[#1Req(#a)]", "three_2: RR[#3Req(#a)]")

        // 3a.8: Create group - usurp the reqcode [one]
        test(createRCG(8, "one", "1"))(deadBs, "one: RR[#1Req(#a)]", "three_2: RR[#3Req(#a)]", "one: AD[#8Grp]")

        // 3a.9: Restore req a
        test(restoreA)(deadBs, "one_2: AD[#1Req(#a)]", "three_2: AD[#3Req(#a)]", "one: AD[#8Grp]")

        // 3a.10: Delete group
        test(delRCG(8))(deadBs, "one_2: AD[#1Req(#a)]", "three_2: AD[#3Req(#a)]", "one: RG[#8]")
      }

      // Tests req restoration with reqcode conflict resolution (including a ref-to-req being migrated)
      'script3b {
        val tester = new ScriptTester("3b")
        import tester.test

        // 3b.1: Create req a
        test(createGR(reqA, codes = Set(1 -> "one", 3 -> "three")))(
          "one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]")

        // 3b.2: Create refs to it
        val refs = GenericReqCreate(500, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #1 and #3: "), GRT.CodeRef(3), GRT.CodeRef(1)))))
        test(refs)("one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]")

        // 3b.3: Merge refs
        test(patchA(remove = Set(1, 3), add = patchRcAdd0.addvs("aaa", Set(1, 3))))(
          "aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 3b.4: Delete req a
        test(delA)("aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 3b.5: Usurp reqcode [aaa]
        test(createGR(reqB, codes = Set(7 -> "aaa")))(
          "aaa: AD[#7Req(#b)]", "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]")

        // 3b.6: Restore req a
        test(restoreA)("aaa: AD[#7Req(#b)]", "aaa_2: AD[#1Req(#a)]", "aaa_2: RR[#3Req(#a)]")
      }

      // This comes from [Design/req_codes.ods@logic #2]
      // It tests reqcodes upon req deletion & restoration, including without CodeRefs
      'script4 {
        val tester = new ScriptTester("4")
        import tester.test

        // 4.1: Create req a
        test(createGR(reqA, codes = Set(1 -> "one", 3 -> "three", 2 -> "other")))(
          "one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]", "other: AD[#2Req(#a)]")

        // 4.2: Create refs to some of it
        val refsA = GenericReqCreate(500, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #1 and #3: "), GRT.CodeRef(3), GRT.CodeRef(1)))))
        test(refsA)("one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]", "other: AD[#2Req(#a)]")

        // 4.3: Merge refs
        val origLiveA = Set("aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: AD[#2Req(#a)]")
        test(patchA(remove = Set(1, 3), add = patchRcAdd0.addvs("aaa", Set(1, 3))))(origLiveA)

        // 4.4: Delete req a
        def deleteReqA() = test(delA)("aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: RR[#2Req(#a)]")
        deleteReqA()

        // 4.5: Restore req a
        test(restoreA)(origLiveA)

        // 4.6: Delete req a
        deleteReqA()

        // 4.7: Create b - Usurp reqcodes
        test(createGR(reqB, codes = Set(4 -> "aaa", 5 -> "bbb", 6 -> "other")))(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: RR[#2Req(#a)]",
          "aaa: AD[#4Req(#b)]", "bbb: AD[#5Req(#b)]", "other: AD[#6Req(#b)]")

        // 4.8: Create refs to some of B
        val refsB = GenericReqCreate(501, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #4 and #5: "), GRT.CodeRef(4), GRT.CodeRef(5)))))
        test(refsB)(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: RR[#2Req(#a)]",
          "aaa: AD[#4Req(#b)]", "bbb: AD[#5Req(#b)]", "other: AD[#6Req(#b)]")

        // 4.9: Merge refs
        // Note: Even though #4 already = "aaa", it still needs to be in the remove/add set
        //       This ensures that the active ID is always the minimum ID.
        // TODO Ensure MakeEvent uses this logic ↕
        test(patchB(remove = Set(4, 5), add = patchRcAdd0.addvs("aaa", Set(4, 5))))(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: RR[#2Req(#a)]",
          "aaa: AD[#4Req(#b)]", "aaa: RR[#5Req(#b)]", "other: AD[#6Req(#b)]")

        // 4.10: Restore req a
        test(restoreA)(
          "aaa_2: AD[#1Req(#a)]", "aaa_2: RR[#3Req(#a)]", "other_2: AD[#2Req(#a)]",
          "aaa  : AD[#4Req(#b)]", "aaa  : RR[#5Req(#b)]", "other  : AD[#6Req(#b)]")
      }

      'autoConflictResolution {
        def maxMinus(minus: Int) = {
          val len = Grammar.reqCode.nodeLength.total.max - minus
          val cs = (0 until len).map(i => (i + 97).toChar).toArray
          String valueOf cs
        }
        def m0 = maxMinus(0)
        def m1 = maxMinus(1)
        def m2 = maxMinus(2)
        def m3 = maxMinus(3)
        def m4 = maxMinus(4)
        def test(conflicted: String, inUse: Set[String], expect: String): Unit = {
          _test(conflicted, inUse, expect)
          val f = "x.y." + (_: String)
          _test(f(conflicted), (inUse map f) + (f(expect) + ".ah"), f(expect))
        }
        def _test(conflicted: String, inUse: Set[String], expect: String): Unit = {
          val active = ReqCode.ActiveReq(0, 0, None, ReqCode.emptyReqInactive)
          val t = (inUse + conflicted).foldLeft(ReqCode.Trie.empty)(_.put(_, active))
          val actual = ApplyEventTestFns.apply.ReqCodeLogic.renameReqCodeToAvoidConflict(conflicted, t)
          assertEq[ReqCode.Value](actual, expect)
        }
        def suf(pre: String, to: Int, from: Int = 2): Set[String] =
          (from to to).toStream.map(pre + "_" + _).toSet
        'simpleEasy       - test("abc", ∅                                    , "abc_2")
        'simpleScan       - test("abc", suf("abc", 104)                      , "abc_105")
        'max              - test(m0,    ∅                                    , m2 + "_2")
        'max1Easy         - test(m1,    ∅                                    , m2 + "_2")
        'max1Scan         - test(m1,    suf(m2, 5)                           , m2 + "_6")
        'max2Easy         - test(m2,    ∅                                    , m2 + "_2")
        'max2Scan         - test(m2,    suf(m2, 8)                           , m2 + "_9")
        'max2Drop         - test(m2,    suf(m2, 9)                           , m3 + "_2")
        'max2DropDropScan - test(m2,    suf(m2, 9) | suf(m3, 99) | suf(m4, 4), m4 + "_5")
      }
    }

    'patchTags {
      def patch(id: ReqId)(remove: ApplicableTagId*)(add: ApplicableTagId*): ReqTagsPatch =
        NonEmpty(SetDiff(removed = remove.toSet, added = add.toSet))
          .map(ReqTagsPatch(id, _))
          .getOrElse(sys error "Empty set diff")

      'ok {
        var es = Vector[Event](emptyGR1)
        def test(remove: ApplicableTagId*)(add: ApplicableTagId*)(expect: ApplicableTagId*): Unit = {
          es :+= patch(1)(remove: _*)(add: _*)
          val p = _assertPass(es: _*)
          val a = p.content.reqTags(1)
          assertEq(a, expect.toSet)
        }
        test()(at1, at2)(at1, at2)
        test(at2)()(at1)
        test(at1)(at2)(at2)
        test(at2)()()
      }

      'reqIsDead      - assertFail("dead")(emptyGR1, delGR1, patch(1)()(at1))
      'reqNotFound    - assertFail("found")(patch(1)()(at1))
      'addBadTag      - assertFail("not found")(emptyGR1, patch(1)()(123))
      'removeBadTag   - assertFail("not found")(emptyGR1, patch(1)(123)())
      'addTagGroup    - assertFail("not found")(emptyGR1, patch(1)()(tg1.value.AT))
      'removeTagGroup - assertFail("not found")(emptyGR1, patch(1)(tg1.value.AT)())

      // 'removeMissingTag = nop
      // 'addExistingTag   = nop
    }

    'patchImps {
      def setdiff(remove: ReqId*)(add: ReqId*) =
        NonEmpty(SetDiff(removed = remove.toSet, added = add.toSet)).getOrElse(sys error "Empty set diff")
      def testFailure(msgFrag: String)(subj: ReqId, events: Event*)(remove: ReqId*)(add: ReqId*): Unit = {
        val sd = setdiff(remove: _*)(add: _*)
        assertFail(msgFrag)(events :+ ReqImplicationsPatch(subj, Backwards, sd): _*)
        assertFail(msgFrag)(events :+ ReqImplicationsPatch(subj, Forwards, sd): _*)
      }

      'ok {
        var es = Vector[Event](emptyGR1, impliedGR2, emptyGR3)
        def test(subj: ReqId, dir: Direction)(remove: ReqId*)(add: ReqId*)(expect: (ReqId, Set[ReqId])*): Unit = {
          val sd = setdiff(remove: _*)(add: _*)
          es :+= ReqImplicationsPatch(subj, dir, sd)
          val p = _assertPass(es: _*)
          val a = p.content.implications.forwards.m
          assertEq(a, expect.toMap)
        }
        implicit def ii(t: (Int, Int)): (ReqId, Set[ReqId]) = (t._1, Set(t._2))
        implicit def is(t: (Int, Set[Int])): (ReqId, Set[ReqId]) = (t._1, t._2.map(i => i: ReqId))

        // Start: 1 → 2, 3
        test(2, Forwards )( )(3)(1 -> 2, 2 -> 3)
        test(2, Backwards)(1)( )(2 -> 3)
        test(3, Backwards)(2)(1)(1 -> 3)
        test(2, Backwards)( )(1)(1 -> Set(2, 3))
        test(1, Forwards )(3)( )(1 -> 2)
        test(1, Forwards )(2)(3)(1 -> 3)
      }

      'reqNotFound - testFailure("found")(1, emptyGR3)()(3)
      'reqIsDead   - testFailure("dead") (1, emptyGR1, emptyGR3, delGR1)()(3)
      'impNotFound - testFailure("found")(1, emptyGR1)()(8)
      'impSelf     - testFailure("cycle")(1, emptyGR1)()(1)

      'impCycle {
        val es = Vector(emptyGR1, impliedGR2, GenericReqCreate(3, mf, nev(ImpSrcs(2))))
        assertFail("cycle")(es :+ ReqImplicationsPatch(3, Forwards, NonEmpty.force(SetDiff(Set.empty, Set(1)))): _*)
        assertFail("cycle")(es :+ ReqImplicationsPatch(1, Backwards, NonEmpty.force(SetDiff(Set.empty, Set(3)))): _*)
      }

      // 'removeMissingTag = nop
      // 'addExistingTag   = nop
    }

    'setCustomTextField {
      def e = ReqFieldCustomTextSet(1, cf1, someCTF1)
      'add {
        val p = _assertPass(emptyGR1, e)
        val d = p.content.reqText
        assertEq(d.size, 1)
        val m = d(cf1)
        assertEq(m.size, 1)
        assertEq(m(1), someCTF1)
      }
      'remove {
        val p = _assertPass(emptyGR1, e, ReqFieldCustomTextSet(1, cf1, ∅))
        val d = p.content.reqText
        assertEq(d.size, 0)
      }
      'reqNotFound   - assertFail("found")(e)
      'reqIsDead     - assertFail("dead") (emptyGR1, delGR1, e)
      'fieldNotFound - assertFail("found")(emptyGR1, ReqFieldCustomTextSet(1, 321, someCTF1))
      'fieldDead     - assertFail("dead") (emptyGR1, FieldCustomDelete(cf1), e)
      // TODO test not applicable to target reqtype
    }

    'deleteRestore {

      'deleteReq {
        'notFound - assertFail("not found")(delGR1)
        'twice    - assertFail("is dead")(createGR(1), delGR1, delGR1)
        'ok       - assertPass(createGR(1), delGR1)
      }

      'deleteRCG {
        'notFound - assertFail("not found")(delRCG1)
        'dead     - assertFail("")(createRCG1, delRCG1, delRCG1)
        'emptyTitleNoRefs - {
          val p = _assertPass(createRCG(1, "abc.def"), delRCG1)
          // It's more work to check for text references to decide whether or not to retain empty RCGs. Just keep em.
          // assertEq("No CodeRefs & no title = no need to retain anything.", p.content.reqCodes.trie.isEmpty, true)
          val d = assertSoleReqCode(p, "abc.def")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(1, ∅))))
        }
        'emptyTitleWithRefs - {
          val p = _assertPass(emptyGR1, createRCG(3, "qwe.zxc"), createRefToCode3, delRCG3)
          val d = assertSoleReqCode(p, "qwe.zxc")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(3, ∅))))
        }
        'nonEmptyTitle - {
          val p = _assertPass(createRCG(1, "abc.def", "hehe"), delRCG1)
          val d = assertSoleReqCode(p, "abc.def")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(1, "hehe"))))
        }
      }

      'restoreRCG {
        'notFound - assertFail("not found")(restoreRCG1)
        'live     - assertFail("is already live")(createRCG1, restoreRCG1)
        'live2    - assertFail("is already live")(createRCG1, delRCG1, restoreRCG1, restoreRCG1)
        'ok       - assertPass(createRCG1, delRCG1, restoreRCG1)
        'ok2      - assertPass(createRCG1, delRCG1, restoreRCG1, delRCG1, restoreRCG1)
      }

      'reason {
        val t = new EventTester()
        def p = t.p
        type R = String
        def dead: R = "dead"
        def live: R = "live"
        def none: R = "none"
        def test(e: Event)(reqA: R, rcg1: R)(reqB: R, rcg2: R): Unit =
          t(e)(name => {
            def fmt(txt: Option[Text.DeletionReason.NonEmptyText]) =
              txt.fold(none)(_.mkString("").replaceAll("Literal\\((.*?)\\)", "$1"))

            def req(id: GenericReqId): R =
              p.content.reqs.genericReqs.need(id) match {
                case r if r.live(p.config.reqTypes) is Live => live
                case _                                      => fmt(p.content.deletionReasons getLatest id)
              }

            // RCGs don't get reasons
            def rcg(c: ReqCode.Value): R =
              p.content.reqCodes(c) match {
                case _: ReqCode.ActiveGroup     => live
                case d if d.deadGroup.isDefined => dead
                case _                          => none
              }

            val actualL = req(ContentEventTest.reqA) :: rcg(RCG1_code) ::
                          req(ContentEventTest.reqB) :: rcg(RCG2_code) ::
                          req(ContentEventTest.reqC) :: rcg(RCG3_code) :: Nil
            val expectL = reqA :: rcg1 :: reqB :: rcg2 :: live :: live :: Nil
            val List(a,e) = List(actualL, expectL).map(_ mkString "  ")
            assertEq(name, a, e)
          })

        // Create A,B,C,G1,G2,G3
        t.justApply(createGR(reqA), createGR(reqB), createGR(reqC), createRCG1, createRCG2)
        test(createRCG3)(live, live)(live, live)

        // Delete A,B,G1,G2 with reason
        val dr1 = "dr#1"
        test(ReqsDelete(NonEmptySet(reqA, reqB), Set(1, 2), dr1))(dr1, dead)(dr1, dead)

        // Restore B,G2
        test(ContentRestore(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 - no reason
        test(ReqsDelete(NonEmptySet(reqB), Set(2), ∅))(dr1, dead)(none, dead)

        // Restore B,G2
        test(ContentRestore(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 - no reason
        test(ReqsDelete(NonEmptySet(reqB), Set(2), ∅))(dr1, dead)(none, dead)

        // Restore B,G2
        test(ContentRestore(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 with reason
        val dr2 = "dr#2"
        test(ReqsDelete(NonEmptySet(reqB), Set(2), dr2))(dr1, dead)(dr2, dead)

        // Restore A,B,G1,G2
        test(ContentRestore(Set(reqA, reqB), Set(1, 2)))(live, live)(live, live)

        // Delete A,B with reason
        val dr3 = "dr#3"
        test(ReqsDelete(NonEmptySet(reqA, reqB), ∅, dr3))(dr3, live)(dr3, live)
      }
    }

  }
}
