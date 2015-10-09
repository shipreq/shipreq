package shipreq.webapp.base.event

import nyaya.util.Multimap
import scalaz.{\&/, -\/, \/-}
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import ApplyEventTestFns._
import MTrie.Ops
import Text.{GenericReqTitle => GRT, CustomTextField => CTF, InlineIssueDesc => IID, ReqCodeGroupTitle}
import UnivEq.Implicits._

case class ReqFull(req      : GenericReq,
                   tags     : Set[ApplicableTagId],
                   impliedBy: Set[ReqId],
                   implies  : Set[ReqId],
                   reqCodes : Set[ReqCode.Value])

object ReqFull {
  implicit def equality: UnivEq[ReqFull] = UnivEq.derive

  def extract(p: Project, id: GenericReqId): Option[ReqFull] = {
    val r = p.reqs.req(id) match {case x: GenericReq => x}
    val tags      = p.reqTags(id)
    val impliedBy = p.implications.tgtToSrc(id)
    val implies   = p.implications.srcToTgt(id)
    val reqCodes  = p.reqCodes.activeReqCodesByReqId(id)
    ReqFull(r, tags, impliedBy, implies, reqCodes)
  }
}

// TODO Test atom validity in all events that accept text

object ContentEventTest extends TestSuite {

  implicit def rciav(t: (Int, String)) = ReqCode.IdAndValue(t._1, t._2)

  implicit def setLikePatchAdd1(s: Set[(Int, String)]): Multimap[ReqCode.Value, Set, ReqCodeId] =
    setLikePatchAdd(s map rciav)

  implicit def setLikePatchAdd(s: Set[ReqCode.IdAndValue]): Multimap[ReqCode.Value, Set, ReqCodeId] =
    Multimap(s.toList.map(iv => iv.value -> Set(iv.id)).toMap)

  val mm = Multimap.empty[ReqCode.Value, Set, ReqCodeId]

  val mf: CustomReqTypeId = 100
  val fr: CustomReqTypeId = 101
  val (createMF, createFR) = {
    import CustomReqTypeGD._
    ( CreateCustomReqType(mf, nev(Mnemonic("MF"), Name("MajFea"), Imp(false)))
    , CreateCustomReqType(fr, nev(Mnemonic("FR"), Name("FunReq"), Imp(false)))
    )
  }

  val at1: ApplicableTagId = 11
  val at2: ApplicableTagId = 12
  val (createAT1, createAT2) = {
    import ApplicableTagGD._
    ( CreateApplicableTag(at1, nev(Name("AT #1"), Desc(None), Key("at-one")))
    , CreateApplicableTag(at2, nev(Name("AT #2"), Desc(None), Key("at-two")))
    )
  }

  val tg1: TagGroupId = 20
  val createTG1 = {
    import TagGroupGD._
    CreateTagGroup(tg1, nev(Name("TG #1"), Desc(None), MutexChildren(false)))
  }

  implicit class ProjectExt(private val p: Project) extends AnyVal {
    def @@(id: GenericReqId) = ReqFull.extract(p, id)
  }

  import CreateGenericReqGD._

  val empty1 = CreateGenericReq(1, mf, emptyValues)
  val implied2 = CreateGenericReq(2, mf, nev(ImpSrcs(NonEmptySet(empty1.id))))
  val empty3 = CreateGenericReq(3, mf, emptyValues)

  val someGRTitle: GRT.OptionalText =
    Vector(GRT.Literal("Look at "), GRT.WebAddress("https://google.com"))

  val setGRT1 = SetGenericReqTitle(1, someGRTitle)

  val createCTF1 = {
    import CustomTextFieldGD._
    CreateCustomTextField(80, nev(Name("asdf"), Key("qwer"), Mandatory(true), ReqTypes(allReqTypes)))
  }
  val cf1 = createCTF1.id

  val someCTF1: CTF.NonEmptyText =
    NonEmptyVector(CTF.Literal("hi!"), CTF.blankLine, CTF.Literal("bye."))

//  val someCTF2: CTF.OptionalText =
//    Vector(CTF.Literal("hi again!"), CTF.blankLine, CTF.Literal("bye again."))

  val createIssueType1 = {
    import CustomIssueTypeGD._
    CreateCustomIssueType(1, nev(Key("TBD"), Desc(None)))
  }
  val issueType1 = createIssueType1.id

  implicit val init = InitialEvents(createIssueType1, createMF, createFR, createAT1, createAT2, createTG1, createCTF1)

  def assertReq(p: Project, id: GenericReqId)(req      : GenericReq,
                                              tags     : Set[ApplicableTagId] = UnivEq.emptySet,
                                              impliedBy: Set[ReqId]           = UnivEq.emptySet,
                                              implies  : Set[ReqId]           = UnivEq.emptySet,
                                              reqCodes : Set[ReqCode.Value]   = UnivEq.emptySet): Unit =
    assertEq(p @@ id, Some(ReqFull(req, tags, impliedBy, implies, reqCodes)))

  def assertSoleReqCode(p: Project, code: ReqCode.Value): ReqCode.Data = {
    val v = p.reqCodes.trie.flatStream.toVector
    assertEq("Trie size", v.size, 1)
    assertEq("Sole req code", v.head._1, code)
    v.head._2
  }

  def createGR(id: GenericReqId, rt: CustomReqTypeId = mf, codes: Set[ReqCode.IdAndValue] = ∅, title: GRT.OptionalText = ∅) = {
    var vs = emptyValues
    NonEmptySet.maybe(codes, ())(vs += ReqCodes(_))
    NonEmptyVector.maybe(title, ())(vs += Title(_))
    CreateGenericReq(id, rt, vs)
  }

  def createRCG(id: ReqCodeId, code: ReqCode.Value, title: ReqCodeGroupTitle.OptionalText = ∅) = {
    import ReqCodeGroupGD._
    CreateReqCodeGroup(id, nev(Code(code), Title(title)))
  }

  def updateRCGCode(id: ReqCodeId, code: ReqCode.Value) = {
    import ReqCodeGroupGD._
    UpdateReqCodeGroup(id, nev(Code(code)))
  }

  class ScriptTester(namePrefix: String) extends EventTester {
    makeName = (i, e) => s"Step $namePrefix.$i (${e.getClass.getSimpleName})"

    def fmtRCs(rc: ReqCodes): Set[String] =
      rc.trie.cataV(Set.empty[String]) { (q, p, d) =>
        var n = Set.empty[String]
        def add(typ: String, id: ReqCodeId, tgt: AnyRef) = {
          val t = Option(tgt) match {
            case Some(x: ReqId)        => s"Req(#${x.value.toChar.toString})"
            case Some(g: ReqCodeGroup) => "Grp"
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
          val act: Set[String] = fmtRCs(p.reqCodes)
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
  def patchA(remove: Set[ReqCodeId] = Set.empty,
             restore: Set[ReqCodeId] = Set.empty,
             add: Multimap[ReqCode.Value, Set, ReqCodeId] = mm) =
    PatchReqCodes(reqA, remove = remove, restore = restore, add)

  val reqB = GenericReqId(98)
  def patchB(remove: Set[ReqCodeId] = Set.empty,
             restore: Set[ReqCodeId] = Set.empty,
             add: Multimap[ReqCode.Value, Set, ReqCodeId] = mm) =
    PatchReqCodes(reqB, remove = remove, restore = restore, add)

  val reqC = GenericReqId(99)

  def patchReq(id: GenericReqId, remove: Set[ReqCodeId] = Set.empty,
             restore: Set[ReqCodeId] = Set.empty,
             add: Multimap[ReqCode.Value, Set, ReqCodeId] = mm) =
    PatchReqCodes(id: GenericReqId, remove = remove, restore = restore, add)

  def contentIds(reqIds: ReqId*)(reqCodeIds: ReqCodeId*): NonEmptySet[ReqId] \&/ NonEmptySet[ReqCodeId] =
    (NonEmptySet.option(reqIds.toSet), NonEmptySet.option(reqCodeIds.toSet)) match {
      case (Some(a), None)    => \&/.This(a)
      case (None,    Some(b)) => \&/.That(b)
      case (Some(a), Some(b)) => \&/.Both(a, b)
      case (None,    None)    => sys.error("At least 1 ID required.")
    }

  def delRCG(id: ReqCodeId): DeleteReqCodeGroups =
    DeleteReqCodeGroups(NonEmptySet(id))

  def delReq(id: ReqId): DeleteReqs =
    DeleteReqs(NonEmptySet(id), ∅, ∅)

  def restoreRCG(id: ReqCodeId): RestoreContent =
    RestoreContent(∅, Set(id))

  def restoreReq(id: ReqId): RestoreContent =
    RestoreContent(Set(id), ∅)

  val RCG1_code   = "abc.def": ReqCode.Value
  val createRCG1  = createRCG(1, RCG1_code, "hehe")
  val delRCG1     = delRCG(1)
  val restoreRCG1 = restoreRCG(1)

  val RCG2_code   = "abc.x.why": ReqCode.Value
  val createRCG2  = createRCG(2, RCG2_code, "OMG #2")
  val delRCG2     = delRCG(2)
  val restoreRCG2 = restoreRCG(2)

  val RCG3_code   = "abc.zed": ReqCode.Value
  val createRCG3  = createRCG(3, RCG3_code, "group 3 mate")
  val delRCG3     = delRCG(3)
  val restoreRCG3 = restoreRCG(3)

  val createRefToCode3 = CreateGenericReq(500, mf, nev(
    Title(NonEmptyVector(GRT.Literal("Ref to #3: "), GRT.CodeRef(3)))))

  // As above but hides the ref in an IssueDesc
  val createRefToCode3I = CreateGenericReq(500, mf, nev(
    Title(NonEmptyVector(GRT.Issue(issueType1, Vector(
      IID.Literal("Ref to #3: "), IID.CodeRef(3)))))))

  val restoreReq1 = restoreReq(1)
  val del1 = delReq(1)
  val delA = delReq(reqA)
  val delB = delReq(reqB)
  val restoreA = restoreReq(reqA)
  val restoreCode3From1 = patchReq(1, restore = Set(3))
  val removeCode3From1 = patchReq(1, remove = Set(3))

  override def tests = TestSuite {

    'createGenericReq {
      'empty {
        val p = _assertPass(empty1)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live))
      }

      'title {
        val t = NonEmptyVector(GRT.Literal("cool"))
        val p = _assertPass(empty1.copy(vs = nev(Title(t))))
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), t.whole, Live))
      }

      'tags {
        val t = NonEmptySet(at1)
        val p = _assertPass(empty1.copy(vs = nev(Tags(t))))
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), tags = t.whole)
      }

      'impSrc {
        val v = NonEmptySet[ReqId](empty1.id)
        val p = _assertPass(empty1, CreateGenericReq(5, mf, nev(ImpSrcs(v))))
        assertReq(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), impliedBy = v.whole)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), implies = Set(5))
      }

      'impTgt {
        val v = NonEmptySet[ReqId](empty1.id)
        val p = _assertPass(empty1, CreateGenericReq(5, mf, nev(ImpTgts(v))))
        assertReq(p, 5)(GenericReq(5, PubidT(mf, 2), ∅, Live), implies = v.whole)
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), impliedBy = Set(5))
      }

      'reqCodes {
        val rcs = NonEmptySet[ReqCode.IdAndValue](7 -> "a.b.c", 8 -> "d")
        val p = _assertPass(empty1.copy(vs = nev(ReqCodes(rcs))))
        assertReq(p, 1)(GenericReq(1, PubidT(mf, 1), ∅, Live), reqCodes = rcs.whole.map(_.value))
        assertEq(p.reqCodes.reqCodesById, rcs.whole.map(_.toTupleIV).toMap)
      }

      'badId           - List(0, -1).foreach(i => assertFail("id")(empty1.copy(id = i)))
      'idInUse         - assertFail("exists")(empty1, empty1)
      'reqTypeNotFound - assertFail("found")(empty1.copy(rt = 666))
      'reqTypeDead     - assertFail("dead")(DeleteCustomReqType(mf, Delete), empty1)
      'tagNotFound     - assertFail("tag")(empty1.copy(vs = nev(Tags(6.AT))))
      'tagIsGroup      - assertFail("tag")(empty1.copy(vs = nev(Tags(tg1.value.AT))))
      // tagIsDead - allow it
      'impSrcNotFound     - assertFail("")(empty1.copy(vs = nev(ImpSrcs(123))))
      'impTgtNotFound     - assertFail("")(empty1.copy(vs = nev(ImpTgts(123))))
      'impSrcSelf         - assertFail("")(empty1.copy(vs = nev(ImpSrcs(1))))
      'impTgtSelf         - assertFail("")(empty1.copy(vs = nev(ImpTgts(1))))
      'impCycle           - assertFail("")(empty1, implied2, CreateGenericReq(3, mf, nev(ImpSrcs(2), ImpTgts(1))))
      'codeBad            - assertFail("")(empty1.copy(vs = nev(ReqCodes(8 -> "!"))))
      'codeBadCaps        - assertFail("")(empty1.copy(vs = nev(ReqCodes(8 -> "NO"))))
      'codeIdInUseByReq   - assertFail("")(createGR(1, codes = Set(5 -> "a")), createGR(2, codes = Set(5 -> "b")))
      'codeIdInUseByGrp   - assertFail("")(createRCG(5, "a"),                  createGR(2, codes = Set(5 -> "b")))
      'codeInUseByReq     - assertFail("")(createGR(1, codes = Set(5 -> "a")), createGR(2, codes = Set(6 -> "a")))
      'codeInUseByGrp     - assertFail("")(createRCG(5, "a"),                  createGR(2, codes = Set(6 -> "a")))
    }

    'createCodeGroup {
      'badId          - List(0,-1).foreach(i => assertFail("id")(createRCG(i, "hi")))
      'badCode        - assertFail("code")  (createRCG(1, "!!"))
      'codeInCaps     - assertFail("code")  (createRCG(1, "NO"))
      'idInUseByReq   - assertFail("")      (createGR(9, codes = Set(1 -> "a")), createRCG(1, "b"))
      'idInUseByGrp   - assertFail("")      (createRCG(1, "a"),                  createRCG(1, "b"))
      'codeInUseByReq - assertFail("in use")(createGR(9, codes = Set(1 -> "a")), createRCG(2, "a"))
      'codeInUseByGrp - assertFail("in use")(createRCG(1, "a"),                  createRCG(2, "a"))
      'replaceLast    - {
        // Adding a new RCG should clear out .lastGroup
        val p = _assertPass(createRCG(1, "abc.def", "old"), delRCG1, createRCG(2, "abc.def", "new"))
        val d = assertSoleReqCode(p, "abc.def")
        assertEq(d, ReqCode.ActiveGroup(LiveReqCodeGroup(2, "new"), ReqCode.emptyReqInactive))
      }
    }

    'updateCodeGroup {
      'title {
        import ReqCodeGroupGD._
        val t = Vector(ReqCodeGroupTitle.Literal("hi there"))
        val p = _assertPass(createRCG(1, "a"), UpdateReqCodeGroup(1, nev(Title(t))))
        assertEq(p.reqCodes.groups.head.title, t)
      }

      'code {
        import ReqCodeGroupGD._
        val p = _assertPass(createRCG(1, "hehe.grr", "Ze Title"), UpdateReqCodeGroup(1, nev(Code("fine.then"))))
        val d = assertSoleReqCode(p, "fine.then")
        assertEq(d, ReqCode.ActiveGroup(LiveReqCodeGroup(1, "Ze Title"), ReqCode.emptyReqInactive))
      }

      'badCode    - assertFail("code")     (createRCG(1, "a"), updateRCGCode(1, "!!"))
      'codeInCaps - assertFail("code")     (createRCG(1, "a"), updateRCGCode(1, "NO"))
      'idNotFound - assertFail("not found")(updateRCGCode(666, "new"))
      'idIsReq    - assertFail("group")    (createGR(1, codes = Set(1 -> "a")), updateRCGCode(1, "b"))

      'tgtCodeInUseByReq -
        assertFail("in use")(createRCG(1, "old"), createGR(2, codes = Set(3 -> "new")), updateRCGCode(1, "new"))

      'tgtCodeInUseByGrp -
        assertFail("in use")(createRCG(1, "old"), createRCG(2, "new"), updateRCGCode(1, "new"))

      // TODO Need a test here similar to createCodeGroup.replaceLast?
    }

    'patchReqCodes {
      // positive tests are in the script tests below

      'reqIdNotFound     - assertFail("")(patchA(add = Set(1 -> "mm")))
      'reqDead           - assertFail("live")(empty1, del1, patchReq(1, add = Set(5 -> "yay")))
      'addcodeSym        - assertFail("")(empty1, patchReq(1, add = Set(7 -> "!!")))
      'addcodeCaps       - assertFail("")(empty1, patchReq(1, add = Set(7 -> "NO")))
      'addIdInUseByReq   - assertFail("")(empty1, createGR(2, codes = Set(3 -> "x")), patchReq(1, add = Set(3 -> "y")))
      'addIdInUseByGrp   - assertFail("")(empty1, createRCG(3, "x"),                  patchReq(1, add = Set(3 -> "y")))
      'addCodeInUseByReq - assertFail("")(empty1, createGR(2, codes = Set(3 -> "x")), patchReq(1, add = Set(9 -> "x")))
      'addCodeInUseByGrp - assertFail("")(empty1, createRCG(3, "x"),                  patchReq(1, add = Set(9 -> "x")))
      'removeNotFound    - assertFail("")(empty1,                                     removeCode3From1)
      'removeOtherReqs   - assertFail("")(empty1, createGR(2, codes = Set(3 -> "x")), removeCode3From1)
      'removeGrps        - assertFail("")(empty1, createRCG(3, "x"),                  removeCode3From1)
      'removeDeadOwn     - assertFail("")(createGR(1, codes = Set(3 -> "x")), removeCode3From1, removeCode3From1)

      'restoreNotFound      - assertFail("")(empty1,                                     restoreCode3From1)
      'restoreLiveOwn       - assertFail("")(createGR(1, codes = Set(3 -> "x")),         restoreCode3From1)
      'restoreLiveOtherReqs - assertFail("")(empty1, createGR(2, codes = Set(3 -> "x")), restoreCode3From1)
      'restoreLiveGrps      - assertFail("")(empty1, createRCG(3, "x"),                  restoreCode3From1)

      'restoreDeadOtherReqs -
        assertFail("")(empty1, createGR(2, codes = Set(3 -> "x")), createRefToCode3, patchReq(2, remove = Set(3)), restoreCode3From1)

      'restoreDeadGrps -
        assertFail("")(empty1, createRCG3, createRefToCode3, delRCG3, restoreCode3From1)

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
        val createA = CreateGenericReq(reqA, mf, nev(ReqCodes(4 -> "a.x")))
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
        val createA = CreateGenericReq(reqA, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Ref to self: "), GRT.CodeRef(1))),
          ReqCodes(1 -> "a.b.c")))
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
        test(patchA(remove = Set(1,3,4), add = mm.addvs("aaa", Set(1, 3))))(
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
        test(CreateGenericReq(98, mf, nev(ReqCodes(10 -> "aaa"))))(
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
        val refs = CreateGenericReq(500, mf, nev(
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
        val refs = CreateGenericReq(500, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #1 and #3: "), GRT.CodeRef(3), GRT.CodeRef(1)))))
        test(refs)("one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]")

        // 3b.3: Merge refs
        test(patchA(remove = Set(1, 3), add = mm.addvs("aaa", Set(1, 3))))(
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
        val refsA = CreateGenericReq(500, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #1 and #3: "), GRT.CodeRef(3), GRT.CodeRef(1)))))
        test(refsA)("one: AD[#1Req(#a)]", "three: AD[#3Req(#a)]", "other: AD[#2Req(#a)]")

        // 4.3: Merge refs
        val origLiveA = Set("aaa: AD[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: AD[#2Req(#a)]")
        test(patchA(remove = Set(1, 3), add = mm.addvs("aaa", Set(1, 3))))(origLiveA)

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
        val refsB = CreateGenericReq(501, mf, nev(
          Title(NonEmptyVector(GRT.Literal("Refs to #4 and #5: "), GRT.CodeRef(4), GRT.CodeRef(5)))))
        test(refsB)(
          "aaa: RR[#1Req(#a)]", "aaa: RR[#3Req(#a)]", "other: RR[#2Req(#a)]",
          "aaa: AD[#4Req(#b)]", "bbb: AD[#5Req(#b)]", "other: AD[#6Req(#b)]")

        // 4.9: Merge refs
        // Note: Even though #4 already = "aaa", it still needs to be in the remove/add set
        //       This ensures that the active ID is always the minimum ID.
        // TODO Ensure MakeEvent uses this logic ↕
        test(patchB(remove = Set(4, 5), add = mm.addvs("aaa", Set(4, 5))))(
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
          val actual = apply.ReqCodeLogic.renameReqCodeToAvoidConflict(conflicted, t)
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
      def patch(id: ReqId)(remove: ApplicableTagId*)(add: ApplicableTagId*): PatchReqTags =
        NonEmpty(SetDiff(removed = remove.toSet, added = add.toSet))
          .map(PatchReqTags(id, _))
          .getOrElse(sys error "Empty set diff")

      'ok {
        var es = Vector[Event](empty1)
        def test(remove: ApplicableTagId*)(add: ApplicableTagId*)(expect: ApplicableTagId*): Unit = {
          es :+= patch(1)(remove: _*)(add: _*)
          val p = _assertPass(es: _*)
          val a = p.reqTags(1)
          assertEq(a, expect.toSet)
        }
        test()(at1, at2)(at1, at2)
        test(at2)()(at1)
        test(at1)(at2)(at2)
        test(at2)()()
      }

      'reqIsDead      - assertFail("dead")(empty1, del1, patch(1)()(at1))
      'reqNotFound    - assertFail("found")(patch(1)()(at1))
      'addBadTag      - assertFail("not found")(empty1, patch(1)()(123))
      'removeBadTag   - assertFail("not found")(empty1, patch(1)(123)())
      'addTagGroup    - assertFail("not found")(empty1, patch(1)()(tg1.value.AT))
      'removeTagGroup - assertFail("not found")(empty1, patch(1)(tg1.value.AT)())

      // 'removeMissingTag = nop
      // 'addExistingTag   = nop
    }

    'patchImps {
      def setdiff(remove: ReqId*)(add: ReqId*) =
        NonEmpty(SetDiff(removed = remove.toSet, added = add.toSet)).getOrElse(sys error "Empty set diff")
      def testFailure(msgFrag: String)(subj: ReqId, events: Event*)(remove: ReqId*)(add: ReqId*): Unit = {
        val sd = setdiff(remove: _*)(add: _*)
        assertFail(msgFrag)(events :+ PatchImplicationSrc(subj, sd): _*)
        assertFail(msgFrag)(events :+ PatchImplicationTgt(subj, sd): _*)
      }

      'ok {
        var es = Vector[Event](empty1, implied2, empty3)
        def test(subj: ReqId, impTgts: Boolean)(remove: ReqId*)(add: ReqId*)(expect: (ReqId, Set[ReqId])*): Unit = {
          val sd = setdiff(remove: _*)(add: _*)
          es :+= (if (impTgts)
            PatchImplicationTgt(subj, sd)
          else
            PatchImplicationSrc(subj, sd))
          val p = _assertPass(es: _*)
          val a = p.implications.srcToTgt.m
          assertEq(a, expect.toMap)
        }
        implicit def ii(t: (Int, Int)): (ReqId, Set[ReqId]) = (t._1, Set(t._2))
        implicit def is(t: (Int, Set[Int])): (ReqId, Set[ReqId]) = (t._1, t._2.map(i => i: ReqId))

        // Start: 1 → 2, 3
        test(2, true) () (3)(1 -> 2, 2 -> 3)
        test(2, false)(1)() (2 -> 3)
        test(3, false)(2)(1)(1 -> 3)
        test(2, false)()(1) (1 -> Set(2, 3))
        test(1, true) (3)() (1 -> 2)
        test(1, true) (2)(3)(1 -> 3)
      }

      'reqNotFound - testFailure("found")(1, empty3)()(3)
      'reqIsDead   - testFailure("dead") (1, empty1, empty3, del1)()(3)
      'impNotFound - testFailure("found")(1, empty1)()(8)
      'impSelf     - testFailure("cycle")(1, empty1)()(1)

      'impCycle {
        val es = Vector(empty1, implied2, CreateGenericReq(3, mf, nev(ImpSrcs(2))))
        assertFail("cycle")(es :+ PatchImplicationTgt(3, NonEmpty.force(SetDiff(Set.empty, Set(1)))): _*)
        assertFail("cycle")(es :+ PatchImplicationSrc(1, NonEmpty.force(SetDiff(Set.empty, Set(3)))): _*)
      }

      // 'removeMissingTag = nop
      // 'addExistingTag   = nop
    }

    'setGenericReqType {
      'ok {
        var es = Vector[Event](empty3, empty1)
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
      'reqIsDead       - assertFail("dead")(empty1, del1, SetGenericReqType(1, fr))
      'reqTypeNotFound - assertFail("found")(empty1, SetGenericReqType(1, 321))
      'reqTypeIsDead   - assertFail("dead")(empty1, DeleteCustomReqType(fr, Delete), SetGenericReqType(1, fr))
    }

    'setGenericReqTitle {
      'ok {
        val p = _assertPass(empty1, setGRT1)
        assertEq(p.reqs.genericReqs.get(1).get.title, someGRTitle)
      }
      'reqNotFound - assertFail("found")(setGRT1)
      'reqIsDead   - assertFail("dead")(empty1, del1, setGRT1)
    }

    'setCustomTextField {
      def e = SetCustomTextField(1, cf1, someCTF1)
      'add {
        val p = _assertPass(empty1, e)
        val d = p.reqText
        assertEq(d.size, 1)
        val m = d(cf1)
        assertEq(m.size, 1)
        assertEq(m(1), someCTF1)
      }
      'remove {
        val p = _assertPass(empty1, e, SetCustomTextField(1, cf1, ∅))
        val d = p.reqText
        assertEq(d.size, 0)
      }
      'reqNotFound   - assertFail("found")(e)
      'reqIsDead     - assertFail("dead") (empty1, del1, e)
      'fieldNotFound - assertFail("found")(empty1, SetCustomTextField(1, 321, someCTF1))
      'fieldDead     - assertFail("dead") (empty1, DeleteCustomField(cf1, Delete), e)
      // TODO test not applicable to target reqtype
    }

    'deleteRestore {

      'deleteReq {
        'notFound - assertFail("not found")(del1)
        'twice    - assertFail("is dead")(createGR(1), del1, del1)
        'ok       - assertPass(createGR(1), del1)
      }

      'deleteRCG {
        'notFound - assertFail("not found")(delRCG1)
        'dead     - assertFail("")(createRCG1, delRCG1, delRCG1)
        'emptyTitleNoRefs - {
          val p = _assertPass(createRCG(1, "abc.def"), delRCG1)
          // It's more work to check for text references to decide whether or not to retain empty RCGs. Just keep em.
          // assertEq("No CodeRefs & no title = no need to retain anything.", p.reqCodes.trie.isEmpty, true)
          val d = assertSoleReqCode(p, "abc.def")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadReqCodeGroup(1, ∅))))
        }
        'emptyTitleWithRefs - {
          val p = _assertPass(empty1, createRCG(3, "qwe.zxc"), createRefToCode3, delRCG3)
          val d = assertSoleReqCode(p, "qwe.zxc")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadReqCodeGroup(3, ∅))))
        }
        'nonEmptyTitle - {
          val p = _assertPass(createRCG(1, "abc.def", "hehe"), delRCG1)
          val d = assertSoleReqCode(p, "abc.def")
          assertEq(d, ReqCode.Data.empty.copy(deadGroup = Some(DeadReqCodeGroup(1, "hehe"))))
        }
      }

      'deleteBoth {
        implicit val init = ContentEventTest.init.add(createRCG1, empty1, createGR(5), createRCG2)
        'reqNotFound   - assertFail("not found")(DeleteReqs(9, 2, ∅))
        'groupNotFound - assertFail("not found")(DeleteReqs(1, 9, ∅))
        'reqDead       - assertFail("dead")(delReq(5), DeleteReqs(5, 2, ∅))
        'groupDead     - assertFail("is not an ActiveGroup.")(delRCG2, DeleteReqs(5, 2, ∅))
        'ok {
          val p = _assertPass(DeleteReqs(5, 2, ∅))
          assertEq("RC#1", p.reqCodes(RCG1_code).isActive, true)
          assertEq("RC#2", p.reqCodes(RCG2_code).isActive, false)
          assertEq("Req #1", p.reqs.genericReqs.need(1).liveExplicitly, Live)
          assertEq("Req #5", p.reqs.genericReqs.need(5).liveExplicitly, Dead)
        }
      }

      'restoreReq {
        'notFound - assertFail("not found")(restoreReq1)
        'live     - assertFail("is live")(empty1, restoreReq1)
        'live2    - assertFail("is live")(empty1, del1, restoreReq1, restoreReq1)
        'ok       - assertPass(empty1, del1, restoreReq1)
        'ok2      - assertPass(empty1, del1, restoreReq1, del1, restoreReq1)
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
              p.reqs.genericReqs.need(id) match {
                case r if r.live(p.config.customReqTypes) :: Live => live
                case _                                            => fmt(p.deletionReasons getLatest id)
              }

            // RCGs don't get reasons
            def rcg(c: ReqCode.Value): R =
              p.reqCodes(c) match {
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
        test(DeleteReqs(NonEmptySet(reqA, reqB), Set(1, 2), dr1))(dr1, dead)(dr1, dead)

        // Restore B,G2
        test(RestoreContent(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 - no reason
        test(DeleteReqs(NonEmptySet(reqB), Set(2), ∅))(dr1, dead)(none, dead)

        // Restore B,G2
        test(RestoreContent(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 - no reason
        test(DeleteReqs(NonEmptySet(reqB), Set(2), ∅))(dr1, dead)(none, dead)

        // Restore B,G2
        test(RestoreContent(Set(reqB), Set(2)))(dr1, dead)(live, live)

        // Delete B,G2 with reason
        val dr2 = "dr#2"
        test(DeleteReqs(NonEmptySet(reqB), Set(2), dr2))(dr1, dead)(dr2, dead)

        // Restore A,B,G1,G2
        test(RestoreContent(Set(reqA, reqB), Set(1, 2)))(live, live)(live, live)

        // Delete A,B with reason
        val dr3 = "dr#3"
        test(DeleteReqs(NonEmptySet(reqA, reqB), ∅, dr3))(dr3, live)(dr3, live)
      }
    }

  }
}
