package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEventTestFns._
import shipreq.webapp.base.event.ContentEventTestHelp.CustomTextMap
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.{Text => T}

case class DetachedGenericReq(req       : GenericReq,
                              customText: CustomTextMap,
                              tags      : Set[ApplicableTagId],
                              impliedBy : Set[ReqId],
                              implies   : Set[ReqId],
                              reqCodes  : Set[ReqCode.Value])

object DetachedGenericReq {
  implicit def equality: UnivEq[DetachedGenericReq] = UnivEq.derive

  def extract(p: Project, id: GenericReqId): Option[DetachedGenericReq] =
    p.content.reqs.genericReqs.imap.get(id).map { r =>
      val codes      = p.content.reqCodes.activeReqCodesByReqId(id)
      val customText = p.content.reqText.allTextForReq(id)
      val impliedBy  = p.content.implications.backwards(id)
      val implies    = p.content.implications.forwards(id)
      val tags       = p.content.reqTags(id)
      DetachedGenericReq(r, customText, tags, impliedBy, implies, codes)
    }
}

case class DetachedUseCase(req      : UseCase,
                           customText: CustomTextMap,
                           tags     : Set[ApplicableTagId],
                           impliedBy: Set[ReqId],
                           implies  : Set[ReqId],
                           reqCodes : Set[ReqCode.Value])

object DetachedUseCase {
  implicit def equality: UnivEq[DetachedUseCase] = UnivEq.derive

  def extract(p: Project, id: UseCaseId): Option[DetachedUseCase] =
    p.content.reqs.useCases.imap.get(id).map { r =>
      val codes      = p.content.reqCodes.activeReqCodesByReqId(id)
      val customText = p.content.reqText.allTextForReq(id)
      val impliedBy  = p.content.implications.backwards(id)
      val implies    = p.content.implications.forwards(id)
      val tags       = p.content.reqTags(id)
      DetachedUseCase(r, customText, tags, impliedBy, implies, codes)
    }
}

// =====================================================================================================================

object ContentEventTestHelp {

  type CustomTextMap = Map[CustomField.Text.Id, T.CustomTextField.NonEmptyText]

  implicit class ProjectEventTestExt(private val p: Project) extends AnyVal {
    def needUC(id: UseCaseId): UseCase =
      p.content.reqs.useCases.imap need id
  }

  def createRCG(id: ReqCodeGroupId, code: ReqCode.Value, title: T.CodeGroupTitle.OptionalText = ∅) = {
    import CodeGroupGD._
    CodeGroupCreate(id, nev(Code(code), Title(title)))
  }

  def updateRCGCode(id: ReqCodeGroupId, code: ReqCode.Value) = {
    import CodeGroupGD._
    CodeGroupUpdate(id, nev(Code(code)))
  }

  def delRCG(id: ReqCodeGroupId): CodeGroupsDelete =
    CodeGroupsDelete(NonEmptySet(id))

  def restoreRCG(id: ReqCodeGroupId): ContentRestore =
    ContentRestore(∅, Set(id))

  def createGR(id     : GenericReqId,
               rt     : CustomReqTypeId                = mf,
               codes  : Set[ApReqCodeId.AndValue]      = ∅,
               title  : T.GenericReqTitle.OptionalText = ∅,
               impSrcs: Set[ReqId]                     = ∅,
               impTgts: Set[ReqId]                     = ∅) = {
    import GenericReqGD._
    var vs = emptyValues
    NonEmptySet     .maybe(codes,   ())(vs += Codes   (_))
    NonEmptySet     .maybe(impSrcs, ())(vs += ImpSrcs (_))
    NonEmptySet     .maybe(impTgts, ())(vs += ImpTgts (_))
    NonEmptyArraySeq.maybe(title,   ())(vs += Title   (_))
    GenericReqCreate(id, rt, vs)
  }

  def createUC(id     : UseCaseId,
               stepId : UseCaseStepId,
               codes  : Set[ApReqCodeId.AndValue]   = ∅,
               title  : T.UseCaseTitle.OptionalText = ∅,
               impSrcs: Set[ReqId]                  = ∅,
               impTgts: Set[ReqId]                  = ∅) = {
    import UseCaseGD._
    var vs = emptyValues
    NonEmptySet     .maybe(codes,   ())(vs += Codes   (_))
    NonEmptySet     .maybe(impSrcs, ())(vs += ImpSrcs (_))
    NonEmptySet     .maybe(impTgts, ())(vs += ImpTgts (_))
    NonEmptyArraySeq.maybe(title,   ())(vs += Title   (_))
    UseCaseCreate(id, stepId, vs)
  }

  def delGR(id: GenericReqId): ReqsDelete =
    ReqsDelete(NonEmptySet(id), ∅, ∅)

  def delUC(id: UseCaseId): ReqsDelete =
    ReqsDelete(NonEmptySet(id), ∅, ∅)

  def restoreGR(id: GenericReqId): ContentRestore =
    ContentRestore(Set(id), ∅)

  def restoreUC(id: UseCaseId): ContentRestore =
    ContentRestore(Set(id), ∅)

  val patchRcAdd0 = Multimap.empty[ReqCode.Value, Set, ApReqCodeId]

  def patchCodes(id     : ReqId,
                 remove : Set[ApReqCodeId]                          = Set.empty,
                 restore: Set[ApReqCodeId]                          = Set.empty,
                 add    : Multimap[ReqCode.Value, Set, ApReqCodeId] = patchRcAdd0) =
    ReqCodesPatch(id, remove = remove, restore = restore, add)

  case class PatchReqCodeB(id: ReqId) extends AnyVal {
    def apply(remove : Set[ApReqCodeId]                          = Set.empty,
              restore: Set[ApReqCodeId]                          = Set.empty,
              add    : Multimap[ReqCode.Value, Set, ApReqCodeId] = patchRcAdd0) =
      ReqCodesPatch(id, remove = remove, restore = restore, add)
  }

  // ===================================================================================================================

  def assertSoleReqCode(p: Project, code: ReqCode.Value): ReqCode.Data = {
    val v = p.content.reqCodes.trie.flatIterator().toVector
    assertEq("Trie size", v.size, 1)
    assertEq("Sole req code", v.head._1, code)
    v.head._2
  }

  def assertGR(p: Project, id: GenericReqId)(req       : GenericReq,
                                             customText: CustomTextMap        = UnivEq.emptyMap,
                                             tags      : Set[ApplicableTagId] = UnivEq.emptySet,
                                             impliedBy : Set[ReqId]           = UnivEq.emptySet,
                                             implies   : Set[ReqId]           = UnivEq.emptySet,
                                             reqCodes  : Set[ReqCode.Value]   = UnivEq.emptySet): Unit =
    assertEq(
      s"assertGR(${id.value})",
      DetachedGenericReq.extract(p, id),
      Some(DetachedGenericReq(req, customText, tags, impliedBy, implies, reqCodes)))

  def assertUC(p: Project, id: UseCaseId)(uc         : UseCase,
                                          customText : CustomTextMap        = UnivEq.emptyMap,
                                          tags       : Set[ApplicableTagId] = UnivEq.emptySet,
                                          impliedBy  : Set[ReqId]           = UnivEq.emptySet,
                                          implies    : Set[ReqId]           = UnivEq.emptySet,
                                          reqCodes   : Set[ReqCode.Value]   = UnivEq.emptySet,
                                          ignoreSteps: Boolean              = false): Unit = {
    var d = DetachedUseCase.extract(p, id)
    var e = DetachedUseCase(uc, customText, tags, impliedBy, implies, reqCodes)

    if (ignoreSteps) {
      def f(x: DetachedUseCase): DetachedUseCase =
        x.copy(req = x.req.copy(stepsNA = UseCaseSteps.emptyRoot(0), stepsE = UseCaseSteps.empty))
      d = d map f
      e = f(e)
    }

    assertEq(s"assertUC(${id.value})", d, Some(e))
  }

  def assertUcSteps(steps: UseCaseSteps, keys: String*): Unit =
    assertUcStepsO(None, steps, keys: _*)

  def assertUcSteps(name: => String, steps: UseCaseSteps, keys: String*): Unit =
    assertUcStepsO(Some(name), steps, keys: _*)

  def assertUcStepsO(name: => Option[String], steps: UseCaseSteps, keys: String*): Unit =
    assertSetO(name,
      steps.tree.filter(_.liveIgnoringUC(steps) match {
        case Live => VectorTree.NodeFilter.KeepNode
        case Dead => VectorTree.NodeFilter.DiscardNodeAndChildren
      }).locIterator.map(_.map(_.toString).mkString(".")).toSet,
      keys.toSet)

  def assertAllUcSteps(uc: UseCase)(nc: String*)(e: String*): Unit = {
    def prefix = "UC-" + uc.pos.value + " "
    assertUcSteps(prefix + "NC/AC", uc.stepsNA, nc: _*)
    assertUcSteps(prefix + "EC"   , uc.stepsE , e: _*)
  }

  def assertBadIdsRejected(f: Int => ActiveEvent)(implicit ie: InitialEvents): Unit =
    badIds.foreach(i => assertFail("id")(f(i)))

  // ===================================================================================================================

  private val badIds = List(0, -1)

  val mf: CustomReqTypeId = 100
  val fr: CustomReqTypeId = 101
  val (createMF, createFR) = {
    import CustomReqTypeGD._
    ( CustomReqTypeCreate(mf, nev(Mnemonic("MF"), Name("MajFea"), Description(None), Implication(false)))
    , CustomReqTypeCreate(fr, nev(Mnemonic("FR"), Name("FunReq"), Description(None), Implication(false)))
    )
  }

  val at1: ApplicableTagId = 11
  val at2: ApplicableTagId = 12
  val (createAT1, createAT2) = {
    import ApplicableTagGD._
    ( ApplicableTagCreate(at1, nev(Key("at-one"), Desc(None), Colour(None), ApplicableReqTypes(allReqTypes)))
    , ApplicableTagCreate(at2, nev(Key("at-two"), Desc(None), Colour(None), ApplicableReqTypes(allReqTypes)))
    )
  }

  val tg1: TagGroupId = 20
  val createTG1 = {
    import TagGroupGD._
    TagGroupCreate(tg1, nev(Name("TG #1"), Desc(None), Exclusivity(false)))
  }

  val createIssueType1 = {
    import CustomIssueTypeGD._
    CustomIssueTypeCreate(1, nev(Key("TBD"), Desc(None)))
  }
  val issueType1 = createIssueType1.id

  val createCTF1 = {
    import CustomTextFieldGDv1._
    FieldCustomTextCreateV1(80, nev(Name("asdf"), Key("qwer"), Mandatory(true), ApplicableReqTypes(allReqTypes)))
  }
  val cf1 = createCTF1.id

  val createCTF2 = {
    import CustomTextFieldGDv1._
    FieldCustomTextCreateV1(81, nev(Name("blurp!"), Key("blurp"), Mandatory(false), ApplicableReqTypes(allReqTypes)))
  }
  val cf2 = createCTF2.id

  val testHelpInit = InitialEvents(
    createIssueType1,
    createMF,
    createFR,
    createAT1,
    createAT2,
    createTG1,
    createCTF1,
    createCTF2,
  )

  val emptyGR1   = createGR(1)
  val impliedGR2 = createGR(2, impSrcs = Set(emptyGR1.id))
  val emptyGR3   = createGR(3)
  val delGR1     = delGR(1)
  val restoreGR1 = restoreGR(1)

  val emptyUC1   = createUC(1.UC, 1)
  val impliedUC2 = createUC(2.UC, 2, impSrcs = Set(emptyUC1.id))
  val emptyUC3   = createUC(3.UC, 3)
  val delUC1     = delUC(1.UC)
  val restoreUC1 = restoreUC(1.UC)

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
}
