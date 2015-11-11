package shipreq.webapp.base.event

import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import ApplyEventTestFns._
import shipreq.webapp.base.text.{Text => T}

case class DetachedGenericReq(req      : GenericReq,
                              tags     : Set[ApplicableTagId],
                              impliedBy: Set[ReqId],
                              implies  : Set[ReqId],
                              reqCodes : Set[ReqCode.Value])

object DetachedGenericReq {
  implicit def equality: UnivEq[DetachedGenericReq] = UnivEq.derive

  def extract(p: Project, id: GenericReqId): Option[DetachedGenericReq] =
    p.reqs.genericReqs.get(id).map { r =>
      val tags      = p.reqTags(id)
      val impliedBy = p.implications.backwards(id)
      val implies   = p.implications.forwards(id)
      val reqCodes  = p.reqCodes.activeReqCodesByReqId(id)
      DetachedGenericReq(r, tags, impliedBy, implies, reqCodes)
    }
}

case class DetachedUseCase(req      : UseCase,
                           tags     : Set[ApplicableTagId],
                           impliedBy: Set[ReqId],
                           implies  : Set[ReqId],
                           reqCodes : Set[ReqCode.Value])

object DetachedUseCase {
  implicit def equality: UnivEq[DetachedUseCase] = UnivEq.derive

  def extract(p: Project, id: UseCaseId): Option[DetachedUseCase] =
    p.reqs.useCases.imap.get(id).map { r =>
      val tags      = p.reqTags(id)
      val impliedBy = p.implications.backwards(id)
      val implies   = p.implications.forwards(id)
      val reqCodes  = p.reqCodes.activeReqCodesByReqId(id)
      DetachedUseCase(r, tags, impliedBy, implies, reqCodes)
    }
}

// =====================================================================================================================

object ContentEventTestHelp {

  def createRCG(id: ReqCodeId, code: ReqCode.Value, title: T.ReqCodeGroupTitle.OptionalText = ∅) = {
    import ReqCodeGroupGD._
    CreateReqCodeGroup(id, nev(Code(code), Title(title)))
  }

  def updateRCGCode(id: ReqCodeId, code: ReqCode.Value) = {
    import ReqCodeGroupGD._
    UpdateReqCodeGroup(id, nev(Code(code)))
  }

  def delRCG(id: ReqCodeId): DeleteReqCodeGroups =
    DeleteReqCodeGroups(NonEmptySet(id))

  def restoreRCG(id: ReqCodeId): RestoreContent =
    RestoreContent(∅, Set(id))

  def createGR(id     : GenericReqId,
               rt     : CustomReqTypeId                = mf,
               codes  : Set[ReqCode.IdAndValue]        = ∅,
               title  : T.GenericReqTitle.OptionalText = ∅,
               impSrcs: Set[ReqId]                     = ∅,
               impTgts: Set[ReqId]                     = ∅) = {
    import CreateGenericReqGD._
    var vs = emptyValues
    NonEmptySet   .maybe(codes,   ())(vs += ReqCodes(_))
    NonEmptyVector.maybe(title,   ())(vs += Title(_))
    NonEmptySet   .maybe(impSrcs, ())(vs += ImpSrcs(_))
    NonEmptySet   .maybe(impTgts, ())(vs += ImpTgts(_))
    CreateGenericReq(id, rt, vs)
  }

  def createUC(id     : UseCaseId,
               stepId : UseCaseStepId,
               codes  : Set[ReqCode.IdAndValue]     = ∅,
               title  : T.UseCaseTitle.OptionalText = ∅,
               impSrcs: Set[ReqId]                  = ∅,
               impTgts: Set[ReqId]                  = ∅) = {
    import CreateUseCaseGD._
    var vs = emptyValues
    NonEmptySet   .maybe(codes,   ())(vs += ReqCodes(_))
    NonEmptyVector.maybe(title,   ())(vs += Title(_))
    NonEmptySet   .maybe(impSrcs, ())(vs += ImpSrcs(_))
    NonEmptySet   .maybe(impTgts, ())(vs += ImpTgts(_))
    CreateUseCase(id, stepId, vs)
  }

  def delReq(id: ReqId): DeleteReqs =
    DeleteReqs(NonEmptySet(id), ∅, ∅)

  def restoreReq(id: ReqId): RestoreContent =
    RestoreContent(Set(id), ∅)

  val patchRcAdd0 = Multimap.empty[ReqCode.Value, Set, ReqCodeId]

  def patchReqCodes(id     : ReqId,
                    remove : Set[ReqCodeId]                          = Set.empty,
                    restore: Set[ReqCodeId]                          = Set.empty,
                    add    : Multimap[ReqCode.Value, Set, ReqCodeId] = patchRcAdd0) =
    PatchReqCodes(id, remove = remove, restore = restore, add)

  case class PatchReqCodeB(id: ReqId) extends AnyVal {
    def apply(remove : Set[ReqCodeId]                          = Set.empty,
              restore: Set[ReqCodeId]                          = Set.empty,
              add    : Multimap[ReqCode.Value, Set, ReqCodeId] = patchRcAdd0) =
      PatchReqCodes(id, remove = remove, restore = restore, add)
  }

  // =====================================================================================================================

  def assertSoleReqCode(p: Project, code: ReqCode.Value): ReqCode.Data = {
    val v = p.reqCodes.trie.flatStream.toVector
    assertEq("Trie size", v.size, 1)
    assertEq("Sole req code", v.head._1, code)
    v.head._2
  }

  def assertGR(p: Project, id: GenericReqId)(req      : GenericReq,
                                             tags     : Set[ApplicableTagId] = UnivEq.emptySet,
                                             impliedBy: Set[ReqId]           = UnivEq.emptySet,
                                             implies  : Set[ReqId]           = UnivEq.emptySet,
                                             reqCodes : Set[ReqCode.Value]   = UnivEq.emptySet): Unit =
    assertEq(
      DetachedGenericReq.extract(p, id),
      Some(DetachedGenericReq(req, tags, impliedBy, implies, reqCodes)))

  def assertUC(p: Project, id: UseCaseId)(uc       : UseCase,
                                          tags     : Set[ApplicableTagId] = UnivEq.emptySet,
                                          impliedBy: Set[ReqId]           = UnivEq.emptySet,
                                          implies  : Set[ReqId]           = UnivEq.emptySet,
                                          reqCodes : Set[ReqCode.Value]   = UnivEq.emptySet): Unit =
    assertEq(
      DetachedUseCase.extract(p, id),
      Some(DetachedUseCase(uc, tags, impliedBy, implies, reqCodes)))

  // ===================================================================================================================

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

  val createIssueType1 = {
    import CustomIssueTypeGD._
    CreateCustomIssueType(1, nev(Key("TBD"), Desc(None)))
  }
  val issueType1 = createIssueType1.id

  val createCTF1 = {
    import CustomTextFieldGD._
    CreateCustomTextField(80, nev(Name("asdf"), Key("qwer"), Mandatory(true), ReqTypes(allReqTypes)))
  }
  val cf1 = createCTF1.id

  val testHelpInit = InitialEvents(createIssueType1, createMF, createFR, createAT1, createAT2, createTG1, createCTF1)

  val delReq1     = delReq(1)
  val restoreReq1 = restoreReq(1)

  val emptyGR1   = createGR(1)
  val impliedGR2 = createGR(2, impSrcs = Set(emptyGR1.id))
  val emptyGR3   = createGR(3)

  val emptyUC1   = createUC(1.UC, 1)
  val impliedUC2 = createUC(2.UC, 2, impSrcs = Set(emptyUC1.id))
  val emptyUC3   = createUC(3.UC, 3)
}
