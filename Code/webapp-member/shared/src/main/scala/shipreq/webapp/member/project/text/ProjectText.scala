package shipreq.webapp.member.project.text

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.{Memo, Utils}
import scala.collection.immutable.SortedSet
import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.util.Must._
import shipreq.webapp.member.project.util.ReqCodeTreeItem

object ProjectText {

  /** The context in which the user will view the [[ProjectText]] output.
    *
    * Different elements of the project are presented in different ways depending on the context in which they are
    * presented.
    */
  sealed trait Context {
    final def ucNum(p: Project): Option[ReqTypePos] =
      this match {
        case ProjectText.Context.None
           | ProjectText.Context.Req(_: GenericReqId) => None
        case ProjectText.Context.Req(uc: UseCaseId)   => Some(p.content.reqs.need(uc).pubid.pos)
      }
  }

  object Context {

    /** User is looking at the entire project. */
    case object None extends Context
    type None = None.type

    /** User is looking at a single req. */
    final case class Req(id: ReqId) extends Context

    implicit def univEq: UnivEq[Context] = UnivEq.derive
  }

  /** How a set of values should be rendered */
  sealed trait SetRenderStyle

  object SetRenderStyle {
    case object SingleLineBrief extends SetRenderStyle
    case object MultiLineDetailed extends SetRenderStyle
  }

  /** Judgement on how a ReqCode-based reference (eg. [email.failure]) should be displayed */
  sealed trait ReqCodeResolution

  object ReqCodeResolution {
    case class ActiveCodeToReq     (code: ReqCode.Value, reqId: ReqId)         extends ReqCodeResolution
    case class ReqWithAltCode      (code: ReqCode.Value, reqId: ReqId)         extends ReqCodeResolution
    case class ReqWithoutActiveCode(code: ReqCode.Value, reqId: ReqId)         extends ReqCodeResolution
    case class ActiveCodeToGroup   (code: ReqCode.Value, group: LiveCodeGroup) extends ReqCodeResolution
    case class DeadGroup           (code: ReqCode.Value, group: DeadCodeGroup) extends ReqCodeResolution

    /**
     * FR-152: For refs to reqs made using semantic ID, System shall render the ref accordingly...
     *  - If target req has no semIDs anymore, display the pubid.
     *  - If target req has the semID entered on ref creation, display the semID.
     *  - If target req doesn't have the semID entered on ref creation, display the closest semID.
     *
     * FR-292: For refs to SHRs, System shall render the ref accordingly...
     *  - If target exists, display the semID.
     *  - If target doesn't exist, display the semID and mark it as an issue.
     */
    def apply(id: ReqCodeId, rc: ReqCodes): ReqCodeResolution = {
      import ReqCode._
      import PlainText.reqCode

      // "display the closest semID" is translated here to closest via Levenshtein distance.
      // Algorithm could be improved to be more meaningful, like most common (node) prefix, nodes in common, etc.
      def findAlt(reqId: ReqId, deadCode: Value): Option[ReqWithAltCode] = {
        val deadCodeStr = reqCode(deadCode)
        NonEmptySet.option(rc.activeReqCodesByReqId(reqId) - deadCode).map { cs =>
          val c = cs.whole.minBy(c => Utils.levenshtein(deadCodeStr, reqCode(c)))
          ReqWithAltCode(c, reqId)
        }
      }

      val code = rc.reqCode(id)
      rc.need(code) match {
        case d: ActiveReq   if d.id ==* id => ActiveCodeToReq(code, d.reqId)
        case d: ActiveGroup if d.id ==* id => ActiveCodeToGroup(code, d.group)
        case d =>
          d.deadGroup match {
            case Some(g) if g.id ==* id => DeadGroup(code, g)
            case _                      =>
              def fail = mustNotHappen(ErrorMsg(s"$id not found in $code: $d"))
              id match {
                case i: ApReqCodeId =>
                  d.reqInactive.m.find(_._2 contains i) match {
                    case Some((reqId, _)) => findAlt(reqId, code) getOrElse ReqWithoutActiveCode(code, reqId)
                    case None             => fail
                  }
                case _: ReqCodeGroupId => fail
              }
          }
      }
    }
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

import ProjectText._

abstract class ProjectText[+Ctx <: Context, Out](project: Project, final val ctx: Ctx) {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Abstract

  protected def _implicationList(ids: Vector[Pubid], style: SetRenderStyle): Out

  protected def _text(text: Text.AnyOptional, live: Live, tagValidity: ApplicableTagId => Validity): Out

  protected def deletionReasonWhenNoneGiven: Out

  protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): Out

  protected def emptyText: Out

  /** A single element in the set of flow sources/targets.
   *
   * eg. [This in an example step --> 2.0.1, 2.0.4]
   * could be:                        ↑↑↑↑↑
   * or:                                     ↑↑↑↑↑
   */
  protected val useCaseFlowElement: UseCaseStep.Focus => Out

  def whenBlankButMandatory: Out

  def pastPubids(ids: SortedSet[ExternalPubid]): Out

  def reqCode(c: ReqCode.Value): Out

  def reqCodes(reqCodes: IterableOnce[ReqCode.Value]): Out

  def reqCodeTree(items: Vector[ReqCodeTreeItem]): Out

  def reqCodeTreeItem(item: ReqCodeTreeItem): Out

  /** eg. "UC" */
  def reqTypeShort(id: ReqTypeId): Out

  /** eg. "UC: Use Case" */
  def reqTypeFull(id: ReqTypeId): Out

  def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]],
                             live: Live): Out

  def withCtx[Ctx2 <: ProjectText.Context](newCtx: Ctx2): ProjectText[Ctx2, Out]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Derived: protected

  protected final val cfg = project.config

  protected final val latestDeletionReasonById: ReqId => Option[Out] =
    Memo(id =>
      project.content.deletionReasons.getLatest(id).map(text(_, Dead, Valid.always)))

  protected final def memoByReqId = Memo.by[Req, ReqId](_.id)

  protected final def useCaseFlowElementById(id: UseCaseStepId): Out =
    useCaseFlowElement(project.content.reqs.useCases.focusStep(id))

  protected final def useCaseFlowElements(elements: Iterator[UseCaseStep.Focus]): MutableArray[Out] =
    MutableArray(elements)
      .sortBy(_.ploc)
      .map(useCaseFlowElement)

  protected final def useCaseFlowElementsById(ids: Set[UseCaseStepId]): MutableArray[Out] =
    useCaseFlowElements(ids.iterator.map(project.content.reqs.useCases.focusStep))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Derived: public

  final val codeGroupTitle: CodeGroup => Out =
    Memo.by((_: CodeGroup).id)(g =>
      text(g.title, g.live, Valid.always, Optional))

  final def customTextField(id: CustomField.Text.Id, req: Req, live: Live, mandatory: Mandatory): Out =
    customTextFieldOption(id)(req).getOrElse[Out] {
      if (live.is(Live) && mandatory.is(Mandatory))
        whenBlankButMandatory
      else
        emptyText
    }

  final val customTextFieldOption: CustomField.Text.Id => Req => Option[Out] =
    Memo { fid =>
      project.content.reqText.data.get(fid) match {
        case Some(m) =>
          val liveField = cfg.fields.customFields.need(fid).live(cfg)
          memoByReqId(r =>
            m.get(r.id).map(text(_, liveField & r.live(cfg.reqTypes), project.naTagsForReq(r))))
        case None =>
          Function const None
      }
    }

  final def deleteReasonForCodeGroup: IfApplicable[Nothing] =
    NotApplicable

  final def deleteReasonForReq(req: Req): IfApplicable[Out] = {
    def latestReason(id: ReqId): Out =
      latestDeletionReasonById(id) getOrElse deletionReasonWhenNoneGiven

    req match {

      case r: GenericReq =>
        import GenericReq.ImplicitLiveStatus._
        r.liveExplicitly match { // explicit must be checked before implicit
          case Live =>
            r.implicitLiveStatus(cfg.reqTypes) match {
              case NoImpact      => NotApplicable // req is live
              case ReqTypeIsDead =>
                cfg.reqTypes.get(r.pubid.reqTypeId) match {
                  case Some(rt) => Applicable(deletionReasonWhenReqTypeIsDead(rt))
                  case None     => NotApplicable
                }
            }
          case Dead => Applicable(latestReason(r.id))
        }

      case uc: UseCase =>
        uc.liveUC match {
          case Live => NotApplicable
          case Dead => Applicable(latestReason(uc.id))
        }
    }
  }

  // Generic context. Upcasts for pattern-matching
  @inline final def gctx: ProjectText.Context =
    ctx

  final def implicationList(ids: Vector[Pubid], live: Live, mandatory: Mandatory, style: SetRenderStyle): Out =
    if (ids.isEmpty && live.is(Live) && mandatory.is(Mandatory))
      whenBlankButMandatory
    else
      _implicationList(ids, style)

  final def manualIssue(text: Text.ManualIssue.NonEmptyText): Out =
    _text(text.whole, Live, Valid.always)

  final val reqTitle: Req => Out = {
    def make(t: Text.AnyOptional, req: Req) = text(t, req.live(cfg.reqTypes), project.naTagsForReq(req), Mandatory)
    memoByReqId {
      case gr: GenericReq => make(gr.title, gr)
      case uc: UseCase    => make(uc.title, uc)
    }
  }

  final def reqTitleById(id: ReqId): Out =
    reqTitle(project.content.reqs.need(id))

  final def text(text: Text.AnyNonEmpty, live: Live, tagValidity: ApplicableTagId => Validity): Out =
    _text(text.whole, live, tagValidity)

  final def text(text: Text.AnyOptional, live: Live, tagValidity: ApplicableTagId => Validity, mandatory: Mandatory): Out =
    if (text.isEmpty && live.is(Live) && mandatory.is(Mandatory))
      whenBlankButMandatory
    else
      _text(text, live, tagValidity)

  final def useCaseStepTextAndFlow(f: UseCaseStep.Focus, fd: FilterDead): Out =
    useCaseStepTextAndFlow(f.textAndFlow(fd), f.live)

}
