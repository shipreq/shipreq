package shipreq.webapp.base.text

import japgolly.microlibs.utils.Utils
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Must._
import ProjectText._

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
      rc(code) match {
        case d: ActiveReq   if d.id ==* id => ActiveCodeToReq(code, d.reqId)
        case d: ActiveGroup if d.id ==* id => ActiveCodeToGroup(code, d.group)
        case d =>
          d.deadGroup match {
            case Some(g) if g.id ==* id => DeadGroup(code, g)
            case _                      =>
              def fail = mustNotHappen(s"$id not found in $code: $d")
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

abstract class ProjectText[Ctx <: Context, Out](project: Project, final val ctx: Ctx) {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Abstract

  def text(text: Text.AnyOptional, live: Live): Out
  protected def whenBlankButMandatory: Out

  def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]],
                             live: Live): Out

  /** A single element in the set of flow sources/targets.
    *
    * eg. [This in an example step --> 2.0.1, 2.0.4]
    * could be:                        ↑↑↑↑↑
    * or:                                     ↑↑↑↑↑
    */
  protected val useCaseFlowElement: UseCaseStep.Focus => Out

  protected def deletionReasonWhenNoneGiven: Out
  protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): Out

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Derived: protected

  protected final val cfg = project.config

  protected final def memoByReqId = Memo.by[Req, ReqId](_.id)

  protected final val latestDeletionReasonById: ReqId => Option[Out] =
    Memo(id =>
      project.content.deletionReasons.getLatest(id).map(text(_, Dead)))

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

  // Generic context. Upcasts for pattern-matching
  @inline final def gctx: ProjectText.Context =
    ctx

  final def text(text: Text.AnyNonEmpty, live: Live): Out =
    this.text(text.whole, live)

  final private def mandatoryText(text: Text.AnyOptional, live: Live): Out =
    if (text.isEmpty && live.is(Live))
      whenBlankButMandatory
    else
      this.text(text, live)

  final val reqTitle: Req => Out =
    memoByReqId {
      case gr: GenericReq => mandatoryText(gr.title, gr live cfg.reqTypes)
      case uc: UseCase    => mandatoryText(uc.title, uc.liveUC)
    }

  final def reqTitleById(id: ReqId): Out =
    reqTitle(project.content.reqs.need(id))

  final val codeGroupTitle: CodeGroup => Out =
    Memo.by((_: CodeGroup).id)(g =>
      text(g.title, g.live))

  final val customTextField: CustomField.Text.Id => Req => Option[Out] =
    Memo { fid =>
      project.content.reqText.get(fid) match {
        case Some(m) =>
          val liveField = cfg.fields.customFields.need(fid).live(cfg)
          memoByReqId(r =>
            m.get(r.id).map(text(_, liveField & r.live(cfg.reqTypes))))
        case None =>
          Function const None
      }
    }

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
              case ReqTypeIsDead => Applicable(deletionReasonWhenReqTypeIsDead(cfg.reqTypes.need(r.pubid.reqTypeId)))
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

  final def deleteReasonForCodeGroup: IfApplicable[Nothing] =
    NotApplicable
}
