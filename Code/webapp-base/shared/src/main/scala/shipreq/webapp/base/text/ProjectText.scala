package shipreq.webapp.base.text

import shipreq.base.util.{Memo, NonEmptySet, Util}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Must._
import DataImplicits._
import ProjectText.FormatAtomFn

object ProjectText {
  type FormatAtomFn[Out] = (Live, Text.AnyOptional) => Out
  @inline def apply[Out](project: Project, _format: FormatAtomFn[Out]): ProjectText[Out] =
    new ProjectText[Out](project) {
      override val format = _format
    }

  // -------------------------------------------------------------------------------------------------------------------

  /** Judgement on how a ReqCode-based reference (eg. [email.failure]) should be displayed */
  sealed trait ReqCodeResolution
  object ReqCodeResolution {
    case class ActiveCodeToReq     (code: ReqCode.Value, reqId: ReqId)            extends ReqCodeResolution
    case class ReqWithAltCode      (code: ReqCode.Value, reqId: ReqId)            extends ReqCodeResolution
    case class ReqWithoutActiveCode(code: ReqCode.Value, reqId: ReqId)            extends ReqCodeResolution
    case class ActiveCodeToGroup   (code: ReqCode.Value, group: LiveReqCodeGroup) extends ReqCodeResolution
    case class DeadGroup           (code: ReqCode.Value, group: DeadReqCodeGroup) extends ReqCodeResolution
  }

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
  def resolveReqCode(id: ReqCodeId, rc: ReqCodes): ReqCodeResolution = {
    import ReqCodeResolution._
    import ReqCode._
    import PlainText.reqCode

    // "display the closest semID" is translated here to closest via Levenshtein distance.
    // Algorithm could be improved to be more meaningful, like most common (node) prefix, nodes in common, etc.
    def findAlt(reqId: ReqId, deadCode: Value): Option[ReqWithAltCode] = {
      val deadCodeStr = reqCode(deadCode)
      NonEmptySet.option(rc.activeReqCodesByReqId(reqId) - deadCode).map { cs =>
        val c = cs.whole.minBy(c => Util.levenshtein(deadCodeStr, reqCode(c)))
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
          case _ =>
            d.reqInactive.m.find(_._2 contains id) match {
              case Some((reqId, _)) => findAlt(reqId, code) getOrElse ReqWithoutActiveCode(code, reqId)
              case None             => mustNotHappen(s"$id not found in $code: $d")
            }
        }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  abstract class DeletionReasonFormatter[Out] {
    type PT <: ProjectText[Out]

    protected def `n/a`: Out
    protected def noReasonGiven: Out
    protected def reqTypeIsDead(pt: PT, rt: ReqType): Out

    final def reqCodeGroup = `n/a`

    private def latestReason(pt: PT, id: ReqId): Out =
      pt.latestDeletionReason(id) getOrElse noReasonGiven

    final def req(p: Project, pt: PT, req: Req): Out =
      req match {

        case r: GenericReq =>
          import GenericReq.ImplicitLiveStatus._
          r.liveExplicitly match { // explicit must be checked before implicit
            case Live =>
              r.implicitLiveStatus(p.config.customReqTypes) match {
                case NoImpact      => `n/a` // req is live
                case ReqTypeIsDead => reqTypeIsDead(pt, p.config.reqType(r.pubid.reqTypeId))
              }
            case Dead => latestReason(pt, r.id)
          }

        case uc: UseCase =>
          uc.liveUC match {
            case Live => `n/a`
            case Dead => latestReason(pt, uc.id)
          }
      }
  }
}

abstract class ProjectText[Out](project: Project) {
  final val cfg = project.config

  val format: FormatAtomFn[Out]

  val format1: (Live, Text.AnyNonEmpty) => Out =
    (l, nev) => format(l, nev.whole)

  private def memoByReqId = Memo.by[Req, ReqId](_.id)

  val reqTitle: Req => Out =
    memoByReqId {
      case gr: GenericReq => format(gr live cfg.customReqTypes, gr.title)
      case uc: UseCase    => format(uc.liveUC, uc.title)
    }

  val reqCodeGroupTitle: ReqCodeGroup => Out =
    Memo.by((_: ReqCodeGroup).id)(g =>
      format(g.live, g.title))

  def reqTitleById(id: ReqId): Out =
    reqTitle(project.reqs.req(id))

  val customTextField: CustomField.Text.Id => Req => Option[Out] =
    Memo { fid =>
      project.reqText.get(fid) match {
        case Some(m) =>
          val liveField = cfg.fields.customFields.need(fid).live(cfg)
          memoByReqId(r =>
            m.get(r.id) map (format1(liveField & r.live(cfg.customReqTypes), _)))
        case None =>
          Function const None
      }
    }

  val latestDeletionReason: ReqId => Option[Out] =
    Memo(id =>
      project.deletionReasons.getLatest(id).map(format1(Dead, _)))
}
