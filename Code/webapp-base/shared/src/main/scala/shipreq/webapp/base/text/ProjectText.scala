package shipreq.webapp.base.text

import scalaz.syntax.equal._
import shipreq.base.util.{Memo, NonEmptySet, Util}
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
    case class ActiveCode     (code: ReqCode.Value, target: ReqCode.Target) extends ReqCodeResolution
    case class DeadGroup      (code: ReqCode.Value)                         extends ReqCodeResolution
    case class ReqWithoutCodes(reqId: ReqId)                                extends ReqCodeResolution
    case class ReqWithAltCode (code: ReqCode.Value, reqId: ReqId)           extends ReqCodeResolution
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
      NonEmptySet.option(rc.activeReqCodesByTarget(reqId) - deadCode).map { cs =>
        val c = cs.whole.minBy(c => Util.levenshtein(deadCodeStr, reqCode(c)))
        ReqWithAltCode(c, reqId)
      }
    }

    val code = rc.reqCode(id)
    val data = rc(code)
    data.active match {
      case Some(ad) if ad.id ≟ id =>
        ActiveCode(code, ad.target)
      case None =>
        if (data.refsToGroup contains id)
          DeadGroup (code)
        else
          data.reqInactive.m.find(_._2 contains id) match {
            case Some((reqId, _)) => findAlt(reqId, code) getOrElse ReqWithoutCodes(reqId)
            case None             => mustNotHappen(s"$id not found in $code: $data")
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
      case r: GenericReq => format(r live cfg.customReqTypes, r.title)
    }

  val reqCodeGroupTitle: ReqCodeGroup.AndId => Out =
    Memo.by((_: ReqCodeGroup.AndId).id)(g =>
      format(g.group.live, g.group.title))

  def reqTitleById(id: ReqId): Out =
    reqTitle(project.reqs.req(id))

  val customTextField: CustomField.Text.Id => Req => Option[Out] =
    Memo { fid =>
      project.reqText.get(fid) match {
        case Some(m) =>
          val liveField = cfg.fields.customFields.need(fid).live(cfg)
          memoByReqId(r =>
            m.get(r.id) map (format1(liveField && r.live(cfg.customReqTypes), _)))
        case None =>
          Function const None
      }
    }
}
