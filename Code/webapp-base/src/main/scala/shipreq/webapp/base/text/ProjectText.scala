package shipreq.webapp.base.text

import scalaz.syntax.equal._
import shipreq.base.util.{Util, NonEmptySet, Must, UnivEq}
import shipreq.webapp.base.data._

object ProjectText {
  @inline def apply[Out](project: Project, _format: Text.AnyOptional => Out): ProjectText[Out] =
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
  def resolveReqCode(id: ReqCodeId, rc: ReqCodes): Must[ReqCodeResolution] = {
    import ReqCodeResolution._
    import ReqCode._
    import Must.Auto._
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

    rc.reqCode(id).flatMap(code =>
      rc.applyM(code).flatMap {
        case Data(Some(ad), _, _) if ad.id ≟ id => ActiveCode(code, ad.target)
        case d if d.refsToGroup.contains(id)    => DeadGroup(code)
        case d => d.refsToReqs.m.find(_._2 contains id) match {
          case Some((reqId, _))                 => findAlt(reqId, code) getOrElse[ReqCodeResolution] ReqWithoutCodes(reqId)
          case None                             => Must.Failed(s"$id not found in $code: $d")
        }
      }
    )
  }
}

abstract class ProjectText[Out](project: Project) {
  import UnivEq.{mutableHashMapMemo => memo}

  val format: Text.AnyOptional => Out

  val format1: Text.AnyNonEmpty => Out =
    nev => format(nev.whole)

  private val _reqTitle: Req => Out = {
    case r: GenericReq => format(r.title)
  }

  val reqTitle: Req => Out = {
    val memo = new scala.collection.mutable.HashMap[ReqId, Out]
    req => memo.getOrElseUpdate(req.id, _reqTitle(req))
  }

  private val reqCodeGroupTitleMemo =
    new scala.collection.mutable.HashMap[ReqCodeId, Out]

  def reqCodeGroupTitle(g: ReqCodeGroup.AndId): Out =
    reqCodeGroupTitleMemo.getOrElseUpdate(g.id, format(g.group.title))

  def reqTitleById(id: ReqId): Must[Out] =
    project.reqs.data.reqM(id) map reqTitle

  private val _customTextField: CustomField.Text.Id => ReqId => Option[Out] =
    fid => {
      val m = project.reqFieldData.data.text.getOrElse(fid, Map.empty)
      m.get(_) map format1
    }

  val customTextField: CustomField.Text.Id => ReqId => Option[Out] =
    memo { fid => val g = _customTextField(fid); memo(g) }
}
