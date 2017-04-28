package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.extra.Reusability
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._

/** Determinations of whether or not a field is allowed to be edited.
  *
  * Each class herein just provides reusable compositions that eventually just reduce to
  * `CellKey => Permission`.
  *
  * This is especially important on dense screens like ReqTable where having a reusable instance for all editable
  * fields per-row / per-req can prevent a lot of needless vdom re-calculation and processing.
  */
object Editability {

  def apply(p: Project): ForProject =
    ForProject(p.config, p.reqs, p.reqCodes)

  final case class ForProject(cfg: ProjectConfig, reqs: Requirements, reqCodes: ReqCodes) {
    val forReqs = ForReqs(cfg, reqs)
    val forReqCodeGroups = ForReqCodeGroups(reqCodes)
    val forUseCaseSteps = ForUseCaseSteps(reqs.useCases)
  }

  implicit val reusabilityForProject: Reusability[ForProject] =
    Reusability.caseClass

  sealed abstract class ForRow[R <: RowKey] {
    def apply(cellKey: R#CellKeyConstraint): Permission
  }

  final case class ForReqs(cfg: ProjectConfig, reqs: Requirements) {
    def apply(id: ReqId): ForReq = {
      val req: Req = reqs.need(id)
      req.live(cfg.reqTypes) match {
        case Live => ForReq(Some((req.reqTypeId, cfg)))
        case Dead => ForReq(None)
      }
    }
  }

  implicit val reusabilityForReqs: Reusability[ForReqs] =
    Reusability.caseClass

  final case class ForReq(whenReqIsLive: Option[(ReqTypeId, ProjectConfig)]) extends ForRow[RowKey.Req] {

    def apply(k: CellKey.ForReq): Permission =
      whenReqIsLive match {
        case Some((reqTypeId, cfg)) =>

          def forField(fid: CustomFieldId): Permission =
            cfg.fields.get(fid) match {
              case Some(f) => Allow when f.applicable(reqTypeId).is(Applicable) && f.live(cfg).is(Live)
              case None => Deny // Field has been removed
            }

          k match {
            case CellKey.Code
               | CellKey.Title
               | CellKey.Tags(None)
               | CellKey.Implications   (\/-(_: Direction)) => Allow
            case CellKey.CustomTextField(fid)               => forField(fid)
            case CellKey.Implications   (-\/(fid))          => forField(fid)
            case CellKey.Tags           (Some(fid))         => forField(fid)
            case CellKey.ReqType =>
              reqTypeId match {
                case _: CustomReqTypeId => Allow
                case StaticReqType.UseCase => Deny
              }
          }
        case None => Deny
      }
  }

  implicit val reusabilityForReq: Reusability[ForReq] =
    Reusability.caseClass

  final case class ForReqCodeGroups(reqCodes: ReqCodes) extends AnyVal {
    def apply(id: ReqCodeId): ForReqCodeGroup =
      ForReqCodeGroup(
        reqCodes.getById(id) match {
          case Some(_: ReqCode.ActiveGroup) => Allow
          case Some(_: ReqCode.ActiveReq)
             | Some(_: ReqCode.Inactive)
             | None                         => Deny
        }
      )
  }

  implicit val reusabilityForReqCodeGroups: Reusability[ForReqCodeGroups] =
    Reusability.caseClass

  final case class ForReqCodeGroup(permission: Permission) extends ForRow[RowKey.ReqCodeGroup] {
    def apply(k: CellKey.ForReqCodeGroup): Permission =
      k match {
        case CellKey.Code
           | CellKey.Title => permission
      }
  }

  implicit val reusabilityForReqCodeGroup: Reusability[ForReqCodeGroup] =
    Reusability.caseClass

  final case class ForUseCaseSteps(useCases: UseCases) extends ForRow[RowKey.UseCaseSteps.type] {
    def apply(k: CellKey.UseCaseStep): Permission =
      Allow when useCases.focusStep(k.id).live.is(Live)
  }

  implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] =
    Reusability.caseClass
}
