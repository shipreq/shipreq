package shipreq.webapp.client.project.feature.editor2

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
    val forCodeGroups = ForCodeGroups(reqCodes)
    val forUseCaseSteps = ForUseCaseSteps(reqs.useCases)
  }

  implicit val reusabilityForProject: Reusability[ForProject] =
    Reusability.caseClass

  sealed abstract class ForRow[R <: RowKey] {
    def apply(field: R#FieldKey): Permission
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

    def apply(k: FieldKey.ForReq): Permission =
      whenReqIsLive match {
        case Some((reqTypeId, cfg)) =>

          def forCustomField(fid: CustomFieldId): Permission =
            cfg.fields.get(fid) match {
              case Some(f) => Allow when cfg.applicability(reqTypeId, fid).is(Applicable) && f.live(cfg).is(Live)
              case None    => Deny // Field has been removed
            }

          k match {
            case FieldKey.Code
               | FieldKey.Title
               | FieldKey.Tags(None)
               | FieldKey.Implications   (\/-(_))    => Allow
            case FieldKey.Implications   (-\/(fid))  => forCustomField(fid)
            case FieldKey.CustomTextField(fid)       => forCustomField(fid)
            case FieldKey.Tags           (Some(fid)) => forCustomField(fid)
            case FieldKey.ReqType =>
              reqTypeId match {
                case _: CustomReqTypeId    => Allow
                case StaticReqType.UseCase => Deny
              }
          }
        case None => Deny
      }
  }

  implicit val reusabilityForReq: Reusability[ForReq] =
    Reusability.caseClass

  final case class ForCodeGroups(reqCodes: ReqCodes) extends AnyVal {
    def apply(id: ReqCodeId): ForCodeGroup =
      ForCodeGroup(
        reqCodes.getById(id) match {
          case Some(_: ReqCode.ActiveGroup) => Allow
          case Some(_: ReqCode.ActiveReq)
             | Some(_: ReqCode.Inactive)
             | None                         => Deny
        }
      )
  }

  implicit val reusabilityForCodeGroups: Reusability[ForCodeGroups] =
    Reusability.caseClass

  final case class ForCodeGroup(permission: Permission) extends ForRow[RowKey.CodeGroup] {
    def apply(k: FieldKey.ForCodeGroup): Permission =
      k match {
        case FieldKey.Code
           | FieldKey.Title => permission
      }
  }

  implicit val reusabilityForCodeGroup: Reusability[ForCodeGroup] =
    Reusability.caseClass

  final case class ForUseCaseSteps(useCases: UseCases) extends ForRow[RowKey.UseCaseSteps.type] {
    def apply(k: FieldKey.UseCaseStep): Permission =
      Allow when useCases.focusStep(k.id).live.is(Live)
  }

  implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] =
    Reusability.caseClass
}
