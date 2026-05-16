package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.{Reusability, Reusable}
import shipreq.base.util._
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.member.project.data._

/** Determinations of whether or not a field is allowed to be edited.
  *
  * Each class herein just provides reusable compositions that eventually just reduce to
  * `FieldKey => Permission`.
  *
  * This is especially important on dense screens like ReqTable where having a reusable instance for all editable
  * fields per-row / per-req can prevent a lot of needless vdom re-calculation and processing.
  */
object Editability {

  def apply(p: Project, globalPerm: Permission): ForProject =
    ForProject(p.config, p.content.reqs, p.content.reqCodes, globalPerm)

  final case class ForProject(cfg: ProjectConfig, reqs: Requirements, reqCodes: ReqCodes, globalPerm: Permission) {
    val forReqs         = ForReqs(cfg, reqs, globalPerm)
    val forCodeGroups   = ForCodeGroups(reqCodes, globalPerm)
    val forUseCaseSteps = ForUseCaseSteps(reqs.useCases, globalPerm)

    val forManualIssues: Reusable[ForFields[FieldKey.ManualIssue]] =
      forManualIssueCache(globalPerm)

    def apply(row: RowKey): ForFields[FieldKey] =
      row match {
        case RowKey.CodeGroup(id)  => forCodeGroups(id).widen
        case RowKey.GenericReq(id) => forReqs(id).widen
        case RowKey.UseCase(id)    => forReqs(id).widen
        case RowKey.UseCaseSteps   => forUseCaseSteps.widen
        case RowKey.ManualIssues   => forManualIssues.value.widen
      }
  }

  private val forManualIssueCache: Permission => Reusable[ForFields[FieldKey.ManualIssue]] =
    Permission.memo(perm => Reusable.byRef(_ => perm))

  final case class ForReqs(cfg: ProjectConfig, reqs: Requirements, globalPerm: Permission) {

    def apply(id: GenericReqId): ForGenericReq = {
      val req: GenericReq = reqs.genericReqs.imap.need(id)
      req.live(cfg.reqTypes) match {
        case Live => ForGenericReq(Some((cfg, req.reqTypeId)), globalPerm)
        case Dead => ForGenericReq(None, globalPerm)
      }
    }

    def apply(id: UseCaseId): ForUseCase = {
      val req: UseCase = reqs.useCases.imap.need(id)
      req.live(cfg.reqTypes) match {
        case Live => ForUseCase(Some(cfg), globalPerm)
        case Dead => ForUseCase(None, globalPerm)
      }
    }
  }

  trait ForFields[-FK <: FieldKey] {
    def apply(field: FK): Permission
  }

  implicit final class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
    def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
      new ForFields[W] {
        val newFn = t.widenFn[W, Permission](self.apply)(Deny)
        override def apply(field: W) = newFn(field)
      }
  }

  final case class ForGenericReq(whenReqIsLive: Option[(ProjectConfig, CustomReqTypeId)], globalPerm: Permission) extends ForFields[FieldKey.ForGenericReq] {
    override def apply(k: FieldKey.ForGenericReq): Permission =
      whenReqIsLive match {
        case Some((cfg, reqTypeId)) =>
          k match {
            case FieldKey.Codes
               | FieldKey.GenericReqTitle
               | FieldKey.ReqType
               | FieldKey.Implications   (\/-(_))   => globalPerm
            case FieldKey.Implications   (-\/(fid)) => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.CustomTextField(fid)      => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.CustomFieldTags(fid)      => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.AllTags                   => staticField(cfg, StaticField.AllTags, globalPerm)
            case FieldKey.OtherTags                 => staticField(cfg, StaticField.OtherTags, globalPerm)
          }
        case None => Deny
      }
  }

  final case class ForUseCase(whenReqIsLive: Option[ProjectConfig], globalPerm: Permission) extends ForFields[FieldKey.ForUseCase] {
    private def reqTypeId = StaticReqType.UseCase

    override def apply(k: FieldKey.ForUseCase): Permission =
      whenReqIsLive match {
        case Some(cfg) =>
          k match {
            case FieldKey.Codes
               | FieldKey.UseCaseTitle
               | FieldKey.Implications   (\/-(_))   => globalPerm
            case FieldKey.Implications   (-\/(fid)) => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.CustomTextField(fid)      => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.CustomFieldTags(fid)      => customField(cfg, reqTypeId, fid, globalPerm)
            case FieldKey.AllTags                   => staticField(cfg, StaticField.AllTags, globalPerm)
            case FieldKey.OtherTags                 => staticField(cfg, StaticField.OtherTags, globalPerm)
          }
        case None => Deny
      }
  }

  private def staticField(cfg: ProjectConfig, f: StaticField, globalPerm: Permission): Permission =
    Allow.when(cfg.fields.includes(f)) & globalPerm

  private def customField(cfg: ProjectConfig, reqTypeId: ReqTypeId, fid: CustomFieldId, globalPerm: Permission): Permission =
    (cfg.fields.get(fid) match {
      case Some(f) => Allow when cfg.applicability(reqTypeId, fid).is(Applicable) && f.live(cfg).is(Live)
      case None    => Deny // Field has been removed
    }) & globalPerm

  final case class ForCodeGroups(reqCodes: ReqCodes, globalPerm: Permission) {
    def apply(id: ReqCodeId): ForCodeGroup =
      ForCodeGroup(
        reqCodes.getById(id) match {
          case Some(_: ReqCode.ActiveGroup) => globalPerm
          case Some(_: ReqCode.ActiveReq)
             | Some(_: ReqCode.Inactive)
             | None                         => Deny
        }
      )
  }

  final case class ForCodeGroup(permission: Permission) extends ForFields[FieldKey.ForCodeGroup] {
    def apply(k: FieldKey.ForCodeGroup): Permission =
      k match {
        case FieldKey.Code
           | FieldKey.CodeGroupTitle => permission
      }
  }

  final case class ForUseCaseSteps(useCases: UseCases, globalPerm: Permission) extends ForFields[FieldKey.UseCaseStep] {
    def apply(k: FieldKey.UseCaseStep): Permission =
      Allow.when(useCases.focusStep(k.id).live.is(Live)) & globalPerm
  }

  implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.derive
  implicit val reusabilityForReqs        : Reusability[ForReqs        ] = Reusability.derive
  implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.derive
  implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.derive
  implicit val reusabilityForCodeGroups  : Reusability[ForCodeGroups  ] = Reusability.derive
  implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.derive
  implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.derive
}
