package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.{Reusability, Reusable}
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._

/** Determinations of whether or not a field is allowed to be edited.
  *
  * Each class herein just provides reusable compositions that eventually just reduce to
  * `FieldKey => Permission`.
  *
  * This is especially important on dense screens like ReqTable where having a reusable instance for all editable
  * fields per-row / per-req can prevent a lot of needless vdom re-calculation and processing.
  */
object Editability {

  def apply(p: Project): ForProject =
    ForProject(p.config, p.content.reqs, p.content.reqCodes)

  final case class ForProject(cfg: ProjectConfig, reqs: Requirements, reqCodes: ReqCodes) {
    val forReqs         = ForReqs(cfg, reqs)
    val forCodeGroups   = ForCodeGroups(reqCodes)
    val forUseCaseSteps = ForUseCaseSteps(reqs.useCases)

    def apply(row: RowKey): ForFields[FieldKey] =
      row match {
        case RowKey.CodeGroup(id)  => forCodeGroups(id).widen
        case RowKey.GenericReq(id) => forReqs(id).widen
        case RowKey.UseCase(id)    => forReqs(id).widen
        case RowKey.UseCaseSteps   => forUseCaseSteps.widen
        case RowKey.ManualIssues   => forManualIssues.value.widen
      }
  }

  val forManualIssues: Reusable[ForFields[FieldKey.ManualIssue]] =
    Reusable.byRef(_ => Allow)

  final case class ForReqs(cfg: ProjectConfig, reqs: Requirements) {

    def apply(id: GenericReqId): ForGenericReq = {
      val req: GenericReq = reqs.genericReqs.imap.need(id)
      req.live(cfg.reqTypes) match {
        case Live => ForGenericReq(Some((cfg, req.reqTypeId)))
        case Dead => ForGenericReq(None)
      }
    }

    def apply(id: UseCaseId): ForUseCase = {
      val req: UseCase = reqs.useCases.imap.need(id)
      req.live(cfg.reqTypes) match {
        case Live => ForUseCase(Some(cfg))
        case Dead => ForUseCase(None)
      }
    }
  }

  trait ForFields[-FK <: FieldKey] {
    def apply(field: FK): Permission
  }

  implicit class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
    def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
      new ForFields[W] {
        val newFn = t.widenFn[W, Permission](self.apply)(Deny)
        override def apply(field: W) = newFn(field)
      }
  }

  final case class ForGenericReq(whenReqIsLive: Option[(ProjectConfig, CustomReqTypeId)]) extends ForFields[FieldKey.ForGenericReq] {
    override def apply(k: FieldKey.ForGenericReq): Permission =
      whenReqIsLive match {
        case Some((cfg, reqTypeId)) =>
          k match {
            case FieldKey.Codes
               | FieldKey.GenericReqTitle
               | FieldKey.ReqType
               | FieldKey.Implications   (\/-(_))   => Allow
            case FieldKey.Implications   (-\/(fid)) => customField(cfg, reqTypeId, fid)
            case FieldKey.CustomTextField(fid)      => customField(cfg, reqTypeId, fid)
            case FieldKey.CustomFieldTags(fid)      => customField(cfg, reqTypeId, fid)
            case FieldKey.AllTags                   => staticField(cfg, StaticField.AllTags)
            case FieldKey.OtherTags                 => staticField(cfg, StaticField.OtherTags)
          }
        case None => Deny
      }
  }

  final case class ForUseCase(whenReqIsLive: Option[ProjectConfig]) extends ForFields[FieldKey.ForUseCase] {
    private def reqTypeId = StaticReqType.UseCase

    override def apply(k: FieldKey.ForUseCase): Permission =
      whenReqIsLive match {
        case Some(cfg) =>
          k match {
            case FieldKey.Codes
               | FieldKey.UseCaseTitle
               | FieldKey.Implications   (\/-(_))   => Allow
            case FieldKey.Implications   (-\/(fid)) => customField(cfg, reqTypeId, fid)
            case FieldKey.CustomTextField(fid)      => customField(cfg, reqTypeId, fid)
            case FieldKey.CustomFieldTags(fid)      => customField(cfg, reqTypeId, fid)
            case FieldKey.AllTags                   => staticField(cfg, StaticField.AllTags)
            case FieldKey.OtherTags                 => staticField(cfg, StaticField.OtherTags)
          }
        case None => Deny
      }
  }

  private def staticField(cfg: ProjectConfig, f: StaticField): Permission =
    Allow when cfg.fields.includes(f)

  private def customField(cfg: ProjectConfig, reqTypeId: ReqTypeId, fid: CustomFieldId): Permission =
    cfg.fields.get(fid) match {
      case Some(f) => Allow when cfg.applicability(reqTypeId, fid).is(Applicable) && f.live(cfg).is(Live)
      case None    => Deny // Field has been removed
    }

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

  final case class ForCodeGroup(permission: Permission) extends ForFields[FieldKey.ForCodeGroup] {
    def apply(k: FieldKey.ForCodeGroup): Permission =
      k match {
        case FieldKey.Code
           | FieldKey.CodeGroupTitle => permission
      }
  }

  final case class ForUseCaseSteps(useCases: UseCases) extends ForFields[FieldKey.UseCaseStep] {
    def apply(k: FieldKey.UseCaseStep): Permission =
      Allow when useCases.focusStep(k.id).live.is(Live)
  }

  implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.derive
  implicit val reusabilityForReqs        : Reusability[ForReqs        ] = Reusability.derive
  implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.derive
  implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.derive
  implicit val reusabilityForCodeGroups  : Reusability[ForCodeGroups  ] = Reusability.derive
  implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.derive
  implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.derive
}
