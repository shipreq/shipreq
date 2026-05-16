package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.{Reusability, Reusable}
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.webapp.client.project.feature.editor
import shipreq.webapp.client.project.feature.editor.{Editability => EE}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.util.DataReusability._

/** Determinations of whether or not a field is allowed to be edited.
  *
  * Each class herein just provides reusable compositions that eventually just reduce to
  * `FieldKey => Permission`.
  *
  * This is especially important on dense screens like ReqTable where having a reusable instance for all editable
  * fields per-row / per-req can prevent a lot of needless vdom re-calculation and processing.
  */
object Editability {

  def apply(cfg: ProjectConfig, globalPerm: Permission): ForProject =
    ForProject(cfg, globalPerm)

  final case class ForProject(cfg: ProjectConfig, globalPerm: Permission) {
    def apply(r: RowKey): ForFields[r.FieldKey] =
      r.foldF(RowKey.Fold(
        _ => forCodeGroup(globalPerm),
        x => forGenericReq(x.reqTypeId),
        _ => forUseCase,
        _ => forManualIssue(globalPerm),
      ))

    private def forGenericReq(reqTypeId: CustomReqTypeId): ForFields[FieldKey.ForGenericReq] =
      ForFields.via(
        EE.ForGenericReq(Some((cfg, reqTypeId)), globalPerm),
        FieldKey.editorFieldGR.reverseGet)

    private lazy val forUseCase: ForFields[FieldKey.ForUseCase] =
      ForFields.via(
        EE.ForUseCase(Some(cfg), globalPerm),
        FieldKey.editorFieldUC.reverseGet)
  }

  private def forCodeGroup(globalPerm: Permission): ForFields[FieldKey.ForCodeGroup] =
    ForFields.via(
      EE.ForCodeGroup(globalPerm),
      FieldKey.editorFieldCG.reverseGet)

  private val forManualIssue: Permission => ForFields[FieldKey.ForManualIssue] =
    Permission.memo(perm => ForFields(Reusable.byRef(_ => perm)))

  final case class ForFields[-FK <: FieldKey](fn: Reusable[FK => Permission]) {
    def apply(field: FK): Permission =
      fn(field)
  }

  object ForFields {
    def via[
        FK  <: FieldKey,
        EFK <: editor.FieldKey,
        E   <: EE.ForFields[EFK] : Reusability : ClassTag]
        (ee: E, map: FK => EFK): ForFields[FK] =
      ForFields[FK](Reusable.implicitly(ee).map(e => f => e(map(f))))
  }

  implicit val reusabilityForProject               : Reusability[ForProject        ] = Reusability.derive
  private  val reusabilityForFieldsAny             : Reusability[ForFields[Nothing]] = Reusability.derive
  implicit def reusabilityForFields[FK <: FieldKey]: Reusability[ForFields[FK]     ] = reusabilityForFieldsAny.narrow
}
