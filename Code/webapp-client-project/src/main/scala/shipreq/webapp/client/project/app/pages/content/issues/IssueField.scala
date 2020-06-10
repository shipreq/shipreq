package shipreq.webapp.client.project.app.pages.content.issues

import scalaz.{-\/, \/-}
import shipreq.base.util.{Backwards, Direction}
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey

final case class IssueField[+FK <: FieldKey](key: FK, desc: Option[String])

object IssueField {
  import DataImplicits._

  val CodeGroupTitle  = IssueField(FieldKey.CodeGroupTitle , Some(SpecialBuiltInField.Title.name))
  val GenericReqTitle = IssueField(FieldKey.GenericReqTitle, Some(SpecialBuiltInField.Title.name))
  val UseCaseTitle    = IssueField(FieldKey.UseCaseTitle   , Some(SpecialBuiltInField.Title.name))
  val OtherTags       = IssueField(FieldKey.OtherTags      , Some(StaticField.OtherTags.name))

  def customField(id: CustomFieldId)(implicit cfg: ProjectConfig): IssueField[FieldKey.ForAllReqs] =
    customField(cfg.fields.customFields.need(id))

  def customField(cf: CustomField)(implicit cfg: ProjectConfig): IssueField[FieldKey.ForAllReqs] =
    cf match {
      case f: CustomField.Text        => customField(f)
      case f: CustomField.Tag         => customField(f)
      case f: CustomField.Implication => customField(f)
    }

  def customField(id: CustomField.Text.Id)(implicit cfg: ProjectConfig): IssueField[FieldKey.CustomTextField] =
    customField(cfg.fields.custom(id))

  def customField(id: CustomField.Tag.Id)(implicit cfg: ProjectConfig): IssueField[FieldKey.CustomFieldTags] =
    customField(cfg.fields.custom(id))

  def customField(id: CustomField.Implication.Id)(implicit cfg: ProjectConfig): IssueField[FieldKey.Implications] =
    customField(cfg.fields.custom(id))

  def customField(f: CustomField.Text): IssueField[FieldKey.CustomTextField] =
    IssueField(FieldKey.CustomTextField(f. id), Some(f.name))

  def customField(f: CustomField.Tag)(implicit cfg: ProjectConfig): IssueField[FieldKey.CustomFieldTags] =
    IssueField(FieldKey.CustomFieldTags(f.id), Some(f.name(cfg.tags.tree)))

  def customField(f: CustomField.Implication)(implicit cfg: ProjectConfig): IssueField[FieldKey.Implications] =
    IssueField(FieldKey.Implications(-\/(f.id)), Some(f.name(cfg.reqTypes)))

  val implications: Direction => IssueField[FieldKey.Implications] =
    Direction.memo(d => IssueField(FieldKey.Implications(\/-(d)), Some(SpecialBuiltInField.implication(d).name)))

  def impliedBy = implications(Backwards)

  def useCaseStep(id: UseCaseStepId, p: Project): IssueField[FieldKey.UseCaseStep] = {
    val focus = p.content.reqs.useCases.focusStep(id)
    useCaseStep(focus)
  }

  def useCaseStep(focus: UseCaseStep.Focus): IssueField[FieldKey.UseCaseStep] = {
    val label = focus.field.stepLabel(focus.uc.pubid.pos, focus.ploc, UseCaseStepLabelFmt.`N.m`)
    val desc  = "UC step " + label
    IssueField(FieldKey.UseCaseStep(focus.id), Some(desc))
  }

  def manual(issue: ManualIssue): IssueField[FieldKey.ManualIssue] =
    IssueField(FieldKey.ManualIssue(issue.id), None)
}
