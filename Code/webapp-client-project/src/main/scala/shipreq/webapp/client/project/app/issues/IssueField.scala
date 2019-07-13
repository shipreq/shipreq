package shipreq.webapp.client.project.app.issues

import scalaz.{-\/, \/-}
import shipreq.base.util.{Backwards, Direction}
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey

final case class IssueField(key: FieldKey, desc: String)

object IssueField {

  val CodeGroupTitle  = IssueField(FieldKey.CodeGroupTitle , ColumnNames.title)
  val GenericReqTitle = IssueField(FieldKey.GenericReqTitle, ColumnNames.title)
  val UseCaseTitle    = IssueField(FieldKey.UseCaseTitle   , ColumnNames.title)
  val Tags            = IssueField(FieldKey.Tags(None)     , ColumnNames.tags)

  def customField(id: CustomFieldId)(implicit cfg: ProjectConfig): IssueField =
    customField(cfg.fields.customFields.need(id))

  def customField(cf: CustomField)(implicit cfg: ProjectConfig): IssueField =
    cf match {
      case f: CustomField.Text        => customField(f)
      case f: CustomField.Tag         => customField(f)
      case f: CustomField.Implication => customField(f)
    }

  def customField(f: CustomField.Text): IssueField =
    IssueField(FieldKey.CustomTextField(f. id), f.name)

  def customField(f: CustomField.Tag)(implicit cfg: ProjectConfig): IssueField =
    IssueField(FieldKey.Tags(Some(f.id)), f.name(cfg.tags.tree))

  def customField(f: CustomField.Implication)(implicit cfg: ProjectConfig): IssueField =
    IssueField(FieldKey.Implications(-\/(f.id)), f.name(cfg.reqTypes))

  val implications: Direction => IssueField =
    Direction.memo(d => IssueField(FieldKey.Implications(\/-(d)), ColumnNames.implications(d)))

  def impliedBy = implications(Backwards)

  def reqTextLoc(reqId: ReqId, loc: ReqTextLoc, p: Project): IssueField =
    loc match {
      case ReqTextLoc.Title                    => reqTitle(reqId)
      case ReqTextLoc.CustomTextField(fieldId) => customField(fieldId)(p.config)
      case ReqTextLoc.UseCaseStep(stepId)      => useCaseStep(stepId, p)
    }

  def reqTitle(id: ReqId): IssueField =
    id match {
      case _: GenericReqId => GenericReqTitle
      case _: UseCaseId    => UseCaseTitle
    }

  def tags(id: Option[CustomField.Tag.Id])(implicit cfg: ProjectConfig): IssueField =
    id match {
      case Some(i) => customField(i)
      case None    => Tags
    }

  def useCaseStep(id: UseCaseStepId, p: Project): IssueField = {
    val focus = p.content.reqs.useCases.focusStep(id)
    useCaseStep(focus)
  }

  def useCaseStep(focus: UseCaseStep.Focus): IssueField = {
    val label = focus.field.stepLabel(focus.uc.pubid.pos, focus.ploc, UseCaseStepLabelFmt.`N.m`)
    val desc  = "UC step " + label
    IssueField(FieldKey.UseCaseStep(focus.id), desc)
  }
}
