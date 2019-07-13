package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react.Reusability
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue._
import shipreq.webapp.base.UiText.{Issues => UI}
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.widgets.ViewReq

sealed trait Row {
  val issue: Issue
  val issueClassDesc: String
  def fieldKeyOption: Option[FieldKey]
  // val actions: List[Action]

  def issueCategoryDesc = UI.category(issue.category)
}

object Row {
  final case class ForConfig(issue         : Issue,
                             issueClassDesc: String) extends Row {
    override def fieldKeyOption = None
  }

  final case class ForReq(issue         : Issue,
                          issueClassDesc: String,
                          req           : Req,
                          fieldKey      : FieldKey,
                          viewReq       : ViewReq.Data) extends Row {
    override val fieldKeyOption = Some(fieldKey)
  }

  final case class ForRcg(issue         : Issue,
                          issueClassDesc: String,
                          rcg           : LiveCodeGroup,
                          fieldKeyOption: Option[FieldKey],
                          code          : ReqCode.Value) extends Row

  implicit def reusability: Reusability[Row] =
    Reusability.byRef

  def fromIssue(p: Project): Issue => Row = {
    val customFieldName = CustomField.nameP(p)

    def forReq(i: Issue, desc: String, r: Req, fk: FieldKey) =
      ForReq(i, desc, r, fk, ViewReq.Data.fromProject(r, p, HideDead))

    def forReqAndLoc(i: Issue, desc: String, r: Req, loc: ReqTextLoc) =
      forReq(i, desc, r, FieldKey.reqTextLoc(r.id, loc))

    def forRcg(i: Issue, desc: String, g: LiveCodeGroup, fk: Option[FieldKey]) =
      ForRcg(i, desc, g, fk, p.content.reqCodes.reqCode(g.id))

    {
      case i: Issue.BlankCustomField =>
        val desc = UI.descBlankCustomField(customFieldName(i.field))
        forReq(i, desc, i.req, FieldKey.customField(i.field.id))

      case i: Issue.BlankTitle =>
        forReq(i, UI.descBlankTitle, i.req, FieldKey.reqTitle(i.req.id))

      case i: Issue.BlankUseCaseStep =>
        forReq(i, UI.descBlankUseCaseStep, i.step.uc, FieldKey.UseCaseStep(i.step.id))

      case i: Issue.ConflictingTags =>
        val tag   = p.config.tags.needTagGroup(i.tagGroupId)
        val desc  = UI.descConflictingTags(tag.name)
        val field = p.config.mostRelevantLiveFieldForTag(i.tagGroupId).map(_.id)
        forReq(i, desc, i.req, FieldKey.Tags(field))

      case i: Issue.DeadIssueTagInRcg =>
        val it   = p.config.customIssueType(i.issue.typ)
        val desc = UI.descDeadIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(FieldKey.CodeGroupTitle))

      case i: Issue.DeadIssueTagInReq =>
        val it   = p.config.customIssueType(i.issue.typ)
        val desc = UI.descDeadIssueTag(it.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.DeadRefInRcg =>
        forRcg(i, UI.descDeadRef, i.rcg, Some(FieldKey.CodeGroupTitle))

      case i: Issue.DeadRefInReq =>
        forReqAndLoc(i, UI.descDeadRef, i.req, i.loc)

      case i: Issue.DeadTag =>
        val desc = UI.descDeadTag(i.tag.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.EmptyCodeGroup =>
        forRcg(i, UI.descEmptyCodeGroup, i.rcg, None)

      case i: Issue.ImplicationRequired =>
        val reqType = p.config.reqTypes.need(i.req.reqTypeId)
        val desc = UI.descImplicationRequired(reqType.mnemonic)
        forReq(i, desc, i.req, FieldKey.impliedBy)

      case i: Issue.IssueTagInRcg =>
        val it   = p.config.customIssueType(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(FieldKey.CodeGroupTitle))

      case i: Issue.IssueTagInReq =>
        val it   = p.config.customIssueType(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.UninhabitableTagField =>
        val fieldName = i.field.name(p.config.tags.tree)
        val desc = UI.descUninhabitableTagField(fieldName)
        ForConfig(i, desc)
    }
  }
}
