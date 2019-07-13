package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react.Reusability
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue._
import shipreq.webapp.base.UiText.{Issues => UI}
import shipreq.webapp.client.project.widgets.ViewReq

sealed trait Row {
  val issue: Issue
  val issueClassDesc: String
  def fieldOption: Option[IssueField]
  // val actions: List[Action]

  def issueCategoryDesc = UI.category(issue.category)
}

object Row {
  final case class ForConfig(issue         : Issue,
                             issueClassDesc: String) extends Row {
    override def fieldOption = None
  }

  final case class ForReq(issue         : Issue,
                          issueClassDesc: String,
                          req           : Req,
                          field         : IssueField,
                          viewReq       : ViewReq.Data) extends Row {
    override val fieldOption = Some(field)
  }

  final case class ForRcg(issue         : Issue,
                          issueClassDesc: String,
                          rcg           : LiveCodeGroup,
                          fieldOption   : Option[IssueField],
                          code          : ReqCode.Value) extends Row

  implicit def reusability: Reusability[Row] =
    Reusability.byRef

  def fromIssue(p: Project): Issue => Row = {
    implicit val cfg = p.config
    val customFieldName = CustomField.nameP(p)

    def forReq(i: Issue, desc: String, r: Req, fk: IssueField) =
      ForReq(i, desc, r, fk, ViewReq.Data.fromProject(r, p, HideDead))

    def forReqAndLoc(i: Issue, desc: String, r: Req, loc: ReqTextLoc) =
      forReq(i, desc, r, IssueField.reqTextLoc(r.id, loc, p))

    def forRcg(i: Issue, desc: String, g: LiveCodeGroup, fk: Option[IssueField]) =
      ForRcg(i, desc, g, fk, p.content.reqCodes.reqCode(g.id))

    {
      case i: Issue.BlankCustomField =>
        val desc = UI.descBlankCustomField(customFieldName(i.field))
        forReq(i, desc, i.req, IssueField.customField(i.field.id))

      case i: Issue.BlankTitle =>
        forReq(i, UI.descBlankTitle, i.req, IssueField.reqTitle(i.req.id))

      case i: Issue.BlankUseCaseStep =>
        forReq(i, UI.descBlankUseCaseStep, i.step.uc, IssueField.useCaseStep(i.step))

      case i: Issue.ConflictingTags =>
        val tag   = cfg.tags.needTagGroup(i.tagGroupId)
        val desc  = UI.descConflictingTags(tag.name)
        val field = cfg.mostRelevantLiveFieldForTag(i.tagGroupId).map(_.id)
        forReq(i, desc, i.req, IssueField.tags(field))

      case i: Issue.DeadIssueTagInRcg =>
        val it   = cfg.customIssueType(i.issue.typ)
        val desc = UI.descDeadIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(IssueField.CodeGroupTitle))

      case i: Issue.DeadIssueTagInReq =>
        val it   = cfg.customIssueType(i.issue.typ)
        val desc = UI.descDeadIssueTag(it.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.DeadRefInRcg =>
        forRcg(i, UI.descDeadRef, i.rcg, Some(IssueField.CodeGroupTitle))

      case i: Issue.DeadRefInReq =>
        forReqAndLoc(i, UI.descDeadRef, i.req, i.loc)

      case i: Issue.DeadTag =>
        val desc = UI.descDeadTag(i.tag.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.EmptyCodeGroup =>
        forRcg(i, UI.descEmptyCodeGroup, i.rcg, None)

      case i: Issue.ImplicationRequired =>
        val reqType = cfg.reqTypes.need(i.req.reqTypeId)
        val desc = UI.descImplicationRequired(reqType.mnemonic)
        forReq(i, desc, i.req, IssueField.impliedBy)

      case i: Issue.IssueTagInRcg =>
        val it   = cfg.customIssueType(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(IssueField.CodeGroupTitle))

      case i: Issue.IssueTagInReq =>
        val it   = cfg.customIssueType(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.UninhabitableTagField =>
        val fieldName = i.field.name(cfg.tags.tree)
        val desc = UI.descUninhabitableTagField(fieldName)
        ForConfig(i, desc)
    }
  }
}
