package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{Key, Reusability, Reusable}
import scalaz.{-\/, \/-}
import shipreq.base.util.IfApplicable
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue._
import shipreq.webapp.base.UiText.{Issues => UI}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.EditorNavParent
import shipreq.webapp.client.project.widgets.ProjectWidgets

sealed trait Row {
  val issue: Issue
  val issueClassDesc: String
  def fieldOption: Option[IssueField[EditorFeature.FieldKey]]
  val actions: List[Action]

  val editor: (EditorFeature.ReadWrite.ForProject, Reusable[Px[ProjectWidgets.NoCtx]]) => Option[Reusable[IfApplicable[EditorNavParent.Props]]]

  final def issueCategoryDesc = UI.category(issue.category)
  final val key: Key = issue.hashCode
}

object Row {

  sealed trait ForReq extends Row {
    val req: Req
  }

  final case class ForGenericReq(issue         : Issue,
                                 issueClassDesc: String,
                                 req           : GenericReq,
                                 field         : IssueField[EditorFeature.FieldKey.ForGenericReq],
                                 renderer      : RenderFeature.ForGenericReq,
                                 actions       : List[Action]) extends ForReq {
    override val fieldOption = Some(field)
    override val editor = (e, pw) => Some(renderEditable(field.key)(renderer, e.forGenericReq(req.id), (), pw))
  }

  final case class ForUseCase(issue         : Issue,
                              issueClassDesc: String,
                              req           : UseCase,
                              field         : IssueField[EditorFeature.FieldKey.ForUseCase],
                              renderer      : RenderFeature.ForUseCase,
                              actions       : List[Action]) extends ForReq {
    override val fieldOption = Some(field)
    override val editor = (e, pw) => Some(renderEditable(field.key)(renderer, e.forUseCase(req.id), (), pw))
  }

  final case class ForUseCaseStep(issue         : Issue,
                                  issueClassDesc: String,
                                  req           : UseCase,
                                  field         : IssueField[EditorFeature.FieldKey.UseCaseStep],
                                  ucRenderer    : RenderFeature.ForUseCase,
                                  renderer      : RenderFeature.ForUseCaseSteps,
                                  actions       : List[Action]) extends ForReq {
    override val fieldOption = Some(field)
    override val editor = (e, pw) => Some(
      renderEditable(field.key)(renderer, e.forUseCaseSteps, EditorFeature.FieldKey.UseCaseStep.Args.empty, pw))
  }

  final case class ForRcg(issue         : Issue,
                          issueClassDesc: String,
                          rcg           : LiveCodeGroup,
                          fieldOption   : Option[IssueField[EditorFeature.FieldKey.ForCodeGroup]],
                          code          : ReqCode.Value,
                          renderer      : RenderFeature.ForCodeGroup,
                          actions       : List[Action]) extends Row {

    override val editor = (e, pw) =>
      fieldOption.map { f =>
        renderEditable(f.key)(renderer, e.forCodeGroup(rcg.id), (), pw)
      }
  }

  final case class ForConfig(issue         : Issue,
                             issueClassDesc: String,
                             actions       : List[Action]) extends Row {
    override def fieldOption = None
    override val editor = (_, _) => None
  }

  final case class ForManualIssue(issue   : Issue.ManualIssue,
                                  actions : List[Action],
                                  renderer: RenderFeature.ForManualIssues) extends Row {
    val field = IssueField.manual(issue.issue)
    override val issueClassDesc = UI.descManualIssue
    override def fieldOption = Some(field)
    override val editor = (e, pw) => Some(renderEditable(field.key)(renderer, e.forManualIssues, (), pw))
  }

  // ===================================================================================================================

  implicit def reusability: Reusability[Row] =
    Reusability.byRef

  private def renderEditable[FK <: EditorFeature.FieldKey](fk    : FK)
                                                          (render: RenderFeature.ForField[fk.RenderFieldKey],
                                                           editor: EditorFeature.ReadWrite.ForFields[FK],
                                                           args  : fk.Args,
                                                           pw    : Reusable[Px[ProjectWidgets.NoCtx]]): Reusable[IfApplicable[EditorNavParent.Props]] = {
    render(fk.forRender) match {
      case \/-(view) =>
        val e = editor(fk, pw, HideDead)
        Reusable.implicitly(e).map { e =>
          \/-(TableRow.renderEditor(Column.FieldEditor, view, e, args))
        }
      case e@ -\/(_) =>
        Reusable.implicitly(0).withValue(e)
    }
  }

  def fromIssue(p: Project, rf: RenderFeature.ForProject): Issue => Row = {
    implicit val cfg = p.config
    val actionBuilder = new Actions.Builder(p)

    def forReqA(i: Issue, desc: String, req: Req, fk: IssueField[EditorFeature.FieldKey.ForAllReqs]): ForReq =
      req match {
        case r: GenericReq => forGR(i, desc, r, fk)
        case r: UseCase    => forUC(i, desc, r, fk)
      }

    def forReqF(i: Issue, desc: String, req: Req)(fkGR: IssueField[EditorFeature.FieldKey.ForGenericReq],
                                                  fkUC: IssueField[EditorFeature.FieldKey.ForUseCase]): ForReq =
      req match {
        case r: GenericReq => forGR(i, desc, r, fkGR)
        case r: UseCase    => forUC(i, desc, r, fkUC)
      }

    def forReqTitle(i: Issue, desc: String, req: Req): ForReq =
      forReqF(i, desc, req)(IssueField.GenericReqTitle, IssueField.UseCaseTitle)

    def forGR(i: Issue, desc: String, req: GenericReq, fk: IssueField[EditorFeature.FieldKey.ForGenericReq]) =
      ForGenericReq(
        issue          = i,
        issueClassDesc = desc,
        req            = req,
        field          = fk,
        renderer       = rf.forGenericReq(req.id),
        actions        = actionBuilder(i))

    def forUC(i: Issue, desc: String, req: UseCase, fk: IssueField[EditorFeature.FieldKey.ForUseCase]) =
      ForUseCase(
        issue          = i,
        issueClassDesc = desc,
        req            = req,
        field          = fk,
        renderer       = rf.forUseCase(req.id),
        actions        = actionBuilder(i))

    def forReqAndLoc(i: Issue, desc: String, r: Req, loc: LocationOf.Text.InReq): Row =
      loc match {
        case Location.Text.Title                    => forReqTitle(i, desc, r)
        case Location.Text.CustomTextField(fieldId) => forReqA(i, desc, r, IssueField.customField(fieldId))
        case Location.Text.UseCaseStep(stepId)      => forUcsI(i, desc, stepId)
      }

    def forRcg(i: Issue, desc: String, g: LiveCodeGroup, fk: Option[IssueField[EditorFeature.FieldKey.ForCodeGroup]]) =
      ForRcg(
        issue          = i,
        issueClassDesc = desc,
        rcg            = g,
        fieldOption    = fk,
        code           = p.content.reqCodes.reqCode(g.id),
        renderer       = rf.forCodeGroup(g),
        actions        = actionBuilder(i))

    def forUcsI(i: Issue, desc: String, id: UseCaseStepId): ForUseCaseStep =
      forUcs(i, desc, p.content.reqs.useCases.focusStep(id))

    def forUcs(i: Issue, desc: String, f: UseCaseStep.Focus): ForUseCaseStep =
      ForUseCaseStep(
        issue          = i,
        issueClassDesc = desc,
        req            = f.uc,
        field          = IssueField.useCaseStep(f),
        ucRenderer     = rf.forUseCase(f.uc.id),
        renderer       = rf.forUseCaseSteps,
        actions        = actionBuilder(i))

    def forConfig(i: Issue, desc: String): ForConfig =
      ForConfig(
        issue          = i,
        issueClassDesc = desc,
        actions        = actionBuilder(i))

    // -----------------------------------------------------------------------------------------------------------------

    {
      case i: Issue.BlankCustomField =>
        val desc = UI.descBlankCustomField(cfg.fieldName(i.field.id))
        forReqA(i, desc, i.req, IssueField.customField(i.field.id))

      case i: Issue.BlankTitle =>
        forReqTitle(i, UI.descBlankTitle, i.req)

      case i: Issue.BlankUseCaseStep =>
        forUcs(i, UI.descBlankUseCaseStep, i.step)

      case i: Issue.ConflictingTags =>
        val tag   = cfg.tags.needTagGroup(i.tagGroupId)
        val desc  = UI.descConflictingTags(tag.name)
        val field = cfg.mostRelevantLiveFieldForTag(i.tagGroupId).map(_.id)
        forReqA(i, desc, i.req, IssueField.tags(field))

      case i: Issue.DeadIssueTagInRcg =>
        val it   = cfg.customIssueTypes.need(i.issue.typ)
        val desc = UI.descDeadIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(IssueField.CodeGroupTitle))

      case i: Issue.DeadIssueTagInReq =>
        val it   = cfg.customIssueTypes.need(i.issue.typ)
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

      case i: Issue.FieldDefaultTagDead =>
        val fieldName = cfg.fieldName(i.field.id)
        val desc = UI.descFieldDefaultTagDead(field = fieldName, tag = i.tag.name)
        forConfig(i, desc)

      case i: Issue.FieldDefaultTagUnrelated =>
        val fieldName = cfg.fieldName(i.field.id)
        val desc = UI.descFieldDefaultTagUnrelated(field = fieldName, tag = i.tag.name)
        forConfig(i, desc)

      case i: Issue.FieldDefaultTagNotApplicable =>
        val fieldName = cfg.fieldName(i.field.id)
        val desc = UI.descFieldDefaultTagNotApplicable(field = fieldName, tag = i.tag.name, reqType = i.reqType.mnemonic.value)
        forConfig(i, desc)

      case i: Issue.ImplicationRequired =>
        val reqType = cfg.reqTypes.need(i.req.reqTypeId)
        val desc = UI.descImplicationRequired(reqType.mnemonic)
        forReqA(i, desc, i.req, IssueField.impliedBy)

      case i: Issue.IssueTagInRcg =>
        val it   = cfg.customIssueTypes.need(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forRcg(i, desc, i.rcg, Some(IssueField.CodeGroupTitle))

      case i: Issue.IssueTagInReq =>
        val it   = cfg.customIssueTypes.need(i.issue.typ)
        val desc = UI.descIssueTag(it.key)
        forReqAndLoc(i, desc, i.req, i.loc)

      case i: Issue.ManualIssue =>
        ForManualIssue(i, actionBuilder(i), rf.forManualIssues)

      case i: Issue.NonApplicableField =>
        val fieldName = cfg.fieldName(i.field.id)
        val desc = UI.descNonApplicableField(fieldName)
        forConfig(i, desc)

      case i: Issue.UninhabitableTagField =>
        val fieldName = cfg.fieldName(i.field.id)
        val desc = UI.descUninhabitableTagField(fieldName)
        forConfig(i, desc)
    }
  }
}
