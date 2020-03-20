package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.scalajs.react.vdom.Implicits._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{Allow, Deny}
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue.{ContentRef, Issue}
import shipreq.webapp.base.protocol.websocket.{ManualIssueCmd, UpdateConfigCmd, UpdateContentCmd}
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.semantic.{Button, Icon}
import shipreq.webapp.client.project.app.Style.{issues => *}

final case class Action(icon: Icon, label: String, cmd: Action.Cmd) {
  val button =
    Enabled.memo { enabled =>
      Button(
        tipe = Button.Type.IconAndText(icon, label),
        state = if (enabled is Enabled) Button.State.Active else Button.State.Disabled,
      ).tag(*.actionButton)
    }
}

object Action {
  type Cmd = ManualIssueCmd \/ UpdateConfigCmd \/ UpdateContentCmd
}

object Actions {
  import Action.Cmd

  final class Builder(p: Project) {
    type Actions = List[Action]

    private implicit def singleActionAsList(a: Action): Actions = a :: Nil
    private implicit def cmdFromUpdateConfig(a: UpdateConfigCmd): Cmd = -\/(\/-(a))
    private implicit def cmdFromUpdateContent(a: UpdateContentCmd): Cmd = \/-(a)
    private implicit def cmdFromManualIssueCmd(a: ManualIssueCmd): Cmd = -\/(-\/(a))

    private def delete(subject: String, cmd: Cmd): Action =
      Action(Icon.Trash, "Delete " + subject, cmd)

    private def deleteField(f: CustomField): Action = {
      val name = p.config.fieldName(f.id)
      delete(
        name + " field",
        UpdateConfigCmd.FieldDelete(f.id))
    }

    private def deleteReqCodeGroup(g: LiveCodeGroup): Action = {
      val code = p.content.reqCodes.reqCode(g.id)
      delete(
        PlainText.reqCode(code),
        UpdateContentCmd.DeleteCodeGroups(NonEmptySet.one(g.id)))
    }

    private def restore(subject: String, cmd: Cmd): Action =
      Action(Icon.Undo, "Restore " + subject, cmd)

    private def restoreIssueTag(id: CustomIssueTypeId): Actions = {
      val t = p.config.customIssueTypes.need(id)
      restore(
        PlainText.hashtag(t.key),
        UpdateConfigCmd.CustomIssueTypeRestore(id))
    }

    private def restoreReq(id: ReqId): Actions =
      restoreReq(p.content.reqs.need(id))

    private def restoreReq(req: Req): Actions =
      req.allowLiveChange(p.config.reqTypes) match {
        case Allow =>
          restore(
            PlainText.pubidByReqId(req.id, p),
            UpdateContentCmd.RestoreContent(Set.empty + req.id, Set.empty))
        case Deny => Nil
      }

    private def restoreTag(t: ApplicableTag): Actions =
      restore(
        PlainText.hashtag(t.key),
        UpdateConfigCmd.TagRestore(t.id))

    private def restoreUseCaseStep(id: UseCaseStepId): Actions =
      restoreUseCaseStep(p.content.reqs.useCases.focusStep(id))

    private def restoreUseCaseStep(f: UseCaseStep.Focus): Actions =
      f.uc.liveExplicitly match {
        case Dead => restoreReq(f.uc.id)
        case Live => f.step.liveExplicitly match {
          case Dead =>
            restore(
              f.label(UseCaseStepLabelFmt.`UC-N.m`),
              UpdateContentCmd.RestoreUseCaseStep(f.id))
          case Live => Nil
        }
      }

    private def restoreReqCode(id: ReqCodeId): Actions = {
      val code = p.content.reqCodes.reqCode(id)
      val data = p.content.reqCodes.need(code)
      data.deadGroup match {
        case Some(g) =>
          restore(
            PlainText.reqCode(code),
            UpdateContentCmd.RestoreContent(Set.empty, Set.empty + g.id))
        case None => Nil
      }
    }

    private val restoreRefTarget: ContentRef => Actions = {
      case ContentRef.ReqRef        (id) => restoreReq(id)
      case ContentRef.CodeRef       (id) => restoreReqCode(id)
      case ContentRef.UseCaseStepRef(id) => restoreUseCaseStep(id)
    }

    // -----------------------------------------------------------------------------------------------------------------

    def apply(i: Issue): List[Action] = i match {

      case _: Issue.BlankTitle
         | _: Issue.BlankCustomField
         | _: Issue.BlankUseCaseStep
         | _: Issue.ConflictingTags
         | _: Issue.FieldDefaultTagDead
         | _: Issue.ImplicationRequired
         | _: Issue.IssueTagInRcg
         | _: Issue.IssueTagInReq
            => Nil

      case i: Issue.DeadIssueTagInRcg     => restoreIssueTag(i.issue.typ)
      case i: Issue.DeadIssueTagInReq     => restoreIssueTag(i.issue.typ)
      case i: Issue.DeadRefInRcg          => restoreRefTarget(i.ref)
      case i: Issue.DeadRefInReq          => restoreRefTarget(i.ref)
      case i: Issue.DeadTag               => restoreTag(i.tag)
      case i: Issue.EmptyCodeGroup        => deleteReqCodeGroup(i.rcg)
      case i: Issue.ManualIssue           => delete("issue", ManualIssueCmd.Delete(i.issue.id))
      case i: Issue.NonApplicableField    => deleteField(i.field)
      case i: Issue.UninhabitableTagField => deleteField(i.field)
    }
  }
}
