package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, Deny, Enabled}
import shipreq.webapp.base.ui.semantic.{Button => Btn, Icon}
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.CustomTagFieldGD
import shipreq.webapp.member.project.issue.{ContentRef, Issue}
import shipreq.webapp.member.project.text.PlainText
import shipreq.webapp.member.protocol.websocket.{ManualIssueCmd, UpdateConfigCmd, UpdateContentCmd}

sealed trait Action {
  def cmdOption: Option[Action.Cmd]
}

object Action {

  final case class Button(icon: Icon, label: String, cmd: Action.Cmd) extends Action {

    override val cmdOption =
      Some(cmd)

    val button: Enabled => VdomTag =
      Enabled.memo { enabled =>
        Btn(
          tipe = Btn.Type.IconAndText(icon, label),
          state = if (enabled is Enabled) Btn.State.Active else Btn.State.Disabled,
        ).tag(*.actionButton)
      }
  }

  final case class Link(routerCtl: Routes.RouterCtl, route: Routes.Page.HasStaticTitle) extends Action {

    override def cmdOption =
      None

    val render: VdomNode =
        Btn(
          tipe = Btn.Type.IconAndText(Icon.Setting, route.title),
        ).tag(*.actionButton, routerCtl.setOnLinkClick(route))
  }

  type Cmd = ManualIssueCmd \/ UpdateConfigCmd \/ UpdateContentCmd
}

object Actions {
  import Action.Cmd

  final class Builder(p: Project, routerCtl: Routes.RouterCtl) {
    type Actions = List[Action]

    private implicit def singleActionAsList(a: Action): Actions = a :: Nil
    private implicit def cmdFromUpdateConfig(a: UpdateConfigCmd): Cmd = -\/(\/-(a))
    private implicit def cmdFromUpdateContent(a: UpdateContentCmd): Cmd = \/-(a)
    private implicit def cmdFromManualIssueCmd(a: ManualIssueCmd): Cmd = -\/(-\/(a))

    private def linkTo(route: Routes.Page.HasStaticTitle): Action =
      Action.Link(routerCtl, route)

    private def delete(subject: String, cmd: Cmd): Action =
      Action.Button(Icon.Trash, "Delete " + subject, cmd)

    private def deleteField(f: CustomField): Action = {
      val name = p.config.fieldName(f.id)
      delete(
        name + " field",
        UpdateConfigCmd.CustomFieldDelete(f.id))
    }

    private def deleteReqCodeGroup(g: LiveCodeGroup): Action = {
      val code = p.content.reqCodes.reqCode(g.id)
      delete(
        PlainText.reqCode(code),
        UpdateContentCmd.DeleteCodeGroups(NonEmptySet.one(g.id)))
    }

    private def restore(subject: String, cmd: Cmd): Action =
      Action.Button(Icon.Undo, "Restore " + subject, cmd)

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

    private def deleteDerivativeTagRule(field: CustomField.Tag,
                                        k1: ApplicableTagId,
                                        k2: ApplicableTagId): Actions = {
      val dt = field.derivativeTags.withoutRuleFor(k1, k2)
      val newDT = CustomTagFieldGD.ValueForDerivativeTags(dt)
      val cmd = UpdateConfigCmd.CustomFieldUpdateTag(field.id, newDT)
      delete("the rule", cmd) :: Nil
    }

    // -----------------------------------------------------------------------------------------------------------------

    def apply(i: Issue): List[Action] = i match {

      case _: Issue.BlankTitle
         | _: Issue.BlankCustomField
         | _: Issue.BlankUseCaseStep
         | _: Issue.ConflictingTags
         | _: Issue.DuplicateTitle
         | _: Issue.ImplicationRequired
         | _: Issue.IssueTagInRcg
         | _: Issue.IssueTagInReq
            => Nil

      case _: Issue.FieldDefaultTagDead
         | _: Issue.FieldDefaultTagNotApplicable
         | _: Issue.FieldDefaultTagUnrelated
         | _: Issue.NonApplicableField
            => linkTo(Routes.Page.CfgFields)

      case i: Issue.DeadIssueTagInRcg            => restoreIssueTag(i.issue.typ)
      case i: Issue.DeadIssueTagInReq            => restoreIssueTag(i.issue.typ)
      case i: Issue.DeadRefInRcg                 => restoreRefTarget(i.ref)
      case i: Issue.DeadRefInReq                 => restoreRefTarget(i.ref)
      case i: Issue.DeadTag                      => restoreTag(i.tag)
      case i: Issue.DerivativeTagResultDead      => deleteDerivativeTagRule(i.field, i.key1.id, i.key2.id)
      case i: Issue.DerivativeTagResultUnrelated => deleteDerivativeTagRule(i.field, i.key1.id, i.key2.id)
      case i: Issue.EmptyCodeGroup               => deleteReqCodeGroup(i.rcg)
      case i: Issue.ManualIssue                  => delete("issue", ManualIssueCmd.Delete(i.issue.id))
      case _: Issue.NonApplicableTag             => linkTo(Routes.Page.CfgTags)
      case i: Issue.UninhabitableTagField        => deleteField(i.field)
    }
  }
}
