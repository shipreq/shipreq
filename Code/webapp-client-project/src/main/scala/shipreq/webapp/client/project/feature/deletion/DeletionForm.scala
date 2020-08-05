package shipreq.webapp.client.project.feature.deletion

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.NaTags
import shipreq.webapp.base.feature.{EditControlsFeature, PreviewFeature}
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.protocol.websocket.UpdateContentCmd.DeleteReqs
import shipreq.webapp.base.text.TextSearch
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon}
import shipreq.webapp.client.project.app.Style.{deletionForm => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature.Selection
import shipreq.webapp.client.project.widgets.{ProjectWidgets, RichTextEditor}

object DeletionForm {
  import DeletionRestorationLogic.Data

  final case class Props(data      : Data,
                         widgets   : ProjectWidgets.NoCtx,
                         textSearch: TextSearch,
                         perform   : DeleteReqs => Callback,
                         cancel    : Callback) {
    def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(selectedReqs: Selection[ReqId], reason: String)

  object State {
    def init(p: Props): State =
      apply(Selection(p.data.initialReqs), "")
  }

  final class Backend($: BackendScope[Props, State]) {
    private val setReqSel = Reusable.fn.state($ zoomStateL State.selectedReqs).set
    private val setReason = Reusable.fn.state($ zoomStateL State.reason).setStateFn

    private def reasonEditorProps(p: Props, s: State): RichTextEditor.DeletionReason.Optional =
      RichTextEditor.DeletionReason.Optional(
        project            = p.data.project,
        naTags             = NaTags.none,
        plainTextNoCtx     = p.widgets.plainText,
        textSearch         = p.textSearch,
        projectWidgets     = p.widgets,
        edit               = StateSnapshot.withReuse(s.reason)(setReason),
        asyncStatus        = None,
        abort              = None,
        abortConfirmation  = None,
        autoFocus          = true,
        commitFn           = None,
        commitVerb         = "",
        editorStyle        = EditControlsFeature.Style.default,
        preview            = PreviewFeature.ReadWrite.Single.alwaysShow,
        preEditValue       = None,
        extraKbShortcuts   = KeyboardTheme.Shortcuts.empty,
        showInstructions   = true,
        optionalFullscreen = None)

    private def renderReqTable(p: Props, selectedReqs: Selection[ReqId]): VdomElement =
      SharedUI.reqTable(
        Delete,
        p.data.project,
        p.widgets,
        p.data.actionableReqs,
        selectedReqs,
        selectedReqs.updateBy(setReqSel).legal(p.data.optionalReqIds),
        *.reqTableRow(_))

    private val cancelButton: VdomTag =
      Button(
        tipe = Button.Type.BasicIconAndText(Icon.Remove, UiText.buttonAbortChange),
        colour = Colour.Black
      ).tag(*.cancelButton, ^.onClick --> $.props.flatMap(_.cancel))

    def render(p: Props, s: State): VdomElement = {
      assert(p.data.actionableGroups.isEmpty,
        "Since proper UI/UX implementation, DeletionForm no longer accepts deletable code-groups")

      val reasonTextProps = reasonEditorProps(p, s)

      val deletionReason: VdomTag =
        <.section(
          <.h4(*.deletionReasonHeader, SpecialBuiltInField.DeletionReason.name + ":"),
          RichTextEditor.DeletionReason.Component(reasonTextProps))

      val commit: Option[Callback] =
        for {
          reqs   <- NonEmptySet.option(s.selectedReqs.selected)
          reason <- reasonTextProps.validated.toOption
        } yield p.perform(DeleteReqs(reqs, Set.empty, reason))

      val deleteButton: VdomTag =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Trash, UiText.Life.delete),
          state = Button.State.enabledWhen(commit.isDefined),
          colour = Colour.Red
        ).tag(^.onClick -->? commit)

      <.main(
        *.main,
        TestMarker.deletionForm.tagMod,
        <.h2("You are about to delete the following requirements:"),
        <.section(
          <.div(*.reqHelp, "In addition to those you selected, implied requirements are also presented with exclusively-implied requirements auto-selected for deletion."),
          renderReqTable(p, s.selectedReqs)),
        <.div(*.bottomSections,
          <.div(*.bottomSectionL, deletionReason),
          <.div(*.bottomSectionR, cancelButton, <.br, deleteButton)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build

}
