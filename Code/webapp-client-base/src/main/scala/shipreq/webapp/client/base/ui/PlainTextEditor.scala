package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.text.SingleLine
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.KeyboardTheme
import shipreq.webapp.client.base.ui.semantic._

/**
  * Editor for single-line plain-text.
  */
object PlainTextEditor {

  private val errorPointingUp = Label.Style(Label.Type.BasicPointingUp, Colour.Red).div

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * Temp because it can be aborted, and will likely disappear after commit/abort.
    * Basic because it's just the editor with no commit/abort buttons.
    */
  object TempBasic {

    final case class Props(text        : String,
                           updateText  : String => Callback,
                           status      : EditorStatus,
                           abort       : Callback,
                           inputContMod: TagMod = EmptyTag,
                           inputMod    : TagMod = EmptyTag) {
      @inline def render = Component(this)
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {

      val base = {
        val keys =
          KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
          KeyboardTheme.commitCO($.props.map(_.status.getCommit), SingleLine)

        val onChange = (_: ReactEventI).extract(_.target.value)(t =>
          $.props.flatMap(p =>
            p.status.wrapEdit(p.updateText(t))))

        <.input.text(
          keys,
          ^.autoFocus := true,
          ^.onChange ==> onChange)
      }

      def render(p: Props): ReactElement = {

        def input        = base(p.inputMod, ^.value := p.text)
        def instructions = KeyboardTheme.instructionsForCommitAbort(SingleLine, p.status.getCommit, p.abort, None)

        def renderWithError(err: TagMod) =
          <.div(
            <.div(
              Input.Error(p.inputContMod, input)),
            errorPointingUp(err))

        p.status match {
          case EditorStatus.Ignore | EditorStatus.Valid(_) =>
            <.div(
              <.div(
                Input.Base(p.inputContMod, input),
                instructions))

          case EditorStatus.InTransit =>
            <.div(
              <.div(
                Input.loadingDisabled(p.text)(p.inputContMod)))

          case EditorStatus.Invalid(err) =>
            renderWithError(err)

          case EditorStatus.AsyncError(err, _, _) =>
            renderWithError(err)
        }
      }
    }

    val Component = ReactComponentB[Props]("PlainTextEditor.TempBasic")
      .renderBackend[Backend]
      //    .configure(Reusability.shouldComponentUpdate)
      .build
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * Editor with an attached button on the right.
    *
    * [ Text here      | BUTTON ]
    */
  object WithButton {

    final case class Props(text       : String,
                           updateText : String => Callback,
                           status     : EditorStatus,
                           buttonLabel: String,
                           inputMod   : TagMod) {
      @inline def render = Component(this)
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.caseClass

    val buttonOk       = Button(colour = ColourPlus.Primary)
    val buttonDisabled = Button(colour = ColourPlus.Primary,  state = Button.State.Disabled)
    val buttonError    = Button(colour = ColourPlus.Negative, state = Button.State.Disabled)
    val buttonLoading  = Button(colour = ColourPlus.Primary,  state = Button.State.Loading)

    final class Backend($: BackendScope[Props, Unit]) {

      def render(p: Props): ReactElement = {
        val onChange = (_: ReactEventI).extract(_.target.value)(t => p.status.wrapEdit(p updateText t))

        val input =
          <.input.text(
            p.inputMod,
            ^.value := p.text,
            ^.onChange ==> onChange)

        p.status match {

          case EditorStatus.Ignore =>
            <.div(
              <.div(
                Input.Action(
                  input,
                  buttonDisabled.tag(p.buttonLabel))))

          case EditorStatus.Valid(commit) =>
            val keys = KeyboardTheme.commitCriterion.handle(commit).toReact
            <.div(
              <.div(
                Input.Action(
                  input(keys),
                  buttonOk.tag(^.onClick --> commit, p.buttonLabel))))

          case EditorStatus.InTransit =>
            <.div(
              <.div(
                Input.Action(
                  input(^.disabled := "disabled"),
                  buttonLoading.tag(p.buttonLabel))))

          case EditorStatus.Invalid(err) =>
            <.div(
              <.div(
                Input.ActionError(
                  input,
                  buttonError.tag(p.buttonLabel))),
              errorPointingUp(err))

          case a: EditorStatus.AsyncError =>
            <.div(
              <.div(
                Input.Action(
                  input,
                  buttonOk.tag(^.onClick --> a.retry, UiText.buttonRetry))),
              errorPointingUp(a.err))
        }
      }
    }

    val Component = ReactComponentB[Props]("PlainTextEditor.WithButton")
      .renderBackend[Backend]
      //    .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
