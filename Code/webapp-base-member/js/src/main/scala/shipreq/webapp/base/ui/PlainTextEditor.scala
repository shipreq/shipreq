package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.feature.EditorStatus
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.text.SingleLine
import shipreq.webapp.base.ui.semantic._

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
                           abort       : Option[Callback],
                           inputContMod: TagMod = EmptyVdom,
                           inputMod    : TagMod = EmptyVdom) {
      @inline def render = Component(this)
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.derive

    final class Backend($: BackendScope[Props, Unit]) {

      val base = {
        val keys =
          KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
          KeyboardTheme.commitCO($.props.map(_.status.getCommit))

        val onChange = (_: ReactEventFromInput).extract(_.target.value)(t =>
          $.props.flatMap(p =>
            p.status.wrapEdit(p.updateText(t))))

        <.input.text(
          keys,
          ^.autoFocus := true,
          ^.onChange ==> onChange)
      }

      def render(p: Props): VdomElement = {

        def input = base(p.inputMod, ^.value := p.text)

        def instructions = KeyboardTheme.Instructions.forTextEditor(
          SingleLine,
          commit     = p.status.getCommit,
          commitVerb = KeyboardTheme.Instructions.defaultCommitVerb,
          abort      = p.abort,
          help       = None,
          fullscreen = None)

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
                Input.Text.loadingDisabled(p.text)(p.inputContMod)))

          case EditorStatus.Invalid(err) =>
            renderWithError(err)

          case EditorStatus.AsyncError(err, _, _) =>
            renderWithError(err)
        }
      }
    }

    val Component = ScalaComponent.builder[Props]
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

    final case class Props(text        : String,
                           updateText  : String => Callback,
                           status      : EditorStatus,
                           buttonColour: ColourPlus,
                           buttonLabel : String,
                           inputMod    : TagMod) {
      @inline def render = Component(this)
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.derive

    private def render(p: Props): VdomElement = {
      val onChange = (_: ReactEventFromInput).extract(_.target.value)(t => p.status.wrapEdit(p updateText t))

      def buttonOk       = Button(colour = p.buttonColour)
      def buttonDisabled = Button(colour = p.buttonColour,  state = Button.State.Disabled)
      def buttonError    = Button(colour = ColourPlus.Negative, state = Button.State.Disabled)
      def buttonLoading  = Button(colour = ColourPlus.Primary,  state = Button.State.Loading)

      val input =
        <.input.text(
          p.inputMod,
          ^.value := p.text,
          ^.onChange ==> onChange)

      p.status match {

        case EditorStatus.Ignore | EditorStatus.Valid(None) =>
          <.div(
            <.div(
              Input.Action(
                input,
                buttonDisabled.tag(p.buttonLabel))))

        case EditorStatus.Valid(Some(commit)) =>
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
                input(^.disabled := true),
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

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      //    .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
