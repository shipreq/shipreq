package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.validation.ValidatorU
import shipreq.webapp.client.base.feature.AsyncActionFeature
import shipreq.webapp.client.base.lib.{KeyHandler, KeyboardTheme}
import shipreq.webapp.client.base.ui.semantic._

/**
  * Editor for single-line plain-text.
  */
object PlainTextEditor {

  sealed abstract class State
  object State {
    case object Blank                   extends State
    case object InTransit               extends State
    case class Ready(commit: Callback)  extends State
    case class InputError(err: TagMod)  extends State
    case class AsyncError(err: TagMod, retry: Callback)  extends State

    def async[A](a: AsyncActionFeature.D0.State[A])(implicit f: A => TagMod): Option[State] =
      a.map {
        case AsyncActionFeature.Locked          => InTransit
        case AsyncActionFeature.Failed(e, r, _) => AsyncError(f(e), r)
      }

    def validator[I, C, V](v: ValidatorU[I, C, V])(i: I, isBlank: C => Boolean, commit: V => Callback): State = {
      val corrected = v.correctedU(i)
      if (isBlank(corrected.value))
        Blank
      else
        v.validateU(corrected) match {
          case scalaz.Success(ok)  => State.Ready(commit(ok))
          case scalaz.Failure(err) => State.InputError(err.toText)
        }
    }
  }

  private val errorPointingUp = Label.Style(Label.Type.BasicPointingUp, Colour.Red).div

  private def asyncLogic[A](asyncState   : AsyncActionFeature.D0.State[A],
                            asyncFeature : AsyncActionFeature.D0.Feature[A],
                            updateText   : String => Callback,
                            nonAsyncState: => State)
                           (implicit f: A => TagMod): (String => Callback, State) =

    State.async(asyncState) match {
      case Some(s) => (updateText.andThen(_ >> asyncFeature.clearError(asyncState)), s)
      case None    => (updateText, nonAsyncState)
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * Temp because it can be aborted, and will likely disappear after commit/abort.
    * Basic because it's just the editor with no commit/abort buttons.
    */
  object TempBasic {

    final case class Props(text        : String,
                           updateText  : String => Callback,
                           state       : State,
                           abort       : Callback,
                           inputContMod: TagMod = EmptyTag,
                           inputMod    : TagMod = EmptyTag) {
      @inline def render = Component(this)
    }

    object Props {
      def asyncAware[A](text         : String,
                        updateText   : String => Callback,
                        asyncState   : AsyncActionFeature.D0.State[A],
                        asyncFeature : AsyncActionFeature.D0.Feature[A],
                        nonAsyncState: => State,
                        abort        : Callback,
                        inputContMod : TagMod = EmptyTag,
                        inputMod     : TagMod = EmptyTag
                       )(implicit f: A => TagMod): Props = {
        val (updText, state) = asyncLogic(asyncState, asyncFeature, updateText, nonAsyncState)
        Props(text, updText, state, abort, inputContMod, inputMod)
      }
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {

      val abortKB =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort))

      def render(p: Props): ReactElement = {
        val onChange = (_: ReactEventI).extract(_.target.value)(p.updateText)

        def input =
          <.input.text(
            p.inputMod,
            ^.autoFocus := 1,
            ^.value     := p.text,
            ^.onChange ==> onChange)

        p.state match {

          case State.Blank =>
            <.div(
              <.div(
                Input.Base(p.inputContMod,
                  input(abortKB.toReact)),
                KeyboardTheme.instructionsForCommitAbort(None, p.abort)))

          case State.Ready(commit) =>
            val commitKB = KeyboardTheme.commitCriterion.handle(commit)
            val keys = abortKB :: commitKB :: Nil
            <.div(
              <.div(
                Input.Base(p.inputContMod,
                  input(KeyHandler toReact keys)),
                KeyboardTheme.instructionsForCommitAbort(Some(commit), p.abort)))

          case State.InTransit =>
            <.div(
              <.div(
                Input.loadingDisabled(p.text)(p.inputContMod)))

          case State.InputError(err) =>
            <.div(
              <.div(
                Input.Error(p.inputContMod,
                  input(abortKB.toReact))),
              errorPointingUp(err))

          case State.AsyncError(err, retry) =>
            <.div(
              <.div(
                Input.Error(p.inputContMod,
                  input(abortKB.toReact))),
              errorPointingUp(err))
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
                           state      : State,
                           buttonLabel: String,
                           inputMod   : TagMod) {
      @inline def render = Component(this)
    }

    object Props {
      def asyncAware[A](text         : String,
                        updateText   : String => Callback,
                        asyncState   : AsyncActionFeature.D0.State[A],
                        asyncFeature : AsyncActionFeature.D0.Feature[A],
                        nonAsyncState: => State,
                        buttonLabel  : String,
                        inputMod     : TagMod
                       )(implicit f: A => TagMod): Props = {
        val (updText, state) = asyncLogic(asyncState, asyncFeature, updateText, nonAsyncState)
        Props(text, updText, state, buttonLabel, inputMod)
      }
    }

    //  implicit val reusabilityProps: Reusability[Props] =
    //    Reusability.caseClass

    val buttonOk       = Button(colour = ColourPlus.Primary)
    val buttonDisabled = Button(colour = ColourPlus.Primary,  state = Button.State.Disabled)
    val buttonError    = Button(colour = ColourPlus.Negative, state = Button.State.Disabled)
    val buttonLoading  = Button(colour = ColourPlus.Primary,  state = Button.State.Loading)

    final class Backend($: BackendScope[Props, Unit]) {

      def render(p: Props): ReactElement = {
        val onChange = (_: ReactEventI).extract(_.target.value)(p.updateText)

        val input =
          <.input.text(
            p.inputMod,
            ^.value := p.text,
            ^.onChange ==> onChange)

        p.state match {

          case State.Blank =>
            <.div(
              <.div(
                Input.Action(
                  input,
                  buttonDisabled.tag(p.buttonLabel))))

          case State.Ready(commit) =>
            val keys = KeyboardTheme.commitCriterion.handle(commit).toReact
            <.div(
              <.div(
                Input.Action(
                  input(keys),
                  buttonOk.tag(^.onClick --> commit, p.buttonLabel))))

          case State.InTransit =>
            <.div(
              <.div(
                Input.Action(
                  input(^.disabled := "disabled"),
                  buttonLoading.tag(p.buttonLabel))))

          case State.InputError(err) =>
            <.div(
              <.div(
                Input.ActionError(
                  input,
                  buttonError.tag(p.buttonLabel))),
              errorPointingUp(err))

          case State.AsyncError(err, retry) =>
            <.div(
              <.div(
                Input.Action(
                  input,
                  buttonOk.tag(^.onClick --> retry, UiText.buttonRetry))),
              errorPointingUp(err))
        }
      }
    }

    val Component = ReactComponentB[Props]("PlainTextEditor.WithButton")
      .renderBackend[Backend]
      //    .configure(Reusability.shouldComponentUpdate)
      .build
  }
}
