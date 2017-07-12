package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.webapp.base.data.{Disabled, Enabled, SecurityToken}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.KeyHandler.Criterion
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Button, Colour, Form, Icon, Input, Message, Size}
import shipreq.webapp.base.user.{PlainTextPassword, UserValidators}
import shipreq.webapp.client.public.PublicSpaProtocols.{ResetPassword => P}
import shipreq.webapp.client.public.Styles.{resetPassword => *}

object ResetPassword {

  final case class Props(token: SecurityToken,
                         resetPassword: ServerSideProcInvoker[P.Request, P.Response]) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(password1: String,
                         password2: String,
                         async    : AsyncFeature.State.D0[String],
                         response : Option[P.Response]) {

    val formEnabled: Enabled =
      Disabled when AsyncFeature.isInProgress(async)

    val validated: Option[PlainTextPassword] =
      UserValidators.passwordTwice((password1, password2)).toOption
  }

  object State {
    def init: State =
      State("", "", None, None)
  }

  final class Backend($: BackendScope[Props, State]) {

    val asyncW: AsyncFeature.Write.D0[String] =
      AsyncFeature.Write.D0.init($ zoomStateL State.async)

    def submitCB(props: Props, state: State): Option[Callback] =
      for {
        newPassword <- state.validated
        if state.formEnabled is Enabled
      } yield
        asyncW((s, f) => props.resetPassword(
          P.Request(props.token, newPassword),
          res => s << $.modState(_.copy(response = Some(res))),
          e => f(e) >> Callback.alert(e)))

    val attemptSubmit: Callback =
      $.props.flatMap(p =>
        $.state.flatMap(s =>
          submitCB(p, s).getOrEmpty))

    val submitOnEnter = Criterion.Enter.handle(attemptSubmit) + Criterion.CtrlEnter.handle(attemptSubmit)

    val fieldPassword1 = Form.TextField.highLevel(
      State.password1,
      UserValidators.password.unnamed,
      m => Input.Text.icon(Icon.Lock.tag, <.input.password(m, ^.autoFocus := true, submitOnEnter)),
      Some("New password"))(ValidationUX.Full)

    def renderForm(p: Props, s: State): VdomElement = {

      val fieldPassword2 = Form.TextField.highLevel(
        State.password2,
        UserValidators.password2(s.password1),
        m => Input.Text.icon(Icon.Lock.tag, <.input.password(m, submitOnEnter)),
        Some("Confirm new password"))(ValidationUX.Highlight)

      val submitCB: Option[Callback] =
        this.submitCB(p, s)

      val submit =
        Button(
          state = Button.State.enabledWhen(submitCB.isDefined),
          colour = Colour.Blue,
          size = Size.Large).tag(
          *.submitButton,
          "Change Password",
          ^.onClick -->? submitCB)

      val ss = StateSnapshot(s).setStateVia($)

      var fields: NonEmptyVector[Form.Field] =
        NonEmptyVector(fieldPassword1, fieldPassword2).map(_(ss))

      fields :+= Form.NotAField(<.div(*.submitCont, submit))

      <.div(*.part1, Form(fields))
    }

    def renderResponse(r: P.Response): VdomElement = {
      val msg: VdomTag = r match {

        case P.Response.Success =>
          Message(
            Message.Style(Message.Type.Success),
            Icon.Lock,
            "Changed Password",
            "Your password was updated successfully.")

        case P.Response.TokenExpired | P.Response.TokenInvalid =>
          Message(
            Message.Style(Message.Type.Error),
            Icon.Warning,
            "Invalid token",
            TagMod(
              "The link emailed to you is no longer valid.", <.br,
              "Please request a new one from the login page."))
      }

      <.div(*.part2, msg)
    }

    def render(p: Props, s: State): VdomElement =
      s.response.fold(renderForm(p, s))(renderResponse)
  }

  val Component = ScalaComponent.builder[Props]("ResetPassword")
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}