package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Disabled, Enabled, VerificationToken}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic.{Form, Icon, Input, Message}
import shipreq.webapp.base.user.{PlainTextPassword, UserValidators}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.public.PublicSpaProtocols.{ResetPassword2 => P}
import shipreq.webapp.client.public.Styles.{resetPassword => *}

object ResetPassword {

  final case class Props(token: VerificationToken,
                         resetPassword: ServerSideProcInvoker[P.Request, ErrorMsg, P.Result]) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(password1: String,
                         password2: String,
                         async    : AsyncFeature.State.D0[ErrorMsg],
                         response : Option[P.Result]) {

    val inFlight: Boolean =
      AsyncFeature.isInProgress(async)

    val formEnabled: Enabled =
      Disabled when inFlight

    val validated: Option[PlainTextPassword] =
      UserValidators.passwordTwice((password1, password2)).toOption
  }

  object State {
    def init: State =
      State("", "", None, None)
  }

  private implicit def validationUX = ValidationUX.Full

  final class Backend($: BackendScope[Props, State]) {

    val asyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.async)

    def submitCB(props: Props, state: State): Option[Callback] =
      for {
        newPassword <- state.validated
        if state.formEnabled is Enabled
      } yield
        asyncW.forgetFailure(
          props.resetPassword(P.Request(props.token, newPassword)).flatTapSync {
            case \/-(res) => $.modState(_.copy(response = Some(res)))
            case -\/(err) => GeneralTheme.showErrorMsg(err)
          }
        )

    val attemptSubmit: Callback =
      $.props.flatMap(p =>
        $.state.flatMap(s =>
          submitCB(p, s).getOrEmpty))

    val submitOnEnter = GeneralTheme.submitOnEnter(attemptSubmit)

    val fieldPassword1 =
      Form.Field.text
        .withLabel("New password")
        .withEditor(m => Input.Text.icon(Icon.Lock.tag, <.input.password(m, ^.autoComplete.newPassword, ^.autoFocus := true, submitOnEnter)))
        .withValidator(UserValidators.password.unnamed)
        .withStateLens(State.password1)

    def renderForm(p: Props, s: State): VdomElement = {

      val fieldPassword2 =
        Form.Field.text
          .withLabel("Confirm new password")
          .withEditor(m => Input.Text.icon(Icon.Lock.tag, <.input.password(m, ^.autoComplete.newPassword, submitOnEnter)))
          .withValidator(UserValidators.password2(s.password1))
          .withValidationUX(ValidationUX.Highlight)
          .withStateLens(State.password2)

      val submitButton =
        GeneralTheme.submitButton("Change Password", submitCB(p, s), inFlight = s.inFlight)

      val ss = StateSnapshot(s).setStateVia($)

      var fields: NonEmptyVector[Form.Field[_]] =
        NonEmptyVector(fieldPassword1, fieldPassword2).map(_(ss))

      if (s.formEnabled is Disabled)
        fields = fields.map(_.disable)

      fields :+= Form.Field.replacement(<.div(*.submitCont, submitButton))

      <.form(*.part1, Form(fields))
    }

    def renderResponse(r: P.Result): VdomElement =
      r match {
        case P.Result.TokenExpired => Common.renderTokenExpired
        case P.Result.TokenInvalid => Common.renderTokenInvalid
        case P.Result.Success =>
          <.div(*.part2,
            Message(
              Message.Style(Message.Type.Success),
              Icon.Lock,
              "Changed Password",
              "Your password was updated successfully."))
      }

    def render(p: Props, s: State): VdomElement =
      s.response.fold(renderForm(p, s))(renderResponse)
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}