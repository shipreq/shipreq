package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.{Deny, Disabled, Enabled, ErrorMsg, Permission}
import shipreq.webapp.base.data.EmailAddr
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Icon, Input, Message}
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.base.ui.{CommmonUiText, GeneralTheme}
import shipreq.webapp.base.validation.UserValidators
import shipreq.webapp.client.public.Styles.{register1 => *}
import shipreq.webapp.client.public.spa.{Page, RouterCtl}

object Register1 {

  final case class Props(publicRegistration: Permission,
                         rc                : RouterCtl,
                         state             : StateSnapshot[State],
                         asyncW            : AsyncFeature.Write.D0[ErrorMsg],
                         submit            : ServerSideProcInvoker[EmailAddr, ErrorMsg, Unit]) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(email    : String,
                         async    : AsyncFeature.State.D0[ErrorMsg],
                         submitted: Boolean) {

    val inFlight: Boolean =
      AsyncFeature.isInProgress(async)

    val formEnabled: Enabled =
      Disabled when inFlight

    val validated: Option[EmailAddr] =
      UserValidators.emailAddr.unnamed(email).toOption
  }

  object State {
    def init: State =
      State("", None, false)
  }

  private implicit def validationUX = ValidationUX.Off

  final class Backend($: BackendScope[Props, Unit]) {

    private def submitCB(p: Props): Option[Callback] =
      if (p.state.value.formEnabled is Disabled)
        None
      else
        for {
          email <- p.state.value.validated
        } yield
          p.asyncW.forgetFailure(
            p.submit(email).flatTapSync {
              case \/-(_) => $.props.flatMap(_.state.modState(_.copy(submitted = true)))
              case -\/(e) => GeneralTheme.showErrorMsg(e)
            }
          )

    private val attemptSubmit: Callback =
      $.props.flatMap(submitCB(_).getOrEmpty)

    private val submitOnEnter = GeneralTheme.submitOnEnter(attemptSubmit)

    private val fieldEmail =
      Form.Field.text
        .withLabel(CommmonUiText.emailAddr)
        .withEditor(m => Input.Text.icon(Icon.Mail.tag, <.input.email(m, ^.autoComplete.email, ^.autoFocus := true, submitOnEnter)))
        .withValidator(UserValidators.emailAddr.unnamed)
        .withStateLens(State.email)

    private def renderForm(p: Props): VdomElement = {
      val s = p.state.value

      val submitButton =
        GeneralTheme.submitButton("Register", submitCB(p), inFlight = s.inFlight)

      <.form(*.part1,
        Form(
          fieldEmail(p.state).withEnabled(s.formEnabled),
          Form.Field.replacement(<.div(*.submitCont, submitButton))))
    }

    private def renderSuccess: VdomElement =
      <.div(*.part2,
        Message(
          Message.Style(Message.Type.Success),
          Icon.MailOutline,
          "Check your email",
          "You'll soon receive an email with a link to continue the registration process."))

    private def renderDisabled(rc: RouterCtl): VdomElement =
      <.div(*.part0,
        Message(
          Message.Style(Message.Type.Warning),
          Icon.RemoveUser,
          "Registration disabled",
          TagMod(
            "We're currently not accepting new public registrations.",
            <.br,
            "Check back later or contact us ", rc.link(Page.Home)("here"), ".")))

    def render(p: Props): VdomElement =
      if (p.publicRegistration is Deny)
        renderDisabled(p.rc)
      else if (p.state.value.submitted)
        renderSuccess
      else
        renderForm(p)
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}