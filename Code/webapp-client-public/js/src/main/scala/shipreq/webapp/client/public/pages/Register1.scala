package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.{Deny, ErrorMsg, Permission}
import shipreq.webapp.base.CommmonUiText
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Form, Icon, Input, Message}
import shipreq.webapp.base.user.{EmailAddr, UserValidators}
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

    val formEnabled: Enabled =
      Disabled when AsyncFeature.isInProgress(async)

    val validated: Option[EmailAddr] =
      UserValidators.emailAddr.unnamed(email).toOption
  }

  object State {
    def init: State =
      State("", None, false)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private def submitCB(p: Props): Option[Callback] =
      for {
        email <- p.state.value.validated
        if p.state.value.formEnabled is Enabled
      } yield
        p.asyncW((s, f) => p.submit(
          email,
          _ => s << $.props.flatMap(_.state.modState(_.copy(submitted = true))),
          e => f(e) >> Callback.alert(e.value)))

    private val attemptSubmit: Callback =
      $.props.flatMap(submitCB(_).getOrEmpty)

    private val submitOnEnter = Common.submitOnEnter(attemptSubmit)

    private val fieldEmail = Form.TextField.highLevel(
      State.email,
      UserValidators.emailAddr.unnamed,
      m => Input.Text.icon(Icon.Mail.tag, <.input.email(m, ^.autoComplete.email, ^.autoFocus := true, submitOnEnter)),
      Some(CommmonUiText.emailAddr))(
      ValidationUX.Off)

    private def renderForm(p: Props): VdomElement = {
      val s = p.state.value

      val submitButton =
        Common.submitButton("Register", submitCB(p))

      <.form(*.part1,
        Form(
          fieldEmail(p.state).setEnabled(s.formEnabled),
          Form.NotAField(<.div(*.submitCont, submitButton))))
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

  val Component = ScalaComponent.builder[Props]("Register1")
    .renderBackend[Backend]
    .build
}