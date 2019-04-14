package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import org.scalajs.dom.{html, window}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data.{Disabled, Enabled, TCB}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol2.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.base.user.{EmailAddr, UserValidators, Username}
import shipreq.webapp.base.{CommmonUiText, Urls}
import shipreq.webapp.client.public.Prefetch
import shipreq.webapp.client.public.PublicSpaProtocols.Login.Request
import shipreq.webapp.client.public.Styles.{login => *}

object Login {

  final case class Props(state          : StateSnapshot[State],
                         asyncW         : AsyncFeature.Write.D0[ErrorMsg],
                         attemptLogin   : ServerSideProcInvoker[Request, ErrorMsg, Permission],
                         resetPassword  : ServerSideProcInvoker[Username \/ EmailAddr, ErrorMsg, Unit],
                         redirectOnLogin: Option[Url.Relative]) {

    val formEnabled: Enabled =
      Disabled when AsyncFeature.isInProgress(state.value.async)

    @inline def render: VdomElement = Component(this)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class LocalStorage(rememberMe: Option[Boolean], user: Option[String])

  object LocalStorage {
    import org.scalajs.dom.window.localStorage // TODO is this always available?

    private val KeyPrefix     = "login-"
    private val KeyRememberMe = KeyPrefix + "remember-me"
    private val KeyUser       = KeyPrefix + "user"
    private val ValueTrue     = "1"
    private val ValueFalse    = "0"

    def read() = LocalStorage(
      localStorage.getItem(KeyRememberMe) match {
        case ValueTrue  => Some(true)
        case ValueFalse => Some(false)
        case _          => None
      },
      Option(localStorage.getItem(KeyUser)))

    def write(s: State) = Callback {
      if (s.rememberMe) {
        localStorage.setItem(KeyRememberMe, ValueTrue)
        localStorage.setItem(KeyUser, UserValidators.usernameOrEmail.corrector(s.req.user))
      } else {
        localStorage.setItem(KeyRememberMe, ValueFalse)
        localStorage.removeItem(KeyUser)
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @Lenses
  final case class State(req          : Request.Untyped,
                         rememberMe   : Boolean,
                         errorFlash   : Option[ErrorFlash],
                         async        : AsyncFeature.State.D0[ErrorMsg],
                         passwordReset: Option[Username \/ EmailAddr]) {

    def apply(localStorage: LocalStorage) = {
      var s = this
      for (rm <- localStorage.rememberMe) {
        s = State.rememberMe.set(rm)(s)
        if (rm)
          localStorage.user.foreach(u => s = State.user.set(u)(s))
      }
      s
    }
  }

  object State {
    val user     = req ^|-> Request.Untyped.user
    val password = req ^|-> Request.Untyped.password

    def init: State =
      State(Request.Untyped("", ""), true, None, None, None)
  }

  final case class ErrorFlash(title: String, content: String)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Backend($: BackendScope[Props, Unit]) {

    // User is likely to log in - prefetch new resources for next page
    Prefetch.memberHome()

    def readCredentials: Callback =
      $.props.flatMap(_.state.modState(_(LocalStorage.read()))) >> focusForm(3).delayMs(20).toCallback

    /** Stores the current state in client's local storage according to the remember-me setting */
    val writeCredentials: Callback =
      $.props.flatMap(p => LocalStorage.write(p.state.value))

    // Else a user might go to home thinking they've reloaded, and another user might click Login and see the
    // previous user's password populated.
    def clearCredentials: Callback =
      $.props.flatMap(_.state.setState(State.init))

    private def focusForm(retries: Int): Callback =
      $.props.flatMap { p =>
        val ref = if (p.state.value.req.user.isEmpty) refUser else refPassword
        ref.get.filterNot(_.disabled).asCallback.flatMap {
          case Some(i) => Callback(i.focus())
          case None    => focusForm(retries - 1).delayMs(20).toCallback.when_(retries > 1)
        }
      }

    private def handleAsyncError(f: ErrorMsg => Callback): ErrorMsg => Callback =
      e => f(e) >> setError("Error occurred", e.value)

    private def setError(title: String, content: String): Callback =
      for {
        p <- $.props
        _ <- p.state.modState(_.copy(errorFlash = Some(ErrorFlash(title, content))))
        _ <- $.getDOMNode.map(_.toElement.foreach(JQuery(_).find(Message.jquerySel).transition(Transition.pulse, "320ms"))).delayMs(10).toCallback
      } yield ()

    private val attemptLogin: Callback =
      $.props.flatMap(p =>
        Callback.when(p.formEnabled is Enabled)(
          Request.validator(p.state.value.req) match {
            case \/-(req) =>
              p.asyncW((s, f) => p.attemptLogin(req, {
                  case Allow => onLoginSuccess // `s <<` is deliberately omitted so the form doesn't re-enable before the redirect completes
                  case Deny  => s << onLoginFailure(req.user)
                }, handleAsyncError(f)))
            case -\/(_) =>
              onLoginFailure(Username.orEmail(p.state.value.req.user))
          }
        )
      )

    private def onLoginSuccess: TCB.Success =
      TCB.Success($.props.map(p =>
        window.location.href = p.redirectOnLogin.getOrElse(Urls.memberHome).relativeUrl))

    private def onLoginFailure(user: Username \/ EmailAddr): Callback =
      setError("Login failed", s"Invalid ${CommmonUiText.usernameOrEmail(user.isLeft).toLowerCase} or password.")

    private val submitOnEnter = Common.submitOnEnter(attemptLogin)

    private def onForgotPassword: Callback =
      $.props.flatMap(p =>
        Callback.when(p.formEnabled is Enabled) {
          val usernameOrEmailStr = p.state.value.req.user
          UserValidators.usernameOrEmail(usernameOrEmailStr) match {
            case \/-(u) =>
              p.asyncW((s, f) => p.resetPassword(u,
                _ => s << p.state.modState(_.copy(passwordReset = Some(u))),
                handleAsyncError(f)))
            case -\/(_) =>
              val user = Username.orEmail(usernameOrEmailStr)
              setError("Forgotten password", s"Invalid ${CommmonUiText.usernameOrEmail(user.isLeft).toLowerCase}.")
          }
        }
      )

    private val refUser = Ref[html.Input]
    private val fieldUser = Form.TextField.unvalidated(
      State.user,
      m => Input.Text.icon(Icon.User.tag, <.input.text(m, submitOnEnter).withRef(refUser)),
      Some(CommmonUiText.usernameOrEmail))

    private val refPassword = Ref[html.Input]
    private val fieldPassword = Form.TextField.unvalidated(
      State.password,
      m => Input.Text.icon(Icon.Lock.tag, <.input.password(m, submitOnEnter).withRef(refPassword)),
      Some(
        <.div(*.passwordLabel,
          <.div(CommmonUiText.password),
          <.a(*.forgotPassword, "Forgot password?", ^.onClick --> onForgotPassword))))

    private val textFields: NonEmptyVector[StateSnapshot[State] => Form.TextField] =
      NonEmptyVector(fieldUser, fieldPassword)

    def render(p: Props): VdomElement =
      p.state.value.passwordReset match {
        case None    => renderForm(p)
        case Some(u) => renderPostPasswordReset(u)
      }

    private def renderForm(p: Props): VdomElement = {
      val s = p.state.value

      val errorMsg: Option[VdomTag] =
        s.errorFlash.map(e => Message(Message.Style(Message.Type.Error), Icon.Ban, e.title, e.content))

      var fields = textFields.map[Form.Field](_ (p.state))
      if (p.formEnabled is Disabled)
        fields = fields.map(_.disable)

      val submitButton =
        Common.submitButton("Login", Option.when(p.formEnabled is Enabled)(attemptLogin))

      val bottomRow =
        <.div(*.bottomRow,
          <.div(*.rememberMe, Input.Checkbox.fromStateSnapshot(State.rememberMe, p.state, "Remember me")),
          <.div(*.submitCont, submitButton))

      fields :+= Form.NotAField(bottomRow)

      // Using an array so that React preserves the form and input focus
      val array = VdomArray.empty()
      errorMsg.foreach(e => array += e(^.key := "e"))
      array += Form(fields)(^.key := "f")

      <.div(*.part1, array)
    }

    private def renderPostPasswordReset(u: Username \/ EmailAddr): VdomElement =
      <.div(*.part2,
        Message(
          Message.Style(Message.Type.Info),
          Icon.MailOutline,
          "Check your email",
          TagMod(
            <.em("If your email address was valid,"),
            " you'll soon receive an email with a link to reset your password.")))
  }

  val Component = ScalaComponent.builder[Props]("Login")
    .renderBackend[Backend]
    .componentWillMount(_.backend.readCredentials)
    .componentDidUpdate(_.backend.writeCredentials)
    .componentWillUnmount(_.backend.clearCredentials)
    .build
}