package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import org.scalajs.dom.{html, window}
import scala.annotation.nowarn
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data.{Disabled, Enabled, TCB}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.{BrowserStorage, ValidationUX}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Login.Request
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.base.user.{EmailAddr, UserValidators, Username}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.base.{CommmonUiText, Urls}
import shipreq.webapp.client.public.Prefetch
import shipreq.webapp.client.public.Styles.{login => *}

object Login {

  final case class Props(state          : StateSnapshot[State],
                         asyncW         : AsyncFeature.Write.D0[ErrorMsg],
                         attemptLogin   : ServerSideProcInvoker[Request, ErrorMsg, Permission],
                         resetPassword  : ServerSideProcInvoker[Username \/ EmailAddr, ErrorMsg, Unit],
                         redirectOnLogin: Option[Url.Relative]) {

    val inFlight: Boolean =
      AsyncFeature.isInProgress(state.value.async)

    val formEnabled: Enabled =
      Disabled when inFlight

    @inline def render: VdomElement = Component(this)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class LocalStorage(rememberMe: Option[Boolean], user: Option[String])

  object LocalStorage {

    private final val KeyPrefix = "login-"
    private val FieldRememberMe = BrowserStorage.Field.boolean(KeyPrefix + "remember-me")
    private val FieldUser       = BrowserStorage.Field        (KeyPrefix + "user")

    private implicit def storage = BrowserStorage.localOrEmpty

    def read: CallbackTo[LocalStorage] =
      for {
        rm <- FieldRememberMe.get
        u  <- FieldUser.get
      } yield LocalStorage(rm, u)

    def write(s: State) =
      if (s.rememberMe)
        for {
          _ <- FieldRememberMe.set(true)
          _ <- FieldUser.set(UserValidators.usernameOrEmail.corrector(s.req.usernameOrEmail))
        } yield ()
      else
        for {
          _ <- FieldRememberMe.set(false)
          _ <- FieldUser.remove
        } yield ()
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
          localStorage.user.foreach(u => s = State.usernameOrEmail.set(u)(s))
      }
      s
    }
  }

  object State {
    val usernameOrEmail = req ^|-> Request.Untyped.usernameOrEmail
    val password        = req ^|-> Request.Untyped.password

    def empty: State =
      State(Request.Untyped("", ""), true, None, None, None)

    def init: CallbackTo[State] =
      for {
        s <- LocalStorage.read
      } yield empty(s)
  }

  final case class ErrorFlash(title: String, content: String)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private implicit def validationUX = ValidationUX.Off

  final class Backend($: BackendScope[Props, Unit]) {

    // User is likely to log in - prefetch new resources for next page
    Prefetch.memberHome()

    /** Stores the current state in client's local storage according to the remember-me setting */
    val writeCredentials: Callback =
      $.props.flatMap(p => LocalStorage.write(p.state.value))

    // Else a user might go to home thinking they've reloaded, and another user might click Login and see the
    // previous user's password populated.
    def clearCredentials: Callback =
      $.props.flatMap(_.state.setState(State.empty))

    def onMount: Callback =
      focusForm(3).delayMs(20).toCallback

    private def focusForm(retries: Int): Callback =
      $.props.flatMap { p =>
        val ref = if (p.state.value.req.usernameOrEmail.isEmpty) refUser else refPassword
        ref.get.filterNot(_.disabled).asCallback.flatMap {
          case Some(i) => Callback(i.focus())
          case None    => focusForm(retries - 1).delayMs(20).toCallback.when_(retries > 1)
        }
      }

    private def handleError(e: ErrorMsg): Callback =
      setError("Error occurred", e.value)

    private def setError(title: String, content: String): Callback =
      for {
        p <- $.props
        _ <- p.state.modState(_.copy(errorFlash = Some(ErrorFlash(title, content))))
        _ <- $.getDOMNode.map(_.toElement.foreach(JQuery(_).find(Message.jquerySel).transition(Transition.pulse, "320ms"))).delayMs(10).toCallback
      } yield ()

    private val attemptLogin: Callback =
      $.props.flatMap(p =>
        Callback.when(p.formEnabled is Enabled)(
          p.state.value.req.validate match {
            case \/-(req) =>
              p.asyncW.onSuccess(onSuccess =>
                p.attemptLogin(req)
                  .flatTapSync {
                    case \/-(Allow) => onLoginSuccess // onSuccess is deliberately omitted so the form doesn't re-enable before the redirect completes
                    case \/-(Deny)  => onSuccess << onLoginFailure(req.user)
                    case -\/(e)     => handleError(e)
                  }
              )

            case -\/(_) =>
              onLoginFailure(Username.orEmail(p.state.value.req.usernameOrEmail))
          }
        )
      )

    private def onLoginSuccess: TCB.Success =
      TCB.Success($.props.map(p =>
        window.location.href = p.redirectOnLogin.getOrElse(Urls.memberHome).relativeUrl))

    private def onLoginFailure(user: Username \/ EmailAddr): Callback =
      setError("Login failed", s"Invalid ${CommmonUiText.usernameOrEmail(user.isLeft).toLowerCase} or password.")

    private val submitOnEnter = GeneralTheme.submitOnEnter(attemptLogin)

    private def onForgotPassword: Callback =
      $.props.flatMap(p =>
        Callback.when(p.formEnabled is Enabled) {
          val usernameOrEmailStr = p.state.value.req.usernameOrEmail
          UserValidators.usernameOrEmail(usernameOrEmailStr) match {
            case \/-(u) =>
              p.asyncW(
                p.resetPassword(u).flatTapSync {
                  case \/-(_) => p.state.modState(_.copy(passwordReset = Some(u)))
                  case -\/(e) => handleError(e)
                }
              )

            case -\/(_) =>
              val user = Username.orEmail(usernameOrEmailStr)
              setError("Forgotten password", s"Invalid ${CommmonUiText.usernameOrEmail(user.isLeft).toLowerCase}.")
          }
        }
      )

    private val refUser = Ref[html.Input]
    private val fieldUser =
      Form.Field.text
        .withLabel(CommmonUiText.usernameOrEmail)
        .withEditor(m => Input.Text.icon(Icon.User.tag, <.input.text(^.autoComplete.usernameEmail, m, submitOnEnter).withRef(refUser)))
        .withStateLens(State.usernameOrEmail)

    private val refPassword = Ref[html.Input]
    private val fieldPassword =
      Form.Field.text
        .withLabel(
          <.div(*.passwordLabel,
            <.div(CommmonUiText.password),
            <.a(*.forgotPassword, "Forgot password?", ^.onClick --> onForgotPassword)))
        .withEditor(m => Input.Text.icon(Icon.Lock.tag, <.input.password(^.autoComplete.currentPassword, m, submitOnEnter).withRef(refPassword)))
        .withStateLens(State.password)

    private val textFields: NonEmptyVector[StateSnapshot[State] => Form.Field[String]] =
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

      var fields = textFields.map[Form.Field[_]](_(p.state))
      if (p.formEnabled is Disabled)
        fields = fields.map(_.disable)

      val submitButton =
        GeneralTheme.submitButton("Login", Option.when(p.formEnabled is Enabled)(attemptLogin), inFlight = p.inFlight)

      val bottomRow =
        <.div(*.bottomRow,
          <.div(*.rememberMe, Input.Checkbox.fromStateSnapshot(State.rememberMe, p.state, "Remember me")),
          <.div(*.submitCont, submitButton))

      fields :+= Form.Field.replacement(bottomRow)

      // Using an array so that React preserves the form and input focus
      val array = VdomArray.empty()
      errorMsg.foreach(e => array += e(^.key := "e"))
      array += Form(fields).apply(^.key := "f")

      <.form(*.part1, array)
    }

    @nowarn("cat=unused")
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

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .componentDidUpdate(_.backend.writeCredentials)
    .componentWillUnmount(_.backend.clearCredentials)
    .build
}