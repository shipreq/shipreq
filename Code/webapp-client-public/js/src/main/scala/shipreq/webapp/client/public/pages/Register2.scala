package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, Invalid}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Form, Icon, Input, Message}
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.user._
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.validation.{Composite, Simple}
import shipreq.webapp.base.{CommmonUiText, Urls, WebappConfig}
import shipreq.webapp.client.public.Prefetch
import shipreq.webapp.client.public.PublicSpaProtocols.Register2.{Request, Result}
import shipreq.webapp.client.public.Styles.{register2 => *}

object Register2 {

  final case class Props(token : VerificationToken,
                         submit: ServerSideProcInvoker[Request, ErrorMsg, Result]) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(personName    : String,
                         username      : String,
                         password1     : String,
                         password2     : String,
                         newsletter    : Boolean,
                         tos           : Agreement,
                         vux           : ValidationUX,
                         async         : AsyncFeature.State.D0[ErrorMsg],
                         takenUsernames: Set[Username],
                         response      : Option[Result.Terminal]) {

    val inFlight: Boolean =
      AsyncFeature.isInProgress(async)

    val formEnabled: Enabled =
      Disabled when inFlight
  }

  object State {
    def init: State =
      State(
        "", "", "", "",
        newsletter     = true,
        tos            = Disagree,
        vux            = ValidationUX.Off,
        async          = None,
        takenUsernames = UnivEq.emptySet,
        response       = None)

    val tosB: Lens[State, Boolean] =
      tos ^<-> Agree.isoWhen(true)
  }

  final class Backend($: BackendScope[Props, State]) {

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private val asyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.async)

    private def showValidationFailures =
      State.vux set ValidationUX.Full

    private def onResult(req: Request): Result => Callback = {
      case r: Result.Terminal   => $.modState(_.copy(response = Some(r)))
      case Result.UsernameTaken => $.modState(showValidationFailures compose State.takenUsernames.modify(_ + req.username))
    }

    private def tosName = "terms of service"

    private val tosLabel = TagMod("I agree to the ", <.a.toNewWindow(Urls.termsOfService.relativeUrl)(tosName))

    private val tosValidator: Composite.Stateless[Agreement, Agreement, Agree.type] =
      Simple.Auditor[Simple.Invalidity, Agreement, Agree.type] {
        case Agree    => \/-(Agree)
        case Disagree => -\/(Simple.Invalidity("Agreement is mandatory."))
      }
        .toValidator
        .named(tosName)

    private val fieldPersonName =
      Form.Field.text
        .withLabel(CommmonUiText.userPersonName)
        .withEditor(m => Input.Text.icon(Icon.User.tag, <.input.text(^.autoComplete.name, ^.autoFocus := true, m)))
        .withValidator(UserValidators.personName.unnamed)
        .withStateLens(State.personName)

    private val fieldPassword1 =
      Form.Field.text
        .withLabel(CommmonUiText.password)
        .withEditor(m => Input.Text.icon(Icon.Lock.tag, <.input.password(^.autoComplete.newPassword, m)))
        .withValidator(UserValidators.password.unnamed)
        .withStateLens(State.password1)

    private def renderForm(p: Props, s: State): VdomElement = {

      // User is very close to creating an account and logging in - prefetch new resources for next page
      Prefetch.memberHome()

      val usernameValidator = UserValidators.username(s.takenUsernames)

      type ValiInput = (String, String, (String, String), Agreement)

      val validator: Composite.Validator[ValiInput, ValiInput, Request] =
        UserValidators.personName.named
          .tuple(usernameValidator.named)
          .tuple(UserValidators.passwordTwice)
          .tuple(tosValidator.named)
          .mapValid {
            case (name, username, password, Agree) =>
              Request(p.token, name, username, password, newsletter = s.newsletter)
          }

      val fieldUsername =
        Form.Field.text
          .withLabel(CommmonUiText.username)
          .withEditor(m => Input.Text.icon(Icon.User.tag, <.input.text(^.autoComplete.username, m)))
          .withValidator(usernameValidator.unnamed)
          .withStateLens(State.username)

      val fieldPassword2 =
        Form.Field.text
          .withLabel("Confirm password")
          .withEditor(m => Input.Text.icon(Icon.Lock.tag, <.input.password(^.autoComplete.newPassword, m)))
          .withValidator(UserValidators.password2(s.password1))
          .withStateLens(State.password2)

      val submitCB: Option[Callback] = {
        def submitIfValid: Composite.Invalidity \/ Callback =
          validator((s.personName, s.username, (s.password1, s.password2), s.tos)).map(req =>
            asyncW.forgetFailure(
              p.submit(req).flatTapSync {
                case \/-(res) => onResult(req)(res)
                case -\/(err) => GeneralTheme.showErrorMsg(err)
              }
            )
          )

        Common.validationOffUntilFirstSubmit(
          s.formEnabled,
          s.vux,
          $.modState(showValidationFailures),
          submitIfValid.toOption)
      }

      val ss = StateSnapshot(s).setStateVia($)

      val fieldNewsletter =
        Form.Field.boolean
          .withLabel("Subscribe to newsletter")
          .withState(ss zoomStateL State.newsletter)

      val fieldTermsOfService =
        Form.Field.boolean
          .withLabel(tosLabel)
          .withState(ss zoomStateL State.tosB)
          .withValidity(Invalid.when(s.tos.is(Disagree) && s.vux !=* ValidationUX.Off))

      val fieldSubmit =
        Form.Field.around(
          GeneralTheme.submitButton("Create Account", submitCB, inFlight = s.inFlight),
          *.submitCont)

      var fields: NonEmptyVector[Form.Field[_]] =
        NonEmptyVector(
          fieldPersonName(ss),
          fieldUsername(ss),
          Form.Field.two(
            fieldPassword1(ss),
            fieldPassword2(ss)),
          fieldNewsletter,
          fieldTermsOfService,
          fieldSubmit)

      if (s.formEnabled is Disabled)
        fields = fields.map(_.disable)

      <.form(*.part1, Form(fields)(s.vux))
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private def renderResult(r: Result.Terminal): VdomElement =
      r match {
        case Result.TokenExpired => Common.renderTokenExpired
        case Result.TokenInvalid => Common.renderTokenInvalid
        case Result.Success =>
          <.div(*.part2,
            Message(
              Message.Style(Message.Type.Success),
              Icon.Heart,
              s"Welcome to ${WebappConfig.appName}!",
              TagMod(
                "Your account is now active.", <.br,
                s"May ${WebappConfig.appName} bring you much benefit and happiness!", <.br,
                <.a(*.begin, ^.href := Urls.memberHome.relativeUrl, "Begin..."))))
      }

    def render(p: Props, s: State): VdomElement =
      s.response.fold(renderForm(p, s))(renderResult)
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}