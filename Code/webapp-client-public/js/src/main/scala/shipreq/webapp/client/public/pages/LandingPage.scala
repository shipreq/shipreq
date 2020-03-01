package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.base.{AssetManifest, WebappConfig}
import shipreq.webapp.client.public.PublicSpaProtocols.LandingPage.Request
import shipreq.webapp.client.public.Styles.{landingPage => *}

object LandingPage {

  final case class Props(state: StateSnapshot[State],
                         asyncW: AsyncFeature.Write.D0[ErrorMsg],
                         submit: ServerSideProcInvoker[Request, ErrorMsg, Unit]) {
    @inline def render = Component(this)

    val async: AsyncFeature.ReadWrite.D0[ErrorMsg] =
      AsyncFeature.ReadWrite.D0(asyncW, state.value.async)
  }

  @Lenses
  final case class State(req      : Request.Untyped,
                         vux      : ValidationUX,
                         submitted: Boolean,
                         async    : AsyncFeature.State.D0[ErrorMsg])

  object State {
    def init: State =
      State(req       = Request.Untyped("", "", "", newsletter = true),
            vux       = ValidationUX.Off,
            submitted = false,
            async     = None)
  }

  private val header: TagMod =
    TagMod(
      <.div(
        <.img(*.banner, ^.src := AssetManifest.shipreqBannerSvg, ^.alt := WebappConfig.appName)),
      <.div(*.tagline,
        "Ship quality products with quality requirements."))

  private val yap = ScalaComponent.static("")(
    <.div(*.yap,
      <.div(*.yap1,
        "ShipReq is a modern, online tool", <.br,
        "for requirements development and management,", <.br,
        "currently in private beta phase."
      ),
      <.div(*.yap2,
        "Would you like to know more, or participate in the beta?", <.br,
        "Get in touch !",
        <.span(*.pointAtForm))))

  final class Backend($: BackendScope[Props, Unit]) {

    private val fieldName =
      Form.Field.text
        .withEditor(m => Input.Text.icon(Icon.User.tag, <.input.text(^.autoComplete.name, ^.placeholder := Request.labelName, m)))
        .withValidator(Request.validatorName)
        .withStateLens(Request.Untyped.name)

    private val fieldEmail =
      Form.Field.text
        .withEditor(m => Input.Text.icon(Icon.Mail.tag, <.input.email(^.autoComplete.email, ^.placeholder := Request.labelEmail, m)))
        .withValidator(Request.validatorEmail)
        .withStateLens(Request.Untyped.email)

    private val fieldMsg =
      Form.Field.text
        .withEditor(<.textarea(^.rows := 12, ^.placeholder := "What would you like to say?", _))
        .withValidator(Request.validatorMsg)
        .withStateLens(Request.Untyped.msg)

    private val textTields: NonEmptyVector[StateSnapshot[Request.Untyped] => Form.Field[_]] =
      NonEmptyVector(fieldName, fieldEmail, fieldMsg)

    def submit(p: Props, r: Request): Callback = {
      val task: AsyncCallback[ErrorMsg \/ Unit] =
        p.submit(r).flatTapSync {
          case \/-(_) =>
            val lockForm = p.state.modState(State.submitted.set(true))
            val sayDone  = Callback.alert("Great to hear from you.\n\nWe'll be in touch!")
            lockForm >> sayDone
          case -\/(f) =>
            Callback.alert(f.value)
        }

      p.asyncW(task)
    }

    private def renderForm(p: Props): VdomElement = {
      val s = p.state.value

      val inFlight = AsyncFeature.isInProgress(s.async)

      // Disable form if:
      // - already submitted; 1 msg/user is enough, if they really want to send another msg they can hit reload
      // - submission in progress
      val enabled: Enabled =
        Disabled.when(s.submitted || inFlight)

      val onSubmit: Option[Callback] =
        Common.validationOffUntilFirstSubmit(
          enabled,
          s.vux,
          p.state.modState(State.vux set ValidationUX.Highlight),
          s.req.validate.toOption.map(submit(p, _)))

      val reqSS = p.state.zoomStateL(State.req)

      var fields = textTields.map(_(reqSS))

      fields :+=
        Form.Field.booleanCentered
          .withLabel("Subscribe to newsletter")
          .withState(reqSS zoomStateL Request.Untyped.newsletter)

      fields :+=
        Form.Field.around(
          ^.textAlign.center,
          GeneralTheme.submitButton("Express Interest", onSubmit, inFlight = inFlight))

      if (enabled is Disabled)
        fields = fields.map(_.disable)

      <.div(*.formCont,
        <.form(*.form, Form(fields)(s.vux)))
    }

    def render(p: Props): VdomElement =
      <.div(*.cont, header,
        <.div(*.part2,
          yap(),
          renderForm(p)))
  }

  val Component = ScalaComponent.builder[Props]("LandingPage")
    .renderBackend[Backend]
    .build
}