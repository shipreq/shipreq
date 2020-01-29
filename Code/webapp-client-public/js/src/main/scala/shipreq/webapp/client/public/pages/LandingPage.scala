package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic._
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

    val fieldName = Form.TextField.highLevel(
      State.req ^|-> Request.Untyped.name,
      Request.validatorName,
      m => Input.Text.icon(Icon.User.tag, <.input.text(^.autoComplete.name, ^.placeholder := Request.labelName, m)))

    val fieldEmail = Form.TextField.highLevel(
      State.req ^|-> Request.Untyped.email,
      Request.validatorEmail,
      m => Input.Text.icon(Icon.Mail.tag, <.input.email(^.autoComplete.email, ^.placeholder := Request.labelEmail, m)))

    val fieldMsg = Form.TextField.highLevel(
      State.req ^|-> Request.Untyped.msg,
      Request.validatorMsg,
      <.textarea(^.rows := 12, ^.placeholder := "What would you like to say?")(_))

    val textTields: NonEmptyVector[ValidationUX => StateSnapshot[State] => Form.Field] =
      NonEmptyVector(fieldName, fieldEmail, fieldMsg)

    def submit(p: Props, r: Request): Callback =
      p.asyncW((s, f) =>
        p.submit(r,
          _ => s << p.state.modState(State.submitted.set(true)) >> Callback.alert("Great to hear from you.\n\nWe'll be in touch!"),
          e => f(e) >> Callback.alert(e.value)))

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

      var fields = textTields.map(_(s.vux)(p.state))

      fields :+= Form.BasicField.centered(
        Input.Checkbox.fromStateSnapshot(
          State.req ^|-> Request.Untyped.newsletter,
          p.state,
          "Subscribe to newsletter"))

      fields :+= Form.BasicField.centered(
        GeneralTheme.submitButton("Express Interest", onSubmit, inFlight = inFlight))

      if (enabled is Disabled)
        fields = fields.map(_.disable)

      <.div(*.formCont,
        <.form(*.form, Form(fields)))
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