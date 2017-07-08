package shipreq.webapp.client.public.pages

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalaz.{-\/, \/-}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.base.{AssetManifest, WebappConfig}
import shipreq.webapp.client.public.PublicSpaProtocols.LandingPage.Request
import shipreq.webapp.client.public.Styles.{landingPage => *}

object LandingPage {

  final case class Props(state: StateSnapshot[State],
                         asyncW: AsyncFeature.Write.D0[String],
                         submit: ServerSideProcInvoker[Request, Unit]) {
    @inline def render = Component(this)

    val async: AsyncFeature.ReadWrite.D0[String] =
      AsyncFeature.ReadWrite.D0(asyncW, state.value.async)
  }

  @Lenses
  final case class State(req      : Request.Untyped,
                         vux      : ValidationUX,
                         submitted: Boolean,
                         async    : AsyncFeature.State.D0[String])

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
        "Ship better products with better requirements."))

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
      m => Input.Text.icon(Icon.User.tag, <.input.text(^.placeholder := Request.labelName, m)))

    val fieldEmail = Form.TextField.highLevel(
      State.req ^|-> Request.Untyped.email,
      Request.validatorEmail,
      m => Input.Text.icon(Icon.Mail.tag, <.input.text(^.placeholder := Request.labelEmail, m)))

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
          e => f(e) >> Callback.alert(e)))

    private def renderForm(p: Props): VdomElement = {
      val s = p.state.value

      val onSubmit: Option[Callback] =
        (s.req.validate, s.vux) match {
          case (\/-(r), _)                => Some(submit(p, r))
          case (-\/(_), ValidationUX.Off) => Some(p.state.modState(State.vux set ValidationUX.Highlight))
          case (-\/(_), _)                => None
        }

      var fields = textTields.map(_(s.vux)(p.state))

      fields :+= Form.CenteredField(
        Input.Checkbox.fromStateSnapshot(
          State.req ^|-> Request.Untyped.newsletter,
          p.state,
          "Subscribe to newsletter"))

      fields :+= Form.CenteredField(
        Button(
          colour = Colour.Blue,
          state = Button.State.enabledWhen(onSubmit.isDefined),
          size = Size.Large)
          .tag(*.formSubmit,
            ^.onClick -->? onSubmit,
            "Express Interest"))

      // Disable form if:
      // - already submitted; 1 msg/user is enough, if they really want to send another msg they can hit reload
      // - submission in progress
      if (s.submitted || s.async.exists {
        case AsyncFeature.Status.InProgress => true
        case _                              => false
      })
        fields = fields.map(_.disable)

      <.div(*.formCont,
        <.div(*.form, Form(fields)))
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