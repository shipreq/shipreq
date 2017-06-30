package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.{AssetManifest, WebappConfig}
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.client.public.Styles.{landingPage => *}
import shipreq.webapp.client.public.protocol.LandingPageProtocol.Request

object LandingPage {

  final case class Props() {
    @inline def render = Component(this)
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
    def render(p: Props): VdomElement = {

      val form = {
        val fieldName = Input.Text.icon(Icon.User.tag, <.input.text(^.placeholder := Request.labelName))
        val fieldMail = Input.Text.icon(Icon.Mail.tag, <.input.text(^.placeholder := Request.labelEmail))
        val fieldText = <.textarea(^.rows := 12, ^.placeholder := "What would you like to say?")
        val fieldNews = Form.Field.center(Input.Checkbox(EmptyVdom, "Subscribe to newsletter"))
        val fieldSend = Form.Field.center(Button(colour = Colour.Blue, size = Size.Large).tag(*.formSubmit, "Express Interest"))
        <.div(*.formCont, <.div(*.form, Form(fieldName, fieldMail, fieldText, fieldNews, fieldSend)))
      }

      <.div(*.cont, header,
        <.div(*.part2, yap(), form))
    }
  }

  val Component = ScalaComponent.builder[Props]("LandingPage")
    .renderBackend[Backend]
    .build
}