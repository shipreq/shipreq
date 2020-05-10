package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{Dead, FilterDead, HideDead, Live, ShowDead}
import shipreq.webapp.base.ui.semantic._

object FilterDeadButton {

  private val renderButton =
    FilterDead.memo[VdomTag] { fd =>

      val (btnColour, iconColour, tipText) = fd match {
        case HideDead => (Colour.Default, Colour.Grey   , "Click to show deleted content.")
        case ShowDead => (Colour.Red    , Colour.Default, "Showing deleted content.")
      }

      val icon  = Icon.TrashOutline.withSize(Size.Large).withColour(iconColour)
      val btn   = Button(tipe = Button.Type.IconOnly(icon), colour = btnColour)
      val popup = Popup.Css(tipText, Popup.Position.LeftCenter)

      btn.tag(popup)
    }

  type Props = StateSnapshot[FilterDead]

  private def render(props: Props) = {
    val fd = props.value
    renderButton(fd)(^.onClick --> props.setState(!fd))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build

  val ForceHideDead: VdomElement =
    renderButton(HideDead)(^.disabled := true)

  val ForceShowDead: VdomElement =
    renderButton(ShowDead)(^.disabled := true)

  /** Say the user is viewing a dead req, then are already looking at dead content which is ShowDead. */
  def whenLive(live: Live)(props: => Props): VdomElement =
    live match {
      case Live => Component(props)
      case Dead => ForceShowDead
    }
}
