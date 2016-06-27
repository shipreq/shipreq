package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.data.{FilterDead, HideDead, ShowDead}
import shipreq.webapp.client.base.ui.semantic._

object FilterDeadButton {

  private val renderButton =
    FilterDead.memo[ReactTag] { fd =>

      val (btnColour, iconColour, tipText) = fd match {
        case HideDead => (Colour.Default, Colour.Grey , "Click to show deleted content.")
        case ShowDead => (Colour.Red    , Colour.White, "Showing deleted content.")
      }

      val icon  = Icon.TrashOutline.withSize(Size.Large).withColour(iconColour)
      val btn   = Button(tipe = Button.Type.IconOnly(icon), colour = btnColour)
      val popup = Popup.Css(tipText, Popup.Css.Position.LeftCenter)

      btn.tag(popup)
    }

  type Props = ReusableVar[FilterDead]

  private def render(props: Props) = {
    val fd = props.value
    renderButton(fd)(^.onClick --> props.set(!fd))
  }

  val Component = ReactComponentB[Props]("FilterDeadButton")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
