package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TopNode
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js

/** http://semantic-ui.com/modules/popup.html */
object Popup {

  sealed abstract class Position(val value: String) {
    def toReact = Position.position := value
  }
  object Position {
    private val position = VdomAttr("data-position")

    case object BottomCenter extends Position("bottom center")
    case object BottomLeft   extends Position("bottom left")
    case object BottomRight  extends Position("bottom right")
    case object LeftCenter   extends Position("left center")
    case object RightCenter  extends Position("right center")
    case object TopCenter    extends Position("top center")
    case object TopLeft      extends Position("top left")
    case object TopRight     extends Position("top right")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** No JavaScript required. */
  object Css {

    private val tooltip = VdomAttr("data-tooltip")

    def apply(text: String): TagMod =
      tooltip := text

    def apply(text: String, position: Position): TagMod =
      TagMod(tooltip := text, position.toReact)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Uses JavaScript. */
  object Js {

    trait Options extends js.Object {
      import Options._
      val closable      : js.UndefOr[Boolean] = js.undefined
      val delay         : js.UndefOr[Delay  ] = js.undefined
      val duration      : js.UndefOr[Int    ] = js.undefined
      val distanceAway  : js.UndefOr[Int    ] = js.undefined
      val inline        : js.UndefOr[Boolean] = js.undefined
      val hoverable     : js.UndefOr[Boolean] = js.undefined
      val lastResort    : js.UndefOr[Boolean] = js.undefined
      val movePopup     : js.UndefOr[Boolean] = js.undefined
      val observeChanges: js.UndefOr[Boolean] = js.undefined
      val offset        : js.UndefOr[Int    ] = js.undefined
      val preserve      : js.UndefOr[Boolean] = js.undefined
      val position      : js.UndefOr[String ] = js.undefined
      val setFluidWidth : js.UndefOr[String ] = js.undefined
    }

    object Options {
      trait Delay extends js.Object {
        val show: js.UndefOr[Int] = js.undefined
        val hide: js.UndefOr[Int] = js.undefined
      }
    }

    private val uiPopup = <.div(^.cls := "ui popup")

    case class Props(options: Options,
                     anchor: VdomTag,
                     popup: TagMod) {
      @inline def render = Component(this)
    }

    final class Backend($: BackendScope[Props, Unit]) {

      val anchorDom = Ref[TopNode]

      def render(p: Props): VdomElement =
        <.div(
          p.anchor.withRef(anchorDom),
          uiPopup(p.popup))

      val applyPopup: Callback =
        $.props.flatMap(p =>
          anchorDom.foreach(JQuery(_).popup(p.options)))
    }

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .componentDidMount(_.backend.applyPopup)
      .componentDidUpdate(_.backend.applyPopup)
      .build

  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val uiCheckbox = <.div(^.cls := "ui checkbox")

  def renderCheckbox(checkbox: VdomTagOf[html.Input], label: TagMod): VdomTag =
    <.div(
      uiCheckbox(
        checkbox,
        <.label(label)))
}
