package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TopNode
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js

/** http://semantic-ui.com/modules/popup.html */
object Popup {

  sealed abstract class Position(final val value: String) {
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
      var closable      : js.UndefOr[Boolean] = js.undefined
      var delay         : js.UndefOr[Delay  ] = js.undefined
      var duration      : js.UndefOr[Int    ] = js.undefined
      var distanceAway  : js.UndefOr[Int    ] = js.undefined
      var inline        : js.UndefOr[Boolean] = js.undefined
      var prefer        : js.UndefOr[String ] = js.undefined
      var hoverable     : js.UndefOr[Boolean] = js.undefined
      var lastResort    : js.UndefOr[Boolean] = js.undefined
      var movePopup     : js.UndefOr[Boolean] = js.undefined
      var observeChanges: js.UndefOr[Boolean] = js.undefined
      var offset        : js.UndefOr[Int    ] = js.undefined
      var preserve      : js.UndefOr[Boolean] = js.undefined
      var position      : js.UndefOr[String ] = js.undefined
      var setFluidWidth : js.UndefOr[String ] = js.undefined
    }

    object Options {
      trait Delay extends js.Object {
        var show: js.UndefOr[Int] = js.undefined
        var hide: js.UndefOr[Int] = js.undefined
      }
    }

    private val uiPopup = <.div(^.cls := "ui popup flowing", ^.display.none)

    final case class Props(options: Options,
                           base   : VdomTag,
                           display: VdomTag,
                           popup  : TagMod) {
      @inline def render = Component(this)
    }

    final class Backend($: BackendScope[Props, Unit]) {

      val displayRef = Ref[TopNode]

      def render(p: Props): VdomElement =
        p.base(
          p.display.withRef(displayRef),
          uiPopup(p.popup))

      val applyPopup: Callback =
        $.props.flatMap(p =>
          displayRef.foreach(JQuery(_).popup(p.options)))
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
