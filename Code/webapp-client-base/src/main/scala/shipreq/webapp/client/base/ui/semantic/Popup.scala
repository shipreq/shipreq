package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._

/** http://semantic-ui.com/modules/popup.html */
object Popup {

  /** No JavaScript required. */
  object Css {

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

    private val tooltip = VdomAttr("data-tooltip")

    def apply(text: String): TagMod =
      tooltip := text

    def apply(text: String, position: Position): TagMod =
      TagMod(tooltip := text, position.toReact)
  }
}
