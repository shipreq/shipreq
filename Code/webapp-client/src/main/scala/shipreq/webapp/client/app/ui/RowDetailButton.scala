package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import scalaz.Equal
import scalaz.syntax.equal._

object RowDetailButton {

  case class Props(isActive: Boolean, onChange: Callback) {
    def component = Component(this)
  }

  object Props {
    def forRow[A: Equal](thisRow: A)
                        (activeRow: Option[A],
                         onChange: Option[A] => Callback): Props = {
      val isActive = activeRow.fold(false)(_ ≟ thisRow)
      def nextState: Option[A] = if (isActive) None else Some(thisRow)
      Props(isActive, Callback lazily onChange(nextState))
    }
  }

  val Component = ReactComponentB[Props]("RowFocus")
    .render(render(_))
    .build

  def render(p: Props): ReactElement =
    <.button(
      ^.cls := "detail",
      ^.onClick --> p.onChange,
      "Detail")
}