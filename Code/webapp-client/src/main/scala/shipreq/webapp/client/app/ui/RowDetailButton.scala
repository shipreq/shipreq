package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^.{Tag => ReactTag, Modifier => TagMod, _}, ScalazReact._
import scalaz.Equal
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal._
import scalaz.effect.IO

object RowDetailButton {

  case class Props(isActive: Boolean, onChange: IO[Unit]) {
    def component = Component(this)
  }

  object Props {
    def forRow[A: Equal](thisRow: A)
                        (activeRow: Option[A],
                         onChange: Option[A] => IO[Unit]): Props = {
      val isActive = activeRow.fold(false)(_ ≟ thisRow)
      def nextState: Option[A] = if (isActive) None else Some(thisRow)
      Props(isActive, IO(onChange(nextState)).join)
    }
  }

  val Component = ReactComponentB[Props]("RowFocus")
    .render(render(_))
    .build

  def render(p: Props): ReactElement =
    <.button(
      ^.cls := "detail",
      ^.onclick ~~> p.onChange,
      "Detail")
}