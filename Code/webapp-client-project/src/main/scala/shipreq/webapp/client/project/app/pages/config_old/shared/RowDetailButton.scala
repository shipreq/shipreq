package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._
import vdom.html_<^._
import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.webapp.base.ui.semantic.{Button, Colour}

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

  val Component = ScalaComponent.builder[Props]("RowFocus")
    .render_P(render)
    .build

  def render(p: Props): VdomElement =
    Button(tipe = Button.Type.Basic, colour = Colour.Black).tag(
      ^.onClick --> p.onChange,
      "Detail")
}