package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.{Disabled, Enabled, On}

object CheckboxList {
  case class Item[A](value: A, label: String, on: On, enabled: Enabled)
  case class Props[A](items: Vector[Item[A]], toggle: A ~=> Callback)

  implicit def reusabilityItem[A: Reusability] = Reusability.caseClass[Item[A]]
  implicit def reusabilityProps[A: Reusability] = Reusability.caseClass[Props[A]]
}

class CheckboxList[A: Reusability](renderFn: Vector[ReactTag] => ReactElement) {
  import CheckboxList._

  def render(p: Props[A]) =
    renderFn(
      p.items.map { i =>
        val mod = i.enabled match {
          case Enabled  => ^.onChange --> p.toggle(i.value)
          case Disabled => ^.disabled := true
        }
        val box = UI.checkbox(i.on)(mod)
        <.label(box, i.label)
      })

  val Component = ReactComponentB[Props[A]]("Columns")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}