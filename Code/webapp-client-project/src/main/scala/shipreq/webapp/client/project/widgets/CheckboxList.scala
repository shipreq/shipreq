package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.client.base.data.{Disabled, Enabled, On}
import shipreq.webapp.client.project.lib.DataReusability._

object CheckboxList {
  case class Item[A](value: A, label: String, on: On, enabled: Enabled)
  case class Props[A](items: Vector[Item[A]], toggle: A ~=> Callback)

  implicit def reusabilityItem[A: Reusability] = Reusability.caseClass[Item[A]]
  implicit def reusabilityProps[A: Reusability] = Reusability.caseClass[Props[A]]
}

class CheckboxList[A: Reusability](renderFn: Vector[VdomTag] => VdomElement) {
  import CheckboxList._

  def render(p: Props[A]) =
    renderFn(
      p.items.map { i =>
        val mod = i.enabled match {
          case Enabled  => ^.onChange --> p.toggle(i.value)
          case Disabled => ^.disabled := true
        }
        val box = Widgets.checkbox(i.on)(mod)
        <.label(box, i.label)
      })

  val Component = ScalaComponent.builder[Props[A]]("Columns")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}