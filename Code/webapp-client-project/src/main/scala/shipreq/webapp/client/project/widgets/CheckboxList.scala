package shipreq.webapp.client.project.widgets

import cats.Eq
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.util._
import shipreq.webapp.client.project.widgets.CheckboxList._
import shipreq.webapp.member.project.util.DataReusability._

object CheckboxList {
  final case class RenderItem(checkbox: VdomTagOf[html.Input], label: String)
  type RenderFn = Iterator[RenderItem] => VdomElement
}

final case class CheckboxList[A: Reusability](renderFn: RenderFn) {

  case class Item(value: A, label: String, on: On, enabled: Enabled)

  case class Props(items: NonEmptyVector[Item], update: Update ~=> Callback) {
    @inline def render: VdomElement = Component(this)
  }

  case class Update(clickedItem: Item, items: NonEmptyVector[Item]) {
    def newValue: On =
      !clickedItem.on

    def newSelection(implicit eq: Eq[A]): Vector[A] = {
      val f: Item => Boolean =
        newValue match {
          case On  => i => i.on.is(On) || eq.eqv(i.value, clickedItem.value)
          case Off => i => i.on.is(On) && !eq.eqv(i.value, clickedItem.value)
        }
      items.iterator.filter(f).map(_.value).toVector
    }

    def update(as: Vector[A])(implicit eq: Eq[A]): Vector[A] =
      newValue match {
        case On  => as :+ clickedItem.value
        case Off => as.filter(!eq.eqv(_, clickedItem.value))
      }
  }

  implicit def reusabilityItem: Reusability[Item] = Reusability.derive
  implicit def reusabilityProps: Reusability[Props] = Reusability.derive

  private def render(p: Props): VdomElement =
    renderFn(
      p.items.iterator.map { i =>
        val checkbox: VdomTagOf[html.Input] =
          i.enabled match {
            case Enabled  => Widgets.checkbox(i.on)(^.onChange --> p.update(Update(i, p.items)))
            case Disabled => Widgets.checkboxReadOnly(i.on)
          }
        RenderItem(checkbox, i.label)
      }
    )

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
