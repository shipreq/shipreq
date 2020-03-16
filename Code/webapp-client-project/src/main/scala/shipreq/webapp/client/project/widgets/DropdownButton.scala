package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.ui.semantic.{Dropdown, Icon, UsesSemanticUiManually}

/** A stateless button that shows a dropdown when clicked.
  * When a choice is chosen, an action is performed.
  *
  * Eg. [ + Add child... ]
  *
  * For Reusability, create an instance of Props and cache it.
  */
@UsesSemanticUiManually
object DropdownButton {

  final case class Props(icon   : Icon,
                         label  : VdomNode,
                         items  : Vector[Item],
                         enabled: Enabled) {
    @inline def render: VdomElement = Component(this)
  }

  final case class Item(label: VdomNode, select: Option[Callback])

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.byRef

  private val disabled =
    TagMod(^.cls := "disabled", ^.disabled := true)

  private val itemDiv =
    <.div(^.cls := "item")

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomNode =
      <.div(
        ^.cls := "ui floating labeled icon dropdown button",
        disabled.when(p.enabled.is(Disabled || p.items.isEmpty)),
        p.icon.tag,
        <.span(p.label),
        <.div(^.cls := "menu",
          p.items.indices.toVdomArray { i =>
            val item = p.items(i)
            itemDiv(
              ^.key := i,
              ^.onClick -->? item.select,
              disabled.when(item.select.isEmpty),
              item.label)
          }
        )
      )

    val enableDropdown: Callback =
      Dropdown.enable($.getDOMNode)
  }

  val Component = ScalaComponent.builder[Props]("DropdownButton")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
