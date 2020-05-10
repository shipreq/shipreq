package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.ReactKeyGen

object Select {

  type OptionKey = String

  final case class Option[+A](key: OptionKey, title: VdomNode, value: A)

  private[this] val item = <.div(^.cls := "item")

  sealed trait Props {
    type A
    val options : Iterable[Option[A]]
    val selected: scala.Option[OptionKey]
    val enabled : Enabled
    val tagMod  : TagMod
    val validity: Validity
    val onChange: Option[A] => Callback
  }

  def apply[A](options : Iterable[Select.Option[A]],
               selected: scala.Option[OptionKey] = None,
               enabled : Enabled                 = Enabled,
               validity: Validity                = Valid,
               tagMod  : TagMod                  = EmptyVdom)
              (onChange: Select.Option[A] => Callback): VdomElement = {
    type AA       = A
    val _options  = options
    val _selected = selected
    val _enabled  = enabled
    val _validity = validity
    val _tagMod   = tagMod
    val _onChange = onChange
    Component(new Props {
      override type A = AA
      override val options  = _options
      override val selected = _selected
      override val enabled  = _enabled
      override val validity = _validity
      override val tagMod   = _tagMod
      override val onChange = _onChange
    })
  }

  private class Backend($: BackendScope[Props, Unit]) {
    val enableDropdown: Callback =
      Dropdown.enable($.getDOMNode)

    private val keyGen = new ReactKeyGen

    def render(p: Props): VdomNode = {
      import p._

      val itemArray = VdomArray.empty()

      itemArray ++=
        options.iterator.map(o =>
          item(
            ^.key := o.key,
            ^.onClick --> Callback.byName(onChange(o)),
            Dropdown.itemValue := o.key,
            o.title))

      <.div(
        ^.key := keyGen.next(), // Forces DOM replacement - otherwise it retains Semantic UI JS's modifications
        tagMod,
        ^.cls := "ui selection dropdown",
        (^.cls := "disabled").when(enabled is Disabled),
        (^.cls := "error").when(validity is Invalid),
        <.input.hidden(^.value := selected.getOrElse("")),
        Icon.Dropdown.tag,
        <.div(^.cls := "default text"),
        <.div(^.cls := "menu", itemArray))
    }
  }

  private val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
