package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.ui.semantic.{Dropdown, Icon, UsesSemanticUiManually}

@UsesSemanticUiManually
object DropdownAndTextEditor {

  trait Props {
    type A
    val items          : NonEmptyVector[A]
    val renderItem     : A => TagMod
    val state          : StateSnapshot[State[A]]
    val textLiveCorrect: String => String
    val enabled        : Enabled
    val outerTagMod    : TagMod
    val dropdownTagMod : TagMod
    val inputTagMod    : TagMod
    def univEq         : UnivEq[A]

    @inline final def render: VdomElement = Component(this)
  }

  object Props {
    type Of[I] = Props { type A = I }

    def apply[A](items          : NonEmptyVector[A],
                 renderItem     : A => TagMod,
                 state          : StateSnapshot[State[A]],
                 enabled        : Enabled,
                 textLiveCorrect: String => String = identity,
                 outerTagMod    : TagMod           = EmptyVdom,
                 dropdownTagMod : TagMod           = EmptyVdom,
                 inputTagMod    : TagMod           = EmptyVdom,
                )(implicit ue: UnivEq[A]): Of[A] = {
      type _A              = A
      val _items           = items
      val _renderItem      = renderItem
      val _state           = state
      val _enabled         = enabled
      val _textLiveCorrect = textLiveCorrect
      val _outerTagMod     = outerTagMod
      val _dropdownTagMod  = dropdownTagMod
      val _inputTagMod     = inputTagMod
      new Props {
        override type A              = _A
        override val items           = _items
        override val renderItem      = _renderItem
        override val state           = _state
        override val enabled         = _enabled
        override val textLiveCorrect = _textLiveCorrect
        override val outerTagMod     = _outerTagMod
        override val dropdownTagMod  = _dropdownTagMod
        override val inputTagMod     = _inputTagMod
        override def univEq          = ue
      }
    }
  }

  final case class State[A](selected: A, text: String)

  implicit def reusabilityState[A: Reusability]: Reusability[State[A]] =
    Reusability.byRef || Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    private val onTextChange: ReactEventFromInput => Callback =
      e => {
        val i = e.target.value
        $.props.flatMap(p => p.state.modState(_.copy(text = p.textLiveCorrect(i))))
      }

    def render(p: Props): VdomNode = {
      import p.A
      val s = p.state.value

      def dropdownItem(a: A): VdomNode =
        <.div(
          ^.cls := "item",
          ^.onClick --> p.state.modState(_.copy(selected = a)),
          p.renderItem(a))

      val dropdown =
        <.div(^.cls := "ui dropdown label",
          p.dropdownTagMod,
          <.div(^.cls := "text", p.renderItem(s.selected)),
          Icon.Dropdown.tag,
          <.div(^.cls := "menu",
            p.items.whole.toTagMod(dropdownItem)))

      val input =
        <.input.text(
          p.inputTagMod,
          ^.value := s.text,
          ^.onChange ==> onTextChange)

      <.div(
        ^.cls := "ui labeled input",
        ^.borderLeftWidth := "0",
        p.outerTagMod,
        dropdown,
        input)
    }

    val enableDropdown: Callback =
      Dropdown.enable($.getDOMNode)
  }

  val Component = ScalaComponent.builder[Props]("DropdownAndTextEditor")
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
