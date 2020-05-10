package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{Colour, DataValidators}
import shipreq.webapp.base.jsfacade.ReactColor
import shipreq.webapp.base.ui.semantic.{Button, Input}
import shipreq.webapp.client.project.app.Style.{widgets => *}

/** Note: validation errors aren't presented as part of this. */
object ColourPicker {

  final case class Props(state  : StateSnapshot[State],
                         palette: ReactColor.Github.Colours) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  sealed trait PickerType
  object PickerType {
    case object Github extends PickerType
    case object Chrome extends PickerType

    implicit def reusability: Reusability[PickerType] =
      Reusability.derive

    implicit def univEq: UnivEq[PickerType] =
      UnivEq.derive
  }

  final case class State(text: String, openPicker: Option[PickerType]) {

    val validated =
      DataValidators.colour.unnamed(text)
  }

  object State {
    def init(c: Option[Colour]): State =
      State(c.fold("")(_.value), None)

    implicit val reusability: Reusability[State] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private def onButtonClick(t: PickerType): Callback =
      $.props.flatMap(_.state.modState { s =>
        val o = Some(t)
        s.copy(openPicker = if (s.openPicker ==* o) None else o)
      })

    private val githubButton =
      Button.text("Palette").onClick(onButtonClick(PickerType.Github))

    private val chromeButton =
      Button.text("Advanced").onClick(onButtonClick(PickerType.Chrome))

    def render(p: Props): VdomNode = {
      val s = p.state.value

      def onChange(str: String): Callback = {
        val c = DataValidators.colour.unnamed.corrector.live(str)
        p.state.modState(_.copy(c))
      }

      def onInputChange(e: ReactEventFromInput): Callback =
        onChange(e.target.value)

      val input =
        <.input.text(
          ^.spellCheck := false,
          ^.value := s.text,
          ^.onChange ==> onInputChange)

      val picker: Option[VdomElement] =
        s.openPicker.map {

          case PickerType.Github =>
            val g = ReactColor.Github.Props.hex(s.text, onChange, p.palette)
            ReactColor.Github.Component(g)

          case PickerType.Chrome =>
            val p = ReactColor.Chrome.Props.hex(s.text, onChange, disableAlpha = true)
            ReactColor.Chrome.Component(p)
        }

      val githubButton =
        this.githubButton(Button.active.when(s.openPicker.contains(PickerType.Github)))

      val chromeButton =
        this.chromeButton(Button.active.when(s.openPicker.contains(PickerType.Chrome)))

      <.div(*.colourPicker,
        Input.Text.withRightButtons(input, githubButton, chromeButton),
        picker.whenDefined(<.div(*.colourPickerPickler, _)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}