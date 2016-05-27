package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.client.base.ui.semantic._

object TextInputAndButton {

  type Result = Option[TagMod \/ Callback]

  final case class Props(text       : ReusableVar[String],
                         result     : Result,
                         placeholder: String,
                         buttonLabel: String) {
    @inline def render = Component(this)
  }

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.caseClass

  private val action = <.div(^.cls := "ui action input")
  private val errLabel = <.div(^.cls := "ui pointing red basic label")

  val buttonOk       = Button(`type` = Button.Type.Primary)
  val buttonDisabled = Button(`type` = Button.Type.Primary, state = Button.State.Disabled)
  val buttonError    = Button(`type` = Button.Type.Negative, state = Button.State.Disabled)

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement = {
      val onChange = (_: ReactEventI).extract(_.target.value)(p.text.set)

      val input =
        <.input.text(
          ^.placeholder := p.placeholder,
          ^.value       := p.text.value,
          ^.onChange   ==> onChange)

      p.result match {
        case None =>
          <.div(
            <.div(
              action(
                input,
                buttonDisabled.tag(p.buttonLabel))))
        case Some(\/-(commit)) =>
          <.div(
            <.div(
              action(
                input,
                buttonOk.tag(
                  ^.onClick --> commit,
                  p.buttonLabel))))
        case Some(-\/(err)) =>
          <.div(
            <.div(
              action(^.cls := "error",
                input,
                buttonError.tag(p.buttonLabel))),
            errLabel(err))
      }
    }
  }

  val Component = ReactComponentB[Props]("TI&B")
    .renderBackend[Backend]
//    .configure(Reusability.shouldComponentUpdate)
    .build
}
