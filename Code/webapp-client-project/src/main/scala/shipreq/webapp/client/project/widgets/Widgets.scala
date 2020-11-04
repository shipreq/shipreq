package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.webapp.base.util.On

object Widgets {

  val checkbox: On => VdomTagOf[html.Input] =
    On.memo(on =>
      <.input.checkbox(
        ^.checked := (on is On)))

  val checkboxReadOnly: On => VdomTagOf[html.Input] =
    On.memo(checkbox(_)(^.readOnly := true, ^.disabled := true))

}
