package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._, vdom.html_<^._
import shipreq.webapp.client.base.data.On

object Widgets {

  val checkbox: On => VdomTag =
    On.memo(on =>
      <.input.checkbox(
        ^.checked := (on :: On)))

  val checkboxAlwaysOn =
    checkbox(On)(^.readOnly := true, ^.disabled := true)

}
