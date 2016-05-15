package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.webapp.client.project.data.On

object Widgets {

  val checkbox: On => ReactTag =
    On.memo(on =>
      <.input.checkbox(
        ^.checked := (on :: On)))

  val checkboxAlwaysOn =
    checkbox(On)(^.readOnly := true, ^.disabled := true)

}
