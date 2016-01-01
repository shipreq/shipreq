package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.webapp.client.data.On

object Widgets {

  val checkbox: On => ReactTag =
    On.memo(on =>
      <.input(
        ^.`type` := "checkbox",
        ^.checked := (on :: On)))

  val checkboxAlwaysOn =
    checkbox(On)(^.readOnly := true, ^.disabled := true)

}
