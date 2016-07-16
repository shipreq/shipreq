package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

final class Modal(val render: ReactElement, val show: Callback)

object Modal {

  private var counter = 0

  def apply(header: ReactNode, content: ReactNode): Modal = {
    counter += 1

    val id = "semuimodal-" + counter

    val render: ReactElement =
      <.div(
        ^.id := id,
        ^.cls := "ui long modal",
        Icon.Close.tag,
        <.div(^.cls := "header", header),
        <.div(^.cls := "content", content))

    val component =
      ReactComponentB.static("Modal", render)
        .componentDidMount($ => Callback(JQuery($.getDOMNode()).modal()))
        .build

    val show = Callback(JQuery.byId(id).modal("show"))

    new Modal(component(), show)
  }
}
