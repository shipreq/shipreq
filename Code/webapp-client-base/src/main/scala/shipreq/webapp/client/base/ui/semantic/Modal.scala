package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

final class Modal(val render: VdomElement, val show: Callback)

object Modal {

  private var counter = 0

  def apply(header: VdomNode, content: VdomNode): Modal = {
    counter += 1

    val id = "semuimodal-" + counter

    val render: VdomElement =
      <.div(
        ^.id := id,
        ^.cls := "ui long modal",
        Icon.Close.tag,
        <.div(^.cls := "header", header),
        <.div(^.cls := "content", content))

    val component =
      ScalaComponent.builder.static("Modal")(render)
        .componentDidMount($ => Callback(JQuery($.getDOMNode).modal()))
        .build

    val show = Callback(JQuery.byId(id).modal("show"))

    new Modal(component(), show)
  }
}
