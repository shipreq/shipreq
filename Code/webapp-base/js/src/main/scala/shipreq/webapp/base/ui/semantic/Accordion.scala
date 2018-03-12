package shipreq.webapp.base.ui.semantic

import japgolly.univeq._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.microlibs.nonempty.NonEmptyVector

object Accordion {

  type Props = NonEmptyVector[Item]

  final case class Item(title: VdomNode, content: VdomNode)

  final class Backend($: BackendScope[Props, Unit]) {

    def open: Callback =
      $.getDOMNode.map(n => JQuery(n.asElement).accordion("open"))

    def renderItem(i: Item, active: Boolean): TagMod = {
      val x = if (active) " active" else ""
      TagMod(
        divCls("title" + x)(Icon.Dropdown.tag, i.title),
        divCls("content" + x)(i.content))
    }

    def render(p: Props): VdomElement = {
      val activeIndex = 0
      val items = p.whole
      divCls("ui styled accordion fluid")(
        items.indices.map(i => renderItem(items(i), i ==* activeIndex)): _*)
    }
  }

  val Component = ScalaComponent.builder[Props]("Accordion")
    .renderBackend[Backend]
    .componentDidMount(_.backend.open)
    .build
}
