package shipreq.webapp.client.base.ui.semantic

import japgolly.univeq._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.base.util.NonEmptyVector

object Accordion {

  type Props = NonEmptyVector[Item]

  final case class Item(title: ReactNode, content: ReactNode)

  final class Backend($: BackendScope[Props, Unit]) {

    def open = Callback {
      JQuery($.getDOMNode()).accordion("open")
    }

    def renderItem(i: Item, active: Boolean): TagMod = {
      val x = if (active) " active" else ""
      TagMod(
        divCls("title" + x)(Icon.Dropdown.tag, i.title),
        divCls("content" + x)(i.content))
    }

    def render(p: Props): ReactElement = {
      val activeIndex = 0
      val items = p.whole
      divCls("ui styled accordion fluid")(
        items.indices.map(i => renderItem(items(i), i ==* activeIndex)): _*)
    }
  }

  val Component = ReactComponentB[Props]("Accordion")
    .renderBackend[Backend]
    .componentDidMount(_.backend.open)
    .build
}
