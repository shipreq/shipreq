package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html

/** http://semantic-ui.com/collections/breadcrumb.html
  */
object Breadcrumb {

  case class Style(size: Size = Size.Default) {
    val cont = divCls("ui breadcrumb" <+ size)
  }

  sealed abstract class ItemState(c: ClassName) extends HasClass(c)
  object ItemState {
    case object Active  extends ItemState("active")
    case object Default extends ItemState(NoClass)
    implicit def univEq: UnivEq[ItemState] = UnivEq.derive
  }

  sealed abstract class Item {
    val tag: VdomTag
  }

  object Item {
    private val section      = "section"
    private val divSection   = divCls(section)
    private val divider      = divCls("divider")
    private val dropdown     = divCls("ui dropdown inline")
    private val dropdownMenu = divCls("menu")
    private val nbsp         = <.span(^.width := "1ex", ^.display := "inline-block")

    case class Div(content: TagMod, state: ItemState = ItemState.Default) extends Item {
      override val tag = divSection(content) <+ state
    }

    case class Link(a: VdomTagOf[html.Anchor], state: ItemState = ItemState.Default) extends Item {
      override val tag = a.addClass(section) <+ state
    }

    case class Divider(content: TagMod) extends Item {
      override val tag = divider(content)
    }

    case class DividerIcon(i: Icon, mod: TagMod = EmptyVdom) extends Item {
      override val tag = i.tag(^.cls := "divider", mod)
    }

    case class DropDown(title: TagMod, items: Dropdown.Items) extends Item {
      override val tag =
        divSection(
          dropdown(
            title, nbsp, Icon.Dropdown.tag,
            dropdownMenu(
              items.toTagMod(_.tag))))
    }

    case class LinkAndDropdown(a: VdomTagOf[html.Anchor], items: Dropdown.Items) extends Item {
      override val tag =
        divSection(
          a,
          divSection(
            dropdown(
              nbsp, Icon.Dropdown.tag,
              dropdownMenu(
                items.toTagMod(_.tag)))))
    }
  }

  type Items = Seq[Item]

  final case class Props(style: Style, items: Items) {
    @inline def render = Component(this)
  }

  private def render(p: Props) =
    p.style.cont(p.items.map(_.tag): _*)

  val Component = ScalaFnComponent(render)
}

