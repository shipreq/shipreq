package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.html
import scala.scalajs.js

/** http://semantic-ui.com/collections/menu.html
  */
object Menu {

  sealed abstract class Attr(cls: ClassName) extends HasClass(cls)
  object Attr {
    case object Borderless extends Attr("borderless")
    case object Fixed      extends Attr("fixed")
    case object Inverted   extends Attr("inverted")
    case object Pointing   extends Attr("pointing")
    case object Secondary  extends Attr("secondary")
    case object Vertical   extends Attr("vertical")
    implicit def univEq: UnivEq[Attr] = UnivEq.derive
  }

//   case object Fluid      extends Greed("fluid")
//   case object Compact    extends Greed("compact")

//  sealed abstract class Type(cs: ClassName*) extends HasClasses(cs)
//  object Type {
//    case object Icon         extends Type("icon")
//    case object LabelledIcon extends Type("labeled", "icon")
//    case object Text         extends Type("text")
//    implicit def univEq: UnivEq[Type] = UnivEq.derive
//  }

  case class Style(attr: Multiple[Attr] = Multiple.empty,
                   size: Size           = Size.Default) {

    val cont = divCls("ui menu" <+ attr <+ size)
  }

  sealed abstract class ItemState(c: ClassName) extends HasClass(c)
  object ItemState {
    case object Active  extends ItemState("active")
    case object Default extends ItemState(NoClass)
    case object Down    extends ItemState("down")
    implicit def univEq: UnivEq[ItemState] = UnivEq.derive
  }

  sealed abstract class Item {
    val cont: VdomTag
  }

  object Item {
    private val item                  = "item"
    private val divItem               = divCls(item)
    private val divItemDropdownSimple = divItem.addClass("ui dropdown simple")
    private val divMenu               = divCls("menu")

    case class Div(content: TagMod, state: ItemState = ItemState.Default) extends Item {
      override val cont = divItem(content) <+ state
    }

    case class Link(a: VdomTagOf[html.Anchor], state: ItemState = ItemState.Default) extends Item {
      override val cont = a.addClass(item) <+ state
    }

    case class DropdownSimple(content: TagMod, items: Dropdown.Items) extends Item {
      override val cont = divItemDropdownSimple(content, divMenu(items.map(_.tag): _*))
    }
  }

  type Items = Seq[Item]

  final case class Props(style     : Style,
                         leftItems : Items,
                         rightItems: Items = Nil) {
    @inline def render = Component(this)
  }

  private val divRightMenu = divCls("right menu")

  // implicit val reusabilityProps: Reusability[Props] =
  //   Reusability.caseClass

  final class Backend($: BackendScope[Props, Unit]) {

    val options: Dropdown.JsOptions =
      new Dropdown.JsOptions {
        override val action = Dropdown.JsOptions.Action.Hide
      }

    val enableDropdowns: Callback =
      $.getDOMNode.map { node =>
        JQuery(node).find(".ui.dropdown").dropdown(options)
      }

    def render(p: Props) =
      p.style.cont(
        TagMod(p.leftItems.map(_.cont): _*),
        if (p.rightItems.isEmpty)
          EmptyVdom
        else
          divRightMenu(p.rightItems.map(_.cont): _*))
  }

  val Component = ScalaComponent.builder[Props]("Menu")
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDropdowns)
    .componentDidUpdate(_.backend.enableDropdowns)
    .build
}
