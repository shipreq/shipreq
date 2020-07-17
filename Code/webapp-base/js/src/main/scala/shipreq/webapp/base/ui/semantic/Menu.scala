package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Node, html}
import shipreq.webapp.base.ui.semantic.{Dropdown => SDropdown}

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
    // case object Vertical   extends Attr("vertical") pruned from CSS
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

  case class Style(attr  : Multiple[Attr] = Multiple.empty,
                   size  : Size           = Size.Default,
                   tagMod: TagMod         = EmptyVdom) {

    val cont = divCls("ui menu" <+ attr <+ size)(tagMod)
  }

  sealed abstract class ItemState(c: ClassName) extends HasClass(c)
  object ItemState {
    case object Active   extends ItemState("active")
    case object Default  extends ItemState(NoClass)
    case object Disabled extends ItemState("disabled")
    case object Down     extends ItemState("down")
    implicit def univEq: UnivEq[ItemState] = UnivEq.derive

    def disabledWhen(b: Boolean): ItemState =
      if (b) Disabled else Default
  }

  sealed trait ItemType {
    def cont: VdomTag

    final def toItem: Item =
      toItem()

    final def toItem(state : ItemState = ItemState.Default,
                     colour: Colour    = Colour.Default,
                     tagMod: TagMod    = EmptyVdom): Item =
      Item(this, state, colour, tagMod)
  }
  object ItemType {
    private val item            = "item"
    private val divItem         = divCls(item)
    private val divItemDropdown = divItem.addClass("ui dropdown")
    private val divMenu         = divCls("menu")

    final case class Div(content: TagMod) extends ItemType {
      override def cont = divItem(content)
    }
    final case class Link(a: VdomTagOf[html.Anchor]) extends ItemType {
      override def cont = a.addClass(item)
    }
    final case class Dropdown(`type` : DropdownType,
                              content: TagMod,
                              items  : SDropdown.Items) extends ItemType {
      override def cont = divItemDropdown(content, divMenu(items.map(_.tag): _*)) <+ `type`
    }
  }

  sealed abstract class DropdownType(c: ClassName) extends HasClass(c) {
    @inline final def apply(content: TagMod, items: SDropdown.Items) =
      ItemType.Dropdown(this, content, items)
  }
  object DropdownType {
    case object Simple extends DropdownType("simple")
    case object OnHover extends DropdownType("onhover")
  }

  final case class Item(`type`: ItemType,
                        state : ItemState = ItemState.Default,
                        colour: Colour    = Colour.Default,
                        tagMod: TagMod    = EmptyVdom) {
    val cont: VdomTag =
      (`type`.cont <+ state <+ colour)(tagMod)

    /** Registers an onClick listener that only triggers when this item is clicked (and not its children or items in its
      * dropdown menu).
      */
    def withOnClick(getDOMNode: CallbackOption[Node], cb: Callback): Item = {
      val onClick: ReactEvent => Callback =
        e => Callback.when(e.target == e.currentTarget)(
          cb >> getDOMNode.map(Dropdown.jquery(_).dropdown("hide")).void)
      copy(tagMod = TagMod(tagMod, ^.onClick ==> onClick))
    }
  }

  type Items = Seq[Item]

  final case class Props(style          : Style,
                         leftItems      : Items,
                         rightItems     : Items               = Nil,
                         dropdownOptions: SDropdown.JsOptions = null) {
    @inline def render = Component(this)
  }

  private val divRightMenu = divCls("right menu")

  // implicit val reusabilityProps: Reusability[Props] =
  //   Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    val dropdownOptions: CallbackOption[SDropdown.JsOptions] =
      $.props.map(p => Option(p.dropdownOptions)).asCBO

    val enableDropdowns: Callback =
      for {
        o <- dropdownOptions
        n <- $.getDOMNode.map(_.toElement).asCBO
      } yield {
        Dropdown.jquery(n).dropdown(o)
        ()
      }

    def disableDropdown: Callback =
      $.getDOMNode.map(_.toElement.foreach(Dropdown.jquery(_).dropdown("hide")))

    def render(p: Props) =
      p.style.cont(
        TagMod(p.leftItems.map(_.cont): _*),
        TagMod.unless(p.rightItems.isEmpty)(
          divRightMenu(p.rightItems.map(_.cont): _*)))
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDropdowns)
    .componentDidUpdate(_.backend.enableDropdowns)
    .componentWillUnmount(_.backend.disableDropdown)
    .build
}
