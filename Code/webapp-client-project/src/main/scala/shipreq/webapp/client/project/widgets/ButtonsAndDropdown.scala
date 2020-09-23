package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.client.project.app.Style.{widgets => *}


/** A button with a dropdown on the right-hand side.
  *
  * Eg. [ Imply | New | Tag ↓ ]
  */
@UsesSemanticUiManually
object ButtonsAndDropdown {

  final case class Item[+A](key             : String,
                            value           : A,
                            renderInButton  : VdomNode,
                            renderInDropdown: VdomNode)

  object Item {
    def apply[A](key   : String,
                 value : A,
                 render: VdomNode): Item[A] =
      apply(key, value, render, render)
  }

  final case class ButtonProps[-A](colour    : ColourPlus,
                                   label     : String,
                                   icon      : Icon,
                                   callback  : Option[Reusable[A => Callback]],
                                   inProgress: Boolean)
  sealed trait Props {
    type A
    val buttons       : NonEmptyVector[ButtonProps[A]]
    val items         : NonEmptyVector[Item[A]]
    val selectItem    : Option[Reusable[A => Callback]]
    val selected      : Option[A]
    val outerTagMod   : TagMod
    val dropdownTagMod: TagMod
    val basic         : Boolean

    implicit def univEqA: UnivEq[A]

    lazy val inProgress =
      buttons.exists(_.inProgress)

    lazy val selectedItem: Item[A] =
      selected
        .flatMap(s => items.whole.find(_.value ==* s))
        .getOrElse(items.head)

    @inline final def render: VdomNode = Component(this)
  }

  object Props {

    type Of[AA] = Props { type A = AA }

    def apply[A](buttons       : NonEmptyVector[ButtonProps[A]],
                 items         : NonEmptyVector[Item[A]],
                 selectItem    : Option[Reusable[A => Callback]],
                 selected      : Option[A],
                 outerTagMod   : TagMod  = TagMod.empty,
                 dropdownTagMod: TagMod  = TagMod.empty,
                 basic         : Boolean = false,
               )(implicit A: UnivEq[A]): Of[A] = {
      type _A             = A
      val _buttons        = buttons
      val _selectItem     = selectItem
      val _items          = items
      val _selected       = selected
      val _outerTagMod    = outerTagMod
      val _dropdownTagMod = dropdownTagMod
      val _basic          = basic
      new Props {
        override type A             = _A
        override val buttons        = _buttons
        override val selectItem     = _selectItem
        override val items          = _items
        override val selected       = _selected
        override val outerTagMod    = _outerTagMod
        override val dropdownTagMod = _dropdownTagMod
        override val basic          = _basic
        override implicit def univEqA = A
      }
    }

    implicit val reusability: Reusability[Props] =
      Reusability.byRef
  }

  // ===================================================================================================================

  private val middleButtonTagMod = TagMod(
    ^.borderBottomWidth := "1px!important",
    ^.borderBottomStyle := "solid!important",
    ^.borderTopWidth    := "1px!important",
    ^.borderTopStyle    := "solid!important",
    ^.borderRadius      := "0!important",
    ^.boxShadow         := "none!important",
  )

  final class Backend($: BackendScope[Props, Unit]) {

    private val dropdownNode = Ref[html.Element]

    def render(p: Props): VdomNode = {
      import p.{A, univEqA}

      val basic = (^.cls := "basic").when(p.basic)

      def renderButton(b: ButtonProps[A], first: Boolean): VdomTag =
        <.a(
          middleButtonTagMod.unless(first),
          ^.cls := "ui button" <+ b.colour,
          TagMod.when(p.inProgress)(^.cls := "loading"),
          basic,
          b.icon.tag,
          b.label,
          ^.onClick -->? b.callback.flatMap(c => Option.unless(p.inProgress)(c.value(p.selectedItem.value))),
        )

      def renderButtons = {
        val buttons = p.buttons.whole
        buttons.indices.toTagMod { i =>
          renderButton(buttons(i), first = i == 0)
        }
      }

      def renderDropdown: VdomTag =
        <.div.withRef(dropdownNode)(
          ^.cls := "ui dropdown label",
          basic,
          p.dropdownTagMod,
          p.selectedItem.renderInButton,
          Icon.Dropdown.tag,
          <.div(^.cls := "menu", renderDropdownItems))

      def renderDropdownItems: VdomArray =
        p.items.whole.toVdomArray(i =>
          <.div(
            ^.cls := "item",
            ^.key := i.key,
            Dropdown.itemValue := i.key,
            (^.cls := "active selected").when(p.selectedItem.value ==* i.value),
            i.renderInDropdown))

      <.div(
        *.dropdownButtonOuter,
        p.outerTagMod,
        ^.cls := "ui labeled button",
        (^.cls := "disabled").when(p.inProgress),
        renderButtons,
        renderDropdown)
    }

    private def selectItem(key: String): Callback =
      for {
        p <- $.props.toCBO
        _ <- CallbackOption.unless(p.inProgress)
        s <- CallbackOption.liftOption(p.selectItem)
        i <- CallbackOption.liftOption(p.items.find(_.key ==* key))
        _ <- s.value(i.value).toCBO
      } yield ()

    private val dropdownOptions: Dropdown.JsOptions =
      Dropdown.JsOptions.default.withOnChange(selectItem(_).runNow())

    val enableDropdown: Callback =
      for (n <- dropdownNode) {
        JQuery(n).dropdown(dropdownOptions)

        // Remove tabindex
        // The reason I did this is that when a ButtonsAndDropdown was added to ReqDetails' ReqTypeRow, it completely
        // broken the table navigation. Rather than try to be crafty, it's easier to just remove tabIndex here.
        // I'd rather just not support keyboard shortcuts for the dropdown (which was only indirectly and incidentally
        // supported anyway), and know that the explicit keyboard navigation feature that I've added will continue to
        // work whether a ButtonsAndDropdown exists or not.
        n.removeAttribute("tabindex")
        n.querySelectorAll("*[tabindex]").iterator.foreach(_.domAsHtml.removeAttribute("tabindex"))
      }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
