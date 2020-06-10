package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.semantic._
import shipreq.webapp.client.project.app.Style.{widgets => *}


/** A button with a dropdown on the right-hand side.
  *
  * Eg. [ New | Tag ↓ ]
  */
@UsesSemanticUiManually
object ButtonAndDropdown {

  class Types[A] {
    final type Item    = ButtonAndDropdown.Item[A]
    final type Update  = ButtonAndDropdown.Update[A]
    final type DBProps = ButtonAndDropdown.Props.Of[A]

    @inline final def Item = ButtonAndDropdown.Item

    @inline final def Update(click: A => Callback, select: A => Callback): Update =
      ButtonAndDropdown.Update(click = click, select = select)
  }

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

  final case class Update[-A](click: A => Callback, select: A => Callback)

  sealed trait Props {
    type A
    val items         : NonEmptyVector[Item[A]]
    val selected      : Option[A]
    val update        : Option[Reusable[Update[A]]]
    val buttonLabel   : String
    val icon          : Icon
    val colour        : ColourPlus
    val buttonTagMod  : TagMod
    val dropdownTagMod: TagMod

    implicit def univEqA: UnivEq[A]

    lazy val selectedItem: Item[A] =
      selected
        .flatMap(s => items.whole.find(_.value ==* s))
        .getOrElse(items.head)

    @inline final def render: VdomNode = Component(this)
  }

  object Props {

    type Of[AA] = Props { type A = AA }

    def apply[A](items         : NonEmptyVector[Item[A]],
                 selected      : Option[A],
                 update        : Option[Reusable[Update[A]]],
                 buttonLabel   : String,
                 icon          : Icon,
                 colour        : ColourPlus              = ColourPlus.Default,
                 buttonTagMod  : TagMod                  = TagMod.empty,
                 dropdownTagMod: TagMod                  = TagMod.empty,
               )(implicit A: UnivEq[A]): Of[A] = {
      type _A             = A
      val _items          = items
      val _selected       = selected
      val _update         = update
      val _buttonLabel    = buttonLabel
      val _icon           = icon
      val _colour         = colour
      val _buttonTagMod   = buttonTagMod
      val _dropdownTagMod = dropdownTagMod
      new Props {
        override type A               = _A
        override val items            = _items
        override val selected         = _selected
        override val update           = _update
        override val buttonLabel      = _buttonLabel
        override val icon             = _icon
        override val colour           = _colour
        override val buttonTagMod     = _buttonTagMod
        override val dropdownTagMod   = _dropdownTagMod
        override implicit def univEqA = A
      }
    }

    def forNew[A](items         : NonEmptyVector[Item[A]],
                  selected      : Option[A],
                  update        : Option[Reusable[Update[A]]],
                  buttonTagMod  : TagMod = TagMod.empty,
                )(implicit A: UnivEq[A]): Of[A] =
      apply(
        items          = items,
        selected       = selected,
        update         = update,
        buttonLabel    = "New",
        icon           = Icon.Plus,
        colour         = Colour.Green,
        buttonTagMod   = buttonTagMod,
        dropdownTagMod = *.dropdownButtonGreenDropdown,
      )

    implicit val reusability: Reusability[Props] =
      Reusability.byRef
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {

    private val dropdownNode = Ref[html.Element]

    def render(p: Props): VdomNode = {
      import p.univEqA

      def renderButton: VdomTag =
        <.a(
          ^.cls := "ui button" <+ p.colour,
          p.icon.tag,
          p.buttonLabel,
          ^.onClick -->? p.update.map(_.click(p.selectedItem.value)))

      def renderDropdown: VdomTag =
        <.div.withRef(dropdownNode)(
          ^.cls := "ui dropdown label",
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
        p.buttonTagMod,
        ^.cls := "ui labeled button",
        (^.cls := "disabled").when(p.update.isEmpty),
        renderButton,
        renderDropdown)
    }

    private def selectItem(key: String): Callback =
      for {
        p <- $.props.toCBO
        u <- CallbackOption.liftOption(p.update)
        i <- CallbackOption.liftOption(p.items.find(_.key ==* key))
        _ <- u.select(i.value).toCBO
      } yield ()

    private val dropdownOptions: Dropdown.JsOptions =
      Dropdown.JsOptions.default.withOnChange(selectItem(_).runNow())

    val enableDropdown: Callback =
      dropdownNode.foreach(JQuery(_).dropdown(dropdownOptions))
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
