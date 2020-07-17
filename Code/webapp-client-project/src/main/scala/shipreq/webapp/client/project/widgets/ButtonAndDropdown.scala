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
  * Eg. [ New | Tag ↓ ]
  */
@UsesSemanticUiManually
object ButtonAndDropdown {

  class Types[A] {
    final type DropdownValue = A
    final type Item          = ButtonAndDropdown.Item[A]
    final type Callbacks     = ButtonAndDropdown.Callbacks[A]
    final type DBProps       = ButtonAndDropdown.Props.Of[A]

    @inline final def Item = ButtonAndDropdown.Item

    @inline final def Callbacks(click: A => Callback, select: A => Callback): Callbacks =
      ButtonAndDropdown.Callbacks(click = click, select = select)
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

  final case class Callbacks[-A](click: A => Callback, select: A => Callback)

  sealed trait Props {
    type A
    val items         : NonEmptyVector[Item[A]]
    val selected      : Option[A]
    val callbacks     : Option[Reusable[Callbacks[A]]]
    val inProgress    : Boolean
    val buttonLabel   : String
    val icon          : Icon
    val colour        : ColourPlus
    val buttonTagMod  : TagMod
    val dropdownTagMod: TagMod
    val basic         : Boolean

    implicit def univEqA: UnivEq[A]

    lazy val callbacksUnlessInProgress =
      callbacks.filterNot(_ => inProgress)

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
                 callbacks     : Option[Reusable[Callbacks[A]]],
                 inProgress    : Boolean,
                 buttonLabel   : String,
                 icon          : Icon,
                 colour        : ColourPlus = ColourPlus.Default,
                 buttonTagMod  : TagMod     = TagMod.empty,
                 dropdownTagMod: TagMod     = TagMod.empty,
                 basic         : Boolean    = false,
               )(implicit A: UnivEq[A]): Of[A] = {
      type _A             = A
      val _items          = items
      val _selected       = selected
      val _callbacks      = callbacks
      val _inProgress     = inProgress
      val _buttonLabel    = buttonLabel
      val _icon           = icon
      val _colour         = colour
      val _buttonTagMod   = buttonTagMod
      val _dropdownTagMod = dropdownTagMod
      val _basic          = basic
      new Props {
        override type A               = _A
        override val items            = _items
        override val selected         = _selected
        override val callbacks        = _callbacks
        override val inProgress       = _inProgress
        override val buttonLabel      = _buttonLabel
        override val icon             = _icon
        override val colour           = _colour
        override val buttonTagMod     = _buttonTagMod
        override val dropdownTagMod   = _dropdownTagMod
        override val basic            = _basic
        override implicit def univEqA = A
      }
    }

    def forNew[A](items       : NonEmptyVector[Item[A]],
                  selected    : Option[A],
                  callbacks   : Option[Reusable[Callbacks[A]]],
                  inProgress  : Boolean,
                  buttonTagMod: TagMod = TagMod.empty,
                  basic       : Boolean = false
                )(implicit A  : UnivEq[A]): Of[A] =
      apply(
        items          = items,
        selected       = selected,
        callbacks      = callbacks,
        inProgress     = inProgress,
        buttonLabel    = "New",
        icon           = Icon.Plus,
        colour         = Colour.Green,
        basic          = basic,
        buttonTagMod   = buttonTagMod,
        dropdownTagMod = *.dropdownButtonGreenDropdown(basic),
      )

    implicit val reusability: Reusability[Props] =
      Reusability.byRef
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {

    private val dropdownNode = Ref[html.Element]

    def render(p: Props): VdomNode = {
      import p.univEqA

      val basic = (^.cls := "basic").when(p.basic)

      def renderButton: VdomTag =
        <.a(
          ^.cls := "ui button" <+ p.colour,
          (^.cls := "loading").when(p.inProgress),
          basic,
          p.icon.tag,
          p.buttonLabel,
          ^.onClick -->? p.callbacksUnlessInProgress.map(_.click(p.selectedItem.value)))

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
        p.buttonTagMod,
        ^.cls := "ui labeled button",
        (^.cls := "disabled").when(p.callbacksUnlessInProgress.isEmpty),
        renderButton,
        renderDropdown)
    }

    private def selectItem(key: String): Callback =
      for {
        p <- $.props.toCBO
        u <- CallbackOption.liftOption(p.callbacksUnlessInProgress)
        i <- CallbackOption.liftOption(p.items.find(_.key ==* key))
        _ <- u.select(i.value).toCBO
      } yield ()

    private val dropdownOptions: Dropdown.JsOptions =
      Dropdown.JsOptions.default.withOnChange(selectItem(_).runNow())

    val enableDropdown: Callback =
      for (n <- dropdownNode) {
        JQuery(n).dropdown(dropdownOptions)

        // Remove tabindex
        // The reason I did this is that when a ButtonAndDropdown was added to ReqDetails' ReqTypeRow, it completely
        // broken the table navigation. Rather than try to be crafty, it's easier to just remove tabIndex here.
        // I'd rather just not support keyboard shortcuts for the dropdown (which was only indirectly and incidentally
        // supported anyway), and know that the explicit keyboard navigation feature that I've added will continue to
        // work whether a ButtonAndDropdown exists or not.
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
