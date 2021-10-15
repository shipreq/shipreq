package shipreq.webapp.base.ui.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.ui.semantic
import shipreq.webapp.base.ui.semantic.{Icon, UsesSemanticUiManually}
import shipreq.webapp.base.util.ReactKeyGen

@UsesSemanticUiManually
object Dropdown {

  type ItemKey = String

  final case class Item[+A](key: ItemKey, label: VdomNode, value: A) {
    def map[B](f: A => B): Item[B] =
      Item(key, label, f(value))
  }

  sealed trait Props {
    type A
    val enabled : Enabled
    val tagMod  : TagMod
    val validity: Validity

    @inline final def render: VdomElement =
      Component(this)
  }

  object Props {

    sealed trait Optional extends Props {
      val items   : ArraySeq[Item[A]]
      val selected: Option[ItemKey]
      val onChange: Item[A] => Callback
    }

    def Optional[A](items   : ArraySeq[Item[A]],
                    selected: Option[ItemKey] = None,
                    enabled : Enabled           = Enabled,
                    tagMod  : TagMod            = EmptyVdom,
                    validity: Validity          = Valid)
                   (onChange: Item[A] => Callback): Optional = {
      type _A       = A
      val _items    = items
      val _selected = selected
      val _onChange = onChange
      val _enabled  = enabled
      val _tagMod   = tagMod
      val _validity = validity
      new Optional {
        override type A       = _A
        override val items    = _items
        override val selected = _selected
        override val onChange = _onChange
        override val enabled  = _enabled
        override val tagMod   = _tagMod
        override val validity = _validity
      }
    }

    sealed trait NonEmpty extends Props {
      val items   : NonEmptyArraySeq[Item[A]]
      val selected: ItemKey
      val onChange: Item[A] => Callback
    }

    def NonEmpty[A](items   : NonEmptyArraySeq[Item[A]],
                    selected: ItemKey,
                    enabled : Enabled  = Enabled,
                    tagMod  : TagMod   = EmptyVdom,
                    validity: Validity = Valid)
                   (onChange: Item[A] => Callback): NonEmpty = {
      type _A       = A
      val _items    = items
      val _selected = selected
      val _onChange = onChange
      val _enabled  = enabled
      val _tagMod   = tagMod
      val _validity = validity
      new NonEmpty {
        override type A       = _A
        override val items    = _items
        override val selected = _selected
        override val onChange = _onChange
        override val enabled  = _enabled
        override val tagMod   = _tagMod
        override val validity = _validity
      }
    }
  }

  // ===================================================================================================================

  private[this] val outer       = <.div(^.cls := "ui selection dropdown")
  private[this] val deftxt      = <.div(^.cls := "default text")
  private[this] val item        = <.div(^.cls := "item")
  private[this] val menu        = <.div(^.cls := "menu")
  private[this] val clsDisabled = ^.cls := "disabled"
  private[this] val clsError    = ^.cls := "error"

  final class Backend($: BackendScope[Props, Unit]) {

    val enableDropdown: Callback =
      semantic.Dropdown.enable($.getDOMNode)

    private val keyGen = new ReactKeyGen

    def render(p: Props): VdomNode = {

      val items = {
        def renderItems[A](items: Iterator[Item[A]], onChange: Item[A] => Callback) =
          items.toVdomArray(i =>
            item(
              ^.key := i.key,
              semantic.Dropdown.itemValue := i.key,
              ^.onClick --> Callback.suspend(onChange(i)),
              i.label))

        p match {
          case x: Props.Optional => renderItems(x.items.iterator, x.onChange)
          case x: Props.NonEmpty => renderItems(x.items.iterator, x.onChange)
        }
      }

      val selected: String =
        p match {
          case x: Props.Optional => x.selected.getOrElse("")
          case x: Props.NonEmpty => x.selected
        }

      outer(
        ^.key := keyGen.next(), // Forces DOM replacement - otherwise it retains Semantic UI JS's modifications
        p.tagMod,
        clsDisabled.when(p.enabled is Disabled),
        clsError.when(p.validity is Invalid),
        <.input.hidden(^.value := selected),
        Icon.Dropdown.tag,
        deftxt,
        menu(items))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDropdown)
    .componentDidUpdate(_.backend.enableDropdown)
    .build
}
