package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
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
    final type DropdownValue = A
    final type Item          = ButtonAndDropdown.Item[A]
    final type Callbacks     = ButtonAndDropdown.Callbacks[A]
    final type DBProps       = ButtonAndDropdown.Props.Of[A]

    @inline final def Item = ButtonAndDropdown.Item

    @inline final def Callbacks(click: A => Callback, select: A => Callback): Callbacks =
      ButtonAndDropdown.Callbacks(click = click, select = select)
  }

  type Props     = ButtonsAndDropdown.Props
  type Item[+A]  = ButtonsAndDropdown.Item[A]
  val  Item      = ButtonsAndDropdown.Item
  val  Component = ButtonsAndDropdown.Component

  final case class Callbacks[-A](click: A => Callback, select: A => Callback)

  object Props {
    import ButtonsAndDropdown.ButtonProps

    type Of[A] = ButtonsAndDropdown.Props.Of[A]

    def newReq[A](items       : NonEmptyVector[Item[A]],
                  selectItem  : Option[Reusable[A => Callback]],
                  selected    : Option[A],
                  create      : Option[Reusable[A => Callback]],
                  inProgress  : Boolean,
                  outerTagMod : TagMod = TagMod.empty,
                  basic       : Boolean = false
                )(implicit A  : UnivEq[A]): Of[A] = {

      val button = ButtonProps[A](
        colour     = Colour.Green,
        label      = "New",
        icon       = Icon.Plus,
        callback   = create,
        inProgress = inProgress,
      )

      ButtonsAndDropdown.Props(
        buttons        = NonEmptyVector.one(button),
        items          = items,
        selectItem     = selectItem,
        selected       = selected,
        outerTagMod    = outerTagMod,
        dropdownTagMod = *.dropdownButtonGreenDropdown(basic),
        basic          = basic,
      )
    }
  }
}
