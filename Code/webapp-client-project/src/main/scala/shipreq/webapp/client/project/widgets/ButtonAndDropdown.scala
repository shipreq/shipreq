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
    final type Click         = ButtonsAndDropdown.Click[A]

    @inline final def Item = ButtonAndDropdown.Item

    @inline final def Callbacks(click: Click => Callback, select: A => Callback): Callbacks =
      ButtonAndDropdown.Callbacks(click = click, select = select)
  }

  type Props     = ButtonsAndDropdown.Props
  type Click[+A] = ButtonsAndDropdown.Click[A]
  val  Click     = ButtonsAndDropdown.Click
  type Item[+A]  = ButtonsAndDropdown.Item[A]
  val  Item      = ButtonsAndDropdown.Item
  val  Component = ButtonsAndDropdown.Component

  final case class Callbacks[-A](click: Click[A] => Callback, select: A => Callback)

  object Props {
    import ButtonsAndDropdown.ButtonProps

    type Of[A] = ButtonsAndDropdown.Props.Of[A]

    def newReq[A](items       : NonEmptyVector[Item[A]],
                  selectItem  : Option[Reusable[A => Callback]],
                  selected    : Option[A],
                  create      : Option[Reusable[Click[A] => Callback]],
                  inProgress  : Boolean,
                  outerTagMod : TagMod = TagMod.empty,
                  basic       : Boolean = false
                )(implicit A  : UnivEq[A]): Of[A] = {

      val button = ButtonProps.newReq(create, inProgress)

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
