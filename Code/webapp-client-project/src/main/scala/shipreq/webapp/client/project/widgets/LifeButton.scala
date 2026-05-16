package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.Enabled
import shipreq.webapp.base.ui.semantic.{Button, Colour, ColourPlus, Icon}
import shipreq.webapp.member.UiText
import shipreq.webapp.member.project.data.{Dead, Live}

/**
  * Buttons to change life.
  *
  * Live. [Delete]
  * Dead. [Restore]
  */
sealed abstract class LifeButton(status: String, button: Button) {

  def apply(onClick: Callback, enabled: Enabled = Enabled): VdomTag =
    button.disableMaybe(enabled).onClick(onClick)(^.whiteSpace.nowrap)

  val justStatus = <.span(status + ".")

  def withStatusOnLeft(onClick: Callback, enabled: Enabled): TagMod =
    TagMod(
      justStatus,
      apply(onClick, enabled)(^.float.right))
}

object LifeButton {

  object Delete extends LifeButton(
    UiText.Life.live,
    Button(
      tipe = Button.Type.BasicIconAndText(Icon.Trash, UiText.Life.delete),
      colour = ColourPlus.Negative))

  object Restore extends LifeButton(
    UiText.Life.dead,
    Button(
      tipe = Button.Type.IconAndText(Icon.Undo, UiText.Life.restore),
      colour = Colour.Green))

  def byCurrentStatus(current: Live): LifeButton =
    current match {
      case Live => Delete
      case Dead => Restore
    }
}
