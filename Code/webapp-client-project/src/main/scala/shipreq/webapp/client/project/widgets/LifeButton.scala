package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Dead, Live}
import shipreq.webapp.base.ui.semantic.{Button, Colour, ColourPlus, Icon}

/**
  * Buttons to change life.
  *
  * Live. [Delete]
  * Dead. [Restore]
  */
sealed abstract class LifeButton(status: String, button: Button) {
  private val base = button.tag(^.whiteSpace.nowrap)

  def apply(onClick: Callback): VdomTag =
    base(^.onClick --> onClick)

  val justStatus = <.span(status + ".")

  def withStatusOnLeft(onClick: Callback): TagMod =
    TagMod(
      justStatus,
      apply(onClick)(^.float.right))

  def withStatusOnLeft(onClick: Option[Callback]): TagMod =
    onClick.fold[TagMod](justStatus)(withStatusOnLeft)
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
