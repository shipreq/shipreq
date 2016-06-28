package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Dead, Live}
import shipreq.webapp.client.base.ui.semantic.{Button, Colour, ColourPlus, Icon}

/**
  * Buttons to change life.
  *
  * Live. [Delete]
  * Dead. [Restore]
  */
sealed abstract class LifeButton(status: String, button: Button) {
  private val base = button.tag

  def apply(onClick: Callback): ReactTag =
    base(^.onClick --> onClick)

  val justStatus = <.span(status + ".")

  def withStatusOnLeft(onClick: Callback): TagMod =
    TagMod(
      justStatus(^.marginRight := "1em"),
      apply(onClick))

  def withStatusOnLeft(onClick: Option[Callback]): TagMod =
    onClick.fold[TagMod](justStatus)(withStatusOnLeft(_))
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
