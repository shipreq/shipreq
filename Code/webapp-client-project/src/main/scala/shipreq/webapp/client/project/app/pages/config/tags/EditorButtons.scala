package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.PotentialChange
import shipreq.webapp.base.ui.semantic.{Button, Colour, ColourPlus, Icon}
import shipreq.webapp.client.project.app.Style.{tagConfig => *}

/** The row of buttons underneath editors.
  *
  * Eg. | [Delete]                      [ Cancel ] [ Update ] |
  */
object EditorButtons {

  sealed trait Props {
    @inline final def render: VdomElement = Component(this)
  }

  object Props {

    final case class Create(abort : Callback,
                            create: Option[Callback]) extends Props

    final case class Update(abort : Callback,
                            delete: Callback,
                            update: PotentialChange[Any, Callback]) extends Props

    final case class Restore(abort  : Callback,
                             restore: Callback) extends Props
  }

  private val outer  = <.div(*.editorButtons)
  private val gap    = <.div(*.editorButtonGap)

  private val cancelButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Remove, "Cancel"),
      colour = Colour.Black)

  private val closeButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Remove, "Close"),
      colour = Colour.Black)

  private val restoreButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Undo, "Restore"),
      colour = Colour.Green)

  private val updateButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Plus, "Update"),
      colour = Colour.Green)

  private val createButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Plus, "Create"),
      colour = Colour.Green)

  private val deleteButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Trash, "Delete"),
      colour = ColourPlus.Negative)

  private def render(p: Props): VdomNode =
    p match {
      case Props.Create(abort, create) =>
        outer(
          gap,
          cancelButton.onClick(abort),
          createButton.onClickWhenDefined(create))

      case Props.Update(abort, delete, PotentialChange.Unchanged) =>
        outer(
          deleteButton.onClick(delete),
          gap,
          closeButton.onClick(abort),
          updateButton.disabled)

      case Props.Update(abort, delete, u: PotentialChange.Changed[Any, Callback]) =>
        outer(
          deleteButton.onClick(delete),
          gap,
          cancelButton.onClick(abort),
          updateButton.onClickWhenDefined(u.getUpdate))

      case Props.Restore(abort, restore) =>
        outer(
          restoreButton.onClick(restore),
          gap,
          closeButton.onClick(abort))
    }

  val Component = ScalaComponent.builder[Props]("EditorButtons")
    .render_P(render)
    .build
}