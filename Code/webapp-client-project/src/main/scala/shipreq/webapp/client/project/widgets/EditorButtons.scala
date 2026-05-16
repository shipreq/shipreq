package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Enabled, PotentialChange}
import shipreq.webapp.base.ui.semantic.{Button, Colour, ColourPlus, Icon}
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.member.project.data.Project

/** The row of buttons underneath editors.
  *
  * Eg. | [Delete]                      [ Cancel ] [ Update ] |
  */
object EditorButtons {

  def createOrUpdate[N, Id, Id2 <: Id, S, Cmd](args            : SplitScreenCrud.EditorArgs[N, Id, S])
                                              (idOption        : Option[Id2],
                                               potentialSaveCmd: PotentialChange[Any, Cmd])
                                              (submitCmd       : (Cmd, String, (Project, Id2) => Callback) => Callback,
                                               deleteCmd       : Id2 => Cmd,
                                               enabled         : Enabled): Props =
    idOption match {
      case Some(id) =>
        Props.Update(
          abort   = args.close,
          delete  = submitCmd(deleteCmd(id), "Deleted", (p, _) => args.reset(p)),
          update  = potentialSaveCmd.map(submitCmd(_, "Updated", (p, _) => args.reset(p))),
          enabled = enabled,
        )

      case None =>
        Props.Create(
          abort   = args.close,
          create  = potentialSaveCmd.toOption.map(submitCmd(_, "Created", args.selectP)),
          enabled = enabled,
        )
    }

  def createOrHardUpdate[N, Id, Id2 <: Id, S, Cmd](args             : SplitScreenCrud.EditorArgs[N, Id, S])
                                                  (idOption         : Option[Id2],
                                                   potentialSaveCmd : PotentialChange[Any, Cmd])
                                                  (submitCmd        : (Cmd, String, (Project, Id2) => Callback) => Callback,
                                                   hardDeleteConfirm: CallbackTo[Boolean],
                                                   hardDeleteCmd    : Id2 => Cmd,
                                                   softDeleteCmd    : Id2 => Cmd,
                                                   enabled          : Enabled): Props =
    idOption match {
      case Some(id) =>
        Props.HardUpdate(
          abort      = args.close,
          hardDelete = hardDeleteConfirm.flatMap(submitCmd(hardDeleteCmd(id), "Permanently deleted", (_, _) => args.close).when_(_)),
          softDelete = submitCmd(softDeleteCmd(id), "Deleted", (p, _) => args.reset(p)),
          update     = potentialSaveCmd.map(submitCmd(_, "Updated", (p, _) => args.reset(p))),
          enabled    = enabled,
        )

      case None =>
        Props.Create(
          abort   = args.close,
          create  = potentialSaveCmd.toOption.map(submitCmd(_, "Created", args.selectP)),
          enabled = enabled,
        )
    }

  def cancel[N, Id, S, Cmd](args: SplitScreenCrud.EditorArgs[N, Id, S]): Props =
    Props.Cancel(args.close)

  def close[N, Id, S, Cmd](args: SplitScreenCrud.EditorArgs[N, Id, S]): Props =
    Props.Close(args.close)

  def restore[N, Id, S, Cmd](args     : SplitScreenCrud.EditorArgs[N, Id, S])
                            (submitCmd: (String, (Project, Id) => Callback) => Callback,
                             enabled  : Enabled): Props =
    Props.Restore(
      abort   = args.close,
      restore = submitCmd("Restored", (p, _) => args.reset(p)),
      enabled = enabled,
    )

  def add[N, Id, S, Cmd](args     : SplitScreenCrud.EditorArgs[N, Id, S])
                        (submitCmd: (String, (Project, Id) => Callback) => Callback,
                         enabled  : Enabled): Props =
    Props.Add(
      abort   = args.close,
      add     = submitCmd("Added", (p, _) => args.reset(p)),
      enabled = enabled,
    )

  def remove[N, Id, S, Cmd](args     : SplitScreenCrud.EditorArgs[N, Id, S])
                           (submitCmd: (String, (Project, Id) => Callback) => Callback,
                            enabled  : Enabled): Props =
    Props.Remove(
      abort   = args.close,
      remove  = submitCmd("Removed", (p, _) => args.reset(p)),
      enabled = enabled,
    )

  sealed trait Props {
    @inline final def render: VdomElement = Component(this)
  }

  object Props {

    final case class Add(abort  : Callback,
                         add    : Callback,
                         enabled: Enabled) extends Props

    final case class Cancel(abort: Callback) extends Props

    final case class Close(abort: Callback) extends Props

    final case class Create(abort  : Callback,
                            create : Option[Callback],
                            enabled: Enabled) extends Props

    final case class Remove(abort  : Callback,
                            remove : Callback,
                            enabled: Enabled) extends Props

    final case class Restore(abort  : Callback,
                             restore: Callback,
                             enabled: Enabled) extends Props

    final case class Update(abort  : Callback,
                            delete : Callback,
                            update : PotentialChange[Any, Callback],
                            enabled: Enabled) extends Props

    final case class HardUpdate(abort     : Callback,
                                hardDelete: Callback,
                                softDelete: Callback,
                                update    : PotentialChange[Any, Callback],
                                enabled   : Enabled) extends Props

  }

  private val outer  = <.div(*.editorButtons)
  private val gap    = <.div(*.editorButtonGap)

  private val addButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Plus, "Add"),
      colour = Colour.Green)

  private val cancelButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Remove, "Cancel"),
      colour = Colour.Black)

  private val closeButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Remove, "Close"),
      colour = Colour.Black)

  private val createButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Plus, "Create"),
      colour = Colour.Green)

  private val deleteButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Trash, "Delete"),
      colour = ColourPlus.Negative)

  private val hardDeleteButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Trash, "Permanently Delete"),
      colour = ColourPlus.Negative)

  private val removeButton =
    Button(
      tipe   = Button.Type.BasicIconAndText(Icon.Trash, "Remove"),
      colour = ColourPlus.Negative)

  private val restoreButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Undo, "Restore"),
      colour = Colour.Green)

  private val updateButton =
    Button(
      tipe   = Button.Type.IconAndText(Icon.Plus, "Update"),
      colour = Colour.Green)

  private def render(p: Props): VdomNode =
    p match {

      case Props.Cancel(abort) =>
        outer(
          gap,
          cancelButton.onClick(abort))

      case Props.Close(abort) =>
        outer(
          gap,
          closeButton.onClick(abort))

      case Props.Create(abort, create, enabled) =>
        outer(
          gap,
          cancelButton.onClick(abort),
          createButton.disableMaybe(enabled).onClickWhenDefined(create))

      case Props.Update(abort, delete, PotentialChange.Unchanged, enabled) =>
        outer(
          deleteButton.disableMaybe(enabled).onClick(delete),
          gap,
          closeButton.onClick(abort),
          updateButton.disabled)

      case Props.Update(abort, delete, u: PotentialChange.Changed[Any, Callback], enabled) =>
        outer(
          deleteButton.disableMaybe(enabled).onClick(delete),
          gap,
          cancelButton.onClick(abort),
          updateButton.disableMaybe(enabled).onClickWhenDefined(u.getUpdate))

      case p@ Props.HardUpdate(_, _, _, PotentialChange.Unchanged, _) =>
        import p._
        outer(
          hardDeleteButton.disableMaybe(enabled).onClick(hardDelete),
          deleteButton.disableMaybe(enabled).onClick(softDelete),
          gap,
          closeButton.onClick(abort),
          updateButton.disabled)

      case p@ Props.HardUpdate(_, _, _, u: PotentialChange.Changed[Any, Callback], _) =>
        import p._
        outer(
          hardDeleteButton.disableMaybe(enabled).onClick(hardDelete),
          deleteButton.disableMaybe(enabled).onClick(softDelete),
          gap,
          cancelButton.onClick(abort),
          updateButton.disableMaybe(enabled).onClickWhenDefined(u.getUpdate))

      case Props.Restore(abort, restore, enabled) =>
        outer(
          restoreButton.disableMaybe(enabled).onClick(restore),
          gap,
          closeButton.onClick(abort))

      case Props.Add(abort, add, enabled) =>
        outer(
          gap,
          closeButton.onClick(abort),
          addButton.disableMaybe(enabled).onClick(add))

      case Props.Remove(abort, remove, enabled) =>
        outer(
          removeButton.disableMaybe(enabled).onClick(remove),
          gap,
          closeButton.onClick(abort))
    }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .build
}
