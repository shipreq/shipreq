package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.event.{Event, SavedViewCreate, VerifiedEvent}
import shipreq.webapp.base.protocol.{SavedViewCmd, ServerSideProcInvoker}
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Icon, Menu => SemUiMenu, SemExtAny}
import shipreq.webapp.client.project.app.Style.reqtable.{savedViews => *}
import SavedViewLogic._

object SavedViewsUI {

  final case class Props(menu       : Menu,
                         runAction  : Action ~=> Callback,
                         savedViewIO: ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]) {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.caseClass

  private val menuStyle =
    SemUiMenu.Style(
      SemUiMenu.Attr.Secondary + SemUiMenu.Attr.Pointing,
      tagMod = *.menu)

  private val dropdownOptions =
    Dropdown.JsOptions.default
      .onHover
      .withDelay(show = 500)

  private val defaultIcon =
    Icon.Star.withColour(Colour.Yellow).tag

  final class Backend($: BackendScope[Props, Unit]) {

    private def interpretMenu(interpretMenuItem: (MenuItem, Boolean) => SemUiMenu.Item)
                             (m: Menu): SemUiMenu.Props =
      SemUiMenu.Props(
        menuStyle,
        m.items.whole.map(i => interpretMenuItem(i, m.isActive(i))),
        dropdownOptions = dropdownOptions)

    private def interpretMenuItem(runAction          : Action => Callback,
                                  interpretMenuAction: MenuAction => Dropdown.Item)
                                 (mi                 : MenuItem,
                                  active             : Boolean): SemUiMenu.Item = {
      val label: TagMod =
        if (mi.default)
          TagMod(defaultIcon, mi.name.value)
        else
          mi.name.value

      val actions =
        mi.actions.whole.map(interpretMenuAction)

      SemUiMenu.DropdownType.OnHover(label, actions)
        .toItem(tagMod = *.activeItem.when(active))
        .withOnClick($.getDOMNode, mi.optionId.map(id => runAction(Action.Select(id))).getOrEmpty)
    }

    private def interpretMenuAction(runAction  : Action => Callback,
                                    savedViewIO: ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]): MenuAction => Dropdown.Item = {

      def item(icon: Icon, label: String, onClick: Callback) =
        Dropdown.Item.Div(TagMod(icon.tag, label))
          .withOnClick($.getDOMNode, onClick)

      type OnSuccess = VerifiedEvent.Seq => Callback

      def runActionOnSuccess(f: PartialFunction[Event, Action]): VerifiedEvent.Seq => Callback =
        _.eventVector.lastOption.map(_.event).collect(f).map(runAction).getOrEmpty

      def onFailure: ErrorMsg => Callback =
        e => Callback.alert("Failed to update saved views.\n\n" + e.value)

      def runCmd(cmd: SavedViewCmd, onSuccess: OnSuccess = _ => Callback.empty): Callback =
        savedViewIO(cmd, onSuccess, onFailure)

      def promptThenRun(prompt   : CallbackTo[Option[String]],
                        validate : String => Either[String, Option[SavedViewCmd]],
                        onSuccess: OnSuccess = _ => Callback.empty): CallbackOption[Unit] =
        for {
          cmd <- CallbackTo.retryUntilRight(
                   prompt.map {
                     case Some(s) => validate(s)
                     case None    => Right(None)
                   }
                 )(Callback.alert).asCBO
          _ <- runCmd(cmd, onSuccess).toCBO
        } yield ()

      {
        case MenuAction.SaveAsNew(cmdFn) =>
          item(Icon.Plus, "Save as new...",
            promptThenRun(
              prompt    = CallbackTo.prompt("Enter a name for this view"),
              validate  = cmdFn(_).map(Some(_)).toEither,
              onSuccess = runActionOnSuccess { case e: SavedViewCreate => Action.Select(e.id) }))

        case MenuAction.Replace(name, cmd) =>
          item(Icon.Save, s"Replace ${name.value}",
            runCmd(cmd))

        case MenuAction.MakeDefault(cmd) =>
          item(Icon.Star, "Set as default",
            runCmd(cmd))

        case MenuAction.Delete(name, cmd, action) =>
          item(Icon.Trash, "Delete...",
            for {
              _ <- CallbackTo.confirm(s"Really delete the '${name.value}' view?").requireCBO
              _ <- runCmd(cmd, _ => runAction(action)).toCBO
            } yield ())

        case MenuAction.Rename(name, cmdFn) =>
          item(Icon.Write, "Rename...",
            promptThenRun(
              prompt   = CallbackTo.prompt(s"Enter a new name for ${name.value}", name.value),
              validate = cmdFn(_).toDisjOption.toEither))
      }
    }

    def render(p: Props): VdomElement = {
      interpretMenu(
        interpretMenuItem(p.runAction,
          interpretMenuAction(p.runAction, p.savedViewIO)))(p.menu).render
    }
  }

  val Component = ScalaComponent.builder[Props]("SavedViewsUI")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate) TODO
    .build
}
