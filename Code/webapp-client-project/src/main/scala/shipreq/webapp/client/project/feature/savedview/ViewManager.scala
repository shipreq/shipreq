package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.{ConfirmJs, PromptJs}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Icon, Menu => SemUiMenu, SemExtAny}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.Style.{savedViews => *}

object ViewManager {
  import ViewLogic._

  val devMarker = VdomAttr.elidable("data-svm")

  final case class Props(menu       : Menu,
                         asyncRW    : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         promptJs   : PromptJs,
                         confirmJs  : ConfirmJs,
                         runAction  : Action ~=> Callback,
                         savedViewIO: ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]) {
    @inline def render: VdomElement = Component(this)
  }

  /** This is really important because without this, every time an unchanged view is updated (eg. the user is typing in
    * to the filter) this view will change which will cause the Semantic UI menu to refresh which causes it to run
    * Semantic UI's shitty `$(...).dropdown` JS which VERY NOTICEABLY slows down the UX.
    */
  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

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

    private def interpretMenu(interpretMenuItem: (MenuItem, Boolean) => SemUiMenu.Item): Menu => SemUiMenu.Props =
      m => SemUiMenu.Props(
        menuStyle,
        m.items.whole.map(i => interpretMenuItem(i, m.isActive(i))),
        dropdownOptions = dropdownOptions)

    private def interpretMenuItem(runAction          : Action => Callback,
                                  asyncR             : AsyncFeature.Read.D0[Any],
                                  interpretMenuAction: MenuAction => Dropdown.Item): (MenuItem, Boolean) => SemUiMenu.Item = {

      val itemState =
        SemUiMenu.ItemState.disabledWhen(asyncR.nonEmpty)

      (mi, active) => {
        val label: TagMod =
          if (mi.default)
            TagMod(defaultIcon, mi.name.value)
          else
            mi.name.value

        val actions =
          mi.actions.whole.map(interpretMenuAction)

        SemUiMenu.DropdownType.OnHover(label, actions)
          .toItem(itemState, tagMod = *.activeItem.when(active))
          .withOnClick($.getDOMNode.map(_.toElement).asCBO, mi.optionId.map(id => runAction(Action.Select(id))).getOrEmpty)
      }
    }

    private def interpretMenuAction(runAction  : Action => Callback,
                                    confirmJs  : ConfirmJs,
                                    promptJs   : PromptJs,
                                    asyncW     : AsyncFeature.Write.D0[ErrorMsg],
                                    savedViewIO: ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]): MenuAction => Dropdown.Item = {

      def item(icon: Icon, label: String, onClick: Callback) =
        Dropdown.Item.Div(TagMod(icon.tag, label))
          .withOnClick($.getDOMNode.map(_.toElement).asCBO, onClick)

      type OnSuccess = VerifiedEvent.Seq => Callback

      def runActionOnSuccess(f: PartialFunction[Event, Action]): VerifiedEvent.Seq => Callback =
        _.lastOption.map(_.event).collect(f).map(runAction).getOrEmpty

      def runCmd(cmd: SavedViewCmd, onSuccess: OnSuccess = _ => Callback.empty): Callback =
        asyncW(savedViewIO(cmd).rightFlatTap(onSuccess(_).asAsyncCallback))

      def promptThenRun(prompt   : CallbackTo[Option[String]],
                        validate : CallbackTo[String => Either[String, Option[SavedViewCmd]]],
                        onSuccess: OnSuccess = _ => Callback.empty): CallbackOption[Unit] =
        for {
          v <- validate
          cmd <- CallbackTo.retryUntilRight(
                   prompt.map {
                     case Some(s) => v(s)
                     case None    => Right(None)
                   }
                 )(Callback.alert).asCBO
          _ <- runCmd(cmd, onSuccess).toCBO
        } yield ()

      {
        case MenuAction.SaveAsNew(cmdFn) =>
          item(Icon.Plus, "Save as new...",
            promptThenRun(
              prompt    = promptJs("Enter a name for this view"),
              validate  = cmdFn.value.map(_.andThen(_.map(Some(_)).toEither)),
              onSuccess = runActionOnSuccess { case e: Event.SavedViewCreate => Action.Select(e.id) }))

        case MenuAction.Replace(name, cmdCB) =>
          item(Icon.Save, s"Replace ${name.value}",
            cmdCB.value.flatMap(runCmd(_).toCBO))

        case MenuAction.MakeDefault(cmd) =>
          item(Icon.Star, "Set as default",
            runCmd(cmd))

        case MenuAction.Delete(name, cmd, action) =>
          item(Icon.Trash, "Delete...",
            for {
              _ <- confirmJs(s"Really delete the '${name.value}' view?").requireCBO
              _ <- runCmd(cmd, _ => runAction(action)).toCBO
            } yield ())

        case MenuAction.Rename(name, cmdFn) =>
          item(Icon.Write, "Rename...",
            promptThenRun(
              prompt   = promptJs(s"Enter a new name for ${name.value}", name.value),
              validate = CallbackTo.pure(cmdFn(_).toDisjOption.toEither)))
      }
    }

    def render(p: Props): VdomElement = {
      val i = interpretMenu(
        interpretMenuItem(p.runAction, p.asyncRW.read,
          interpretMenuAction(p.runAction, p.confirmJs, p.promptJs, p.asyncRW.write, p.savedViewIO)))
      val semUiMenu = i(p.menu)
      <.div(
        devMarker := 1,
        semUiMenu.render)
    }

    private def onFailure(confirmJs: ConfirmJs, f: AsyncFeature.Status.Failed[ErrorMsg]): Callback =
      confirmJs(s"Failed to update saved views:\n${f.failure.value}\n\nRetry?")
        .flatMap {
          case true  => f.retry
          case false => f.cancel
        }

    val handleAsyncError: Callback =
      $.props.flatMap(p =>
        p.asyncRW.read.collect { case f: AsyncFeature.Status.Failed[ErrorMsg] => onFailure(p.confirmJs, f) }
          .getOrEmpty)
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidUpdate(_.backend.handleAsyncError)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
