package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Disabled, Enabled, ErrorMsg, PotentialChange}
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon}
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.Style.{issueConfig => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.{EditorButtons, ProjectWidgets, SplitScreenCrud}

object IssueConfig {

  // Unit because there's only one type of data creatable: CustomIssueType.
  // New button is just a simple button without a dropdown.
  type NewState = Unit

  type EditorState = CustomIssueTypeEditor.State

  val splitScreenCrud = new SplitScreenCrud[NewState, CustomIssueTypeId, EditorState]

  final case class Props(project: Project,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyCustomIssueTypes, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         router : Routes.RouterCtl,
                         toast  : Toast,
                         usage  : Usage,
                        ) {

    val asyncInProgress: Boolean =
      async.isInProgress

    val filterDeadOverride: Option[FilterDead] =
      state.value.filterDead.overrideIfDeadOption(
        state.value.right.idOption.flatMap(project.config.customIssueTypes.get).map(_.live))

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyCustomIssueTypes] =
      state.value.right.editorOption match {
        case Some(s) => s.updateCmd(project.config)
        case None    => PotentialChange.Unchanged
      }

    @inline def render: VdomElement = Component(this)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(())

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait EditorType
  object EditorType {
    final case class Dead  (issueType: CustomIssueType)         extends EditorType
    final case class Editor(issueType: Option[CustomIssueType]) extends EditorType
  }

  private val newButton =
    Button(
      tipe = Button.Type.IconAndText(Icon.Plus, "New"),
      colour = Colour.Green,
    )

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private def initEditor(project: Project, arg: NewState \/ CustomIssueTypeId): Option[EditorState] =
      arg match {
        case \/-(id) => CustomIssueTypeEditor.State.initById(id, project.config.customIssueTypes)
        case -\/(()) => Some(CustomIssueTypeEditor.State.initNew)
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyCustomIssueTypes,
                          toastPrefix: String,
                          onSuccess  : (Project, CustomIssueTypeId) => Callback): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.customIssueTypes.all.headOption)(id =>
              for {
                p2 <- $.props
                io  = p2.project.config.customIssueTypes.get(id).orElse(p.project.config.customIssueTypes.get(id))
                _  <- Callback.traverseOption(io)(i => p2.toast.add(s"$toastPrefix ${i.key.with_#}"))
                _  <- onSuccess(n.project, id)
              } yield ()
            ).asAsyncCallback

          case -\/(e) =>
            GeneralTheme.showErrorMsg(e).asAsyncCallback
        }
      )

    private val leftHeader =
      <.div(*.sectionTitle, "User-Defined Issue Types")

    private def renderNewButton(p: Props, args: splitScreenCrud.NewArgs): VdomNode =
      args match {

        case a: NewArgs.Enabled[NewState] =>
          newButton.disableMaybe(Disabled when p.asyncInProgress).onClick(a.openEditor)

        case NewArgs.Disabled(()) =>
          newButton.disabled
      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      CustomIssueTypeList.Props(
        customIssueTypes   = p.project.config.customIssueTypes,
        filterDead         = p.effectiveFilterDead,
        selected           = args.selection,
        select             = args.enabledSelect,
        pw                 = p.pw,
        enabled            = Disabled when p.asyncInProgress,
        onClickAnywhere    = args.closeEditor.filter(_ => p.potentialSaveCmd.isUnchanged),
        usage              = p.usage,
      ).render

    private def renderRightEmpty(p: Props): VdomNode =
      OtherIssueSources.Props(
        config = p.project.config,
        pw     = p.pw,
        router = p.router,
      ).render

    private def renderHeader(p: Props, args: splitScreenCrud.EditorArgs): VdomNode =
      <.h2(*.editorTitle,
        args.id match {
          case \/-(id) =>
            val i = p.project.config.customIssueTypes.need(id)
            i.key.with_#

          case -\/(()) =>
            "New issue type"
        })

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val header: VdomNode =
        renderHeader(p, args)

      val editorType: EditorType =
        args.id match {
          case -\/(()) => EditorType.Editor(None)
          case \/-(id) =>
            val i = p.project.config.customIssueTypes.need(id)
            if (i.live.is(Dead))
              EditorType.Dead(i)
            else
              EditorType.Editor(Some(i))
        }

      def createOrUpdateButtons(idOption: Option[CustomIssueTypeId]): EditorButtons.Props =
        EditorButtons.createOrUpdate(args)(idOption, p.potentialSaveCmd)(
          submitCmd = submitCmd(p, _, _, _),
          deleteCmd = UpdateConfigCmd.CustomIssueTypeDelete)

      def customIssueTypeEditor(enabled: Enabled) =
        CustomIssueTypeEditor.Props(
          state   = args.state,
          project = p.project.config,
          enabled = enabled,
        ).render

      editorType match {

        case EditorType.Editor(itOption) =>
          val editor  = customIssueTypeEditor(Enabled)
          val buttons = createOrUpdateButtons(itOption.map(_.id)).render
          <.div(header, editor, buttons)

        case EditorType.Dead(i) =>
          val editor = customIssueTypeEditor(Disabled)
          val buttons = EditorButtons.restore(args)(submitCmd(p, UpdateConfigCmd.CustomIssueTypeRestore(i.id), _, _)).render
          <.div(header, editor, buttons)
      }
    }

    def render(p: Props): VdomNode = {
       // println(("="*60) + "\n" + p.state.value + "\n")

      splitScreenCrud(
        filterDeadOverride = p.filterDeadOverride,
        project            = p.project,
        newButton          = renderNewButton(p, _),
        list               = renderLeft(p, _),
        leftTop            = leftHeader,
        rightEmpty         = renderRightEmpty(p),
        editor             = renderEditor(p, _),
        initEditor         = initEditor,
        state              = p.state,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}