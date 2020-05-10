package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, Optics, PotentialChange}
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Message}
import shipreq.webapp.base.ui.{GeneralTheme, Toast}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.app.Style.{reqTypeConfig => *}
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.{EditorButtons, ProjectWidgets, SplitScreenCrud}

object ReqTypeConfig {

  // Unit because there's only one type of data creatable: ReqType.
  // New button is just a simple button without a dropdown.
  type NewState = Unit

  sealed trait EditorState
  object EditorState {
    final case class Custom(state: CustomReqTypeEditor.State) extends EditorState
    case object Static                                        extends EditorState
  }

  val splitScreenCrud = new SplitScreenCrud[NewState, ReqTypeId, EditorState]

  final case class Props(project: Project,
                         state  : StateSnapshot[State],
                         pw     : ProjectWidgets.NoCtx,
                         ssp    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyReqTypes, ErrorMsg, NewEvents],
                         async  : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         confirm: ConfirmJs,
                         toast  : Toast,
                         usage  : Usage,
                        ) {

    val asyncInProgress: Boolean =
      AsyncFeature.isInProgress(async.read)

    val filterDeadOverride: Option[FilterDead] =
      state.value.filterDead.overrideIfDeadOption(
        state.value.right.idOption.flatMap(project.config.reqTypes.get).map(_.live))

    def effectiveFilterDead: FilterDead =
      filterDeadOverride.getOrElse(state.value.filterDead)

    val potentialSaveCmd: PotentialChange[Unit, UpdateConfigCmd.ToModifyReqTypes] =
      state.value.right.editorOption match {
        case Some(EditorState.Custom(s)) => s.updateCmd(project.config)
        case Some(EditorState.Static)
           | None                        => PotentialChange.Unchanged
      }

    @inline def render: VdomElement = Component(this)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(())

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait EditorType
  object EditorType {
    final case class Dead  (rt: CustomReqType)         extends EditorType
    final case class Custom(rt: Option[CustomReqType]) extends EditorType
    final case class Static(id: StaticReqType)         extends EditorType
    case object ReqTypeDeleted                         extends EditorType
  }

  private def editorStateLensForCustom(default: => CustomReqTypeEditor.State): Lens[EditorState, CustomReqTypeEditor.State] =
    Optics.coproductLens[EditorState, CustomReqTypeEditor.State]({ case EditorState.Custom(s) => s }, s => EditorState.Custom(s), default)

  private val rightEmpty =
    SplitScreenCrud.emptyEditorMessage("req type")

  private val newButton =
    Button(
      tipe = Button.Type.IconAndText(Icon.Plus, "New"),
      colour = Colour.Green,
    )

  private val notInUseNotice: VdomElement = {
    val body =
      <.div(
        *.notInUseBody,
        "This req type has never been used in any requirements (including deleted). This leads to some unique abilities:",
        <.ol(
          <.li("you can change the mnemonic without consuming it"),
          <.li("you can permanently delete this such that even when showing deleted content, you won't see this again"),
        ),
        "Once you create a requirement with this req type, even if you delete it or change it's req type, this req type is considered in-use. When in-use:",
        <.ol(
          <.li("the mnemonic is forever attributed to this req type; you can't reuse it"),
          <.li("you can only \"soft delete\" this, meaning it remains accessible when showing deleted content"),
        ),
      )

    VdomElement.static(
      Message.withBody(
        style  = Message.Style(Message.Type.Info),
        icon   = Icon.InfoCircle,
        header = "Not yet in-use",
        body   = body,
      )(*.notInUse)
    )
  }

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.NewArgs

    private def initEditor(project: Project, arg: NewState \/ ReqTypeId): Option[EditorState] =
      arg match {
        case \/-(id: CustomReqTypeId) => CustomReqTypeEditor.State.initById(id, project.config.reqTypes).map(EditorState.Custom)
        case \/-(_: StaticReqType)    => Some(EditorState.Static)
        case -\/(())                  => Some(EditorState.Custom(CustomReqTypeEditor.State.initNew))
      }

    private def submitCmd(p          : Props,
                          cmd        : UpdateConfigCmd.ToModifyReqTypes,
                          toastPrefix: String,
                          onSuccess  : (Project, CustomReqTypeId) => Callback): Callback =
      p.async.write.forgetFailure(
        p.ssp(cmd).flatTap {
          case \/-(n) =>
            Callback.traverseOption(n.summary.customReqTypes.all.headOption)(id =>
              for {
                p2 <- $.props
                rto = p2.project.config.reqTypes.get(id).orElse(p.project.config.reqTypes.get(id))
                _  <- Callback.traverseOption(rto)(rt => p2.toast.add(s"$toastPrefix ${rt.mnemonic.value}"))
                _  <- onSuccess(n.project, id)
              } yield ()
            ).asAsyncCallback

          case -\/(e) =>
            GeneralTheme.showErrorMsg(e).asAsyncCallback
        }
      )

    private def renderNewButton(p: Props, args: splitScreenCrud.NewArgs): VdomNode =
      args match {

        case a: NewArgs.Enabled[NewState] =>
          newButton.disableMaybe(Disabled when p.asyncInProgress).onClick(a.openEditor)

        case NewArgs.Disabled(()) =>
          newButton.disabled
      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      ReqTypeList.Props(
        reqTypes           = p.project.config.reqTypes,
        filterDead         = p.effectiveFilterDead,
        selected           = args.selection,
        select             = args.enabledSelect,
        pw                 = p.pw,
        enabled            = Disabled when p.asyncInProgress,
        onClickAnywhere    = args.closeEditor.filter(_ => p.potentialSaveCmd.isUnchanged),
        usage              = p.usage,
      ).render

    private def renderHeader(p: Props, args: splitScreenCrud.EditorArgs): VdomNode =
      <.h2(*.editorTitle,
        args.id match {
          case \/-(id) =>
            val rt = p.project.config.reqTypes.get(id)
            rt.whenDefined(_ => p.pw.reqTypeFull(id))

          case -\/(()) =>
            "New req type"
        })

    private def renderEditor(p: Props, args: splitScreenCrud.EditorArgs): VdomNode = {

      val header: VdomNode =
        renderHeader(p, args)

      val editorType: EditorType =
        args.id match {
          case \/-(rt: StaticReqType)   => EditorType.Static(rt)
          case -\/(())                  => EditorType.Custom(None)
          case \/-(id: CustomReqTypeId) =>
            p.project.config.reqTypes.custom.get(id) match {
              case Some(rt) =>
                if (rt.live.is(Dead))
                  EditorType.Dead(rt)
                else
                  EditorType.Custom(Some(rt))

              case None =>
                EditorType.ReqTypeDeleted
            }
        }

      def createOrUpdateButtons(idOption: Option[CustomReqTypeId], hard: Boolean): EditorButtons.Props =
        if (hard)
          EditorButtons.createOrHardUpdate(args)(idOption, p.potentialSaveCmd)(
            submitCmd         = submitCmd(p, _, _, _),
            hardDeleteConfirm = p.confirm("Are you sure you want to permanently delete this?"),
            hardDeleteCmd     = UpdateConfigCmd.CustomReqTypeDeleteHard,
            softDeleteCmd     = UpdateConfigCmd.CustomReqTypeDeleteSoft)

        else
          EditorButtons.createOrUpdate(args)(idOption, p.potentialSaveCmd)(
            submitCmd = submitCmd(p, _, _, _),
            deleteCmd = UpdateConfigCmd.CustomReqTypeDeleteSoft)

      def customReqTypeEditor(rtOption: Option[CustomReqType], enabled: Enabled) = {
        val lens = editorStateLensForCustom(CustomReqTypeEditor.State.init(rtOption))
        CustomReqTypeEditor.Props(
          filterDead = p.effectiveFilterDead,
          state      = args.state.zoomStateL(lens),
          project    = p.project.config,
          enabled    = enabled,
        ).render
      }

      def staticReqTypeEditor(rt: StaticReqType) =
        StaticReqTypeEditor.Props(rt).render

      editorType match {

        case EditorType.Custom(rtOption) =>
          val notInUse = rtOption.exists(rt => !p.project.isReqTypeInUse(rt.id))
          val notice   = notInUseNotice.when(notInUse)
          val editor   = customReqTypeEditor(rtOption, Enabled)
          val buttons  = createOrUpdateButtons(rtOption.map(_.id), hard = notInUse).render
          <.div(header, notice, editor, buttons)

        case EditorType.Static(rt) =>
          val editor = staticReqTypeEditor(rt)
          val buttons = EditorButtons.close(args).render
          <.div(header, editor, buttons)

        case EditorType.Dead(rt) =>
          val editor = customReqTypeEditor(Some(rt), Disabled)
          val buttons = EditorButtons.restore(args)(submitCmd(p, UpdateConfigCmd.CustomReqTypeRestore(rt.id), _, _)).render
          <.div(header, editor, buttons)

        case EditorType.ReqTypeDeleted =>
          args.close.runNow()
          EmptyVdom
      }
    }

    def render(p: Props): VdomNode = {
       // println(("="*60) + "\n" + p.state.value + "\n")

      splitScreenCrud(
        filterDeadOverride = p.filterDeadOverride,
        project            = p.project,
        newButton          = renderNewButton(p, _),
        list               = renderLeft(p, _),
        rightEmpty         = rightEmpty,
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