package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.ExternalPubid.LookupFailure
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{Event, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.feature.{AsyncFeature, PreviewFeature, TableNavigationFeature}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.{CreateContentCmd, UpdateContentCmd}
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.{EditTheme, NoContentMessage}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.EditorNavParent
import shipreq.webapp.client.project.widgets.ProjectWidgets.emptySpan
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object ReqDetail {
  import Shared.tableNavigationFeature

  private val bigTextEditorStyle = EditTheme.Style(
    PreviewFeature.Position.Right,
    EditTheme.OpenPreview.ShowWithControls,
    EditTheme.WhenInTransit.ReadOnlyViewWithSpinner,
  )

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[DynamicProps]
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentDidMount(_.backend.onMount)
      .build

  final case class StaticProps(sspUpdateContent     : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, VerifiedEvent.Seq],
                               sspCreateContent     : ServerSideProcInvoker[CreateContentCmd, ErrorMsg, NewEvents],
                               reqDetailRC          : RouterCtl[ExternalPubid],
                               webWorker            : WebWorkerClient.Instance,
                               pxProjectAndOrd      : Px[ProjectAndOrd],
                               pxViewReqDataCache   : Px[ViewReqDataCache],
                               pxTextSearch         : Px[TextSearch],
                               pxProjectWidgetsNoCtx: Px[ProjectWidgets.NoCtx]) {
    val pxProject = pxProjectAndOrd.map(_.project)
    val pxProjectConfig = pxProject.map(_.config).withReuse
  }

  final case class DynamicProps(extPubid   : ExternalPubid,
                                filterDead : StateSnapshot[FilterDead],
                                reqProps   : ReqId => ReqProps,
                                editorUCS  : EditorFeature.ReadWrite.ForUseCaseSteps,
                                state      : StateSnapshot[State],
                                newReqState: StateSnapshot[NewReqButton.State],
                                newReqAsync: AsyncFeature.ReadWrite.D0[ErrorMsg])

  final case class ReqProps(editor: EditorFeature.ReadWrite.ForReq,
                            async : AsyncFeature.ReadWrite.D1[Cell, ErrorMsg])

  type State = Modal.State

  def initState: State =
    Modal.none

  /**
   * All data associated with a requirement required for this screen.
   *
   * Cached by its inputs.
   */
  private final class Data(sp              : StaticProps,
                       val project         : Project,
                       val ord             : WebWorkerCmd.Ord,
                       val req             : Req,
                           viewReqDataCache: ViewReqDataCache,
                           upstreamFD      : FilterDead) {

    val pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]] =
      Reusable.byRef(
        sp.pxProjectWidgetsNoCtx.map(_ withCtx ProjectText.Context.Req(req.id)))

    val pxPlainText: Px[PlainText.ForProject.AnyCtx] =
      pxProjectWidgets.value.map(_.plainText)

    val reqType = project.config.reqTypes.need(req.reqTypeId)

    val live = req.live(project.config.reqTypes)

    val filterDead = live match {
      case Live => upstreamFD
      case Dead => ShowDead
    }

    val rows: Vector[Row] = {
      val fields = project.config.fieldsForReqTypeIterator(req.reqTypeId, filterDead).toVector
      fields.foldLeft(Row head filterDead)((q, f) => q ++ Row.fromField(f.fieldId))
    }

    val pubidText = PlainText.pubid(req.pubid, project)

    private val viewData: ViewReq.Data =
      viewReqDataCache(filterDead)(req.id)

    val pxView: Px[Reusable[ViewReq[VdomTag]]] =
      pxProjectWidgets.value.map { pw =>
        Reusable.ap(
          Reusable.byRef(viewData),
          Reusable.byRef(pw),
        )(_(_).copy(fmtReqTypeShort = false))
      }

    val useCaseData: Option[UseCaseData] =
      req match {
        case uc: UseCase => Some(new UseCaseData(uc))
        case _           => None
      }
  }

  private final class UseCaseData(val uc: UseCase) {
    private def stepData(row: Row.UseCaseSteps) = {
      val steps = row.field.useCaseSteps.get(uc)
      val range = row.treeFilter(steps.tree)
      UseCaseStepTree.StepData(row, steps, range)
    }
    val stepsN = stepData(Row.UseCaseStepsN)
    val stepsA = stepData(Row.UseCaseStepsA)
    val stepsE = stepData(Row.UseCaseStepsE)
  }

  // TODO Better performance if cells are (components + shouldComponentRender) or cached

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final class Backend(SP: StaticProps, $: BackendScope[DynamicProps, Unit]) {
    import SP._

    private val pxExtPubid   = Px.props($).map(_.extPubid).withReuse.manualRefresh
    private val pxUpstreamFD = Px.props($).map(_.filterDead.value).withReuse.manualRefresh

    private def refreshPx(): Unit =
      Px.refresh(pxExtPubid, pxUpstreamFD)

    private val pxData: Px[LookupFailure \/ Data] =
      for {
        p <- pxProjectAndOrd
        e <- pxExtPubid
        v <- pxViewReqDataCache
        f <- pxUpstreamFD
      } yield
        e.lookup(p.project).map(new Data(SP, p.project, p.ord, _, v, f))

    private val cbData: CallbackOption[Data] =
      Callback(refreshPx()).toCBO >> pxData.toCallback.map(_.toOption).asCBO

    private val setFilterDead: Reusable[StateSnapshot.SetFn[FilterDead]] =
      Reusable.byRef((v, cb) => $.props.flatMap(_.filterDead.setStateOption(v, cb)))

    private def mkRunCmdFn[Cmd <: UpdateContentCmd](onSuccess: VerifiedEvent.Seq => Callback) =
      Reusable.fn[ReqId, Cell, Cmd, Callback](
        (reqId, cell, cmd) =>
          $.props.flatMap(p =>
            p.reqProps(reqId).async.write(cell)(
              sspUpdateContent(cmd).rightFlatMap(onSuccess(_).asAsyncCallback)
            )
          )
      )

    private val runCmd: ReqId ~=> (Cell ~=> (UpdateContentCmd ~=> Callback)) =
      mkRunCmdFn(_ => Callback.empty)

    private val runAddAndEditNewUseCaseStep: ReqId ~=> (Cell ~=> (UpdateContentCmd.AddUseCaseStep ~=> Callback)) =
      mkRunCmdFn { ves =>

        val startEditor: Callback =
          ves.iterator.map(_.event).collect {
            case e: Event.UseCaseStepCreate => startUseCaseStepEditor(e.id).delayMs(50).toCallback
          }.nextOption().getOrEmpty

        startEditor
      }

    private var useCaseStepRefs: Map[UseCaseStepId, EditorNavParent.ComponentRef] =
      UnivEq.emptyMap

    private def useCaseStepRef(id: UseCaseStepId): EditorNavParent.ComponentRef =
      useCaseStepRefs.get(id).getOrElse {
        val ref = Ref.toScalaComponent(EditorNavParent.Component)
        useCaseStepRefs = useCaseStepRefs.updated(id, ref)
        ref
      }

    private def startUseCaseStepEditor(id: UseCaseStepId): Callback =
      for {
        data      <- cbData
        props     <- $.props.toCBO
        key       = FieldKey.UseCaseStep(id)
        editor    = props.editorUCS(key, data.pxProjectWidgets, data.filterDead)
        ref       <- CallbackTo(useCaseStepRefs.get(id)).asCBO
        component <- ref.get
        _         <- CallbackOption.liftOptionCallback(component.backend.startEdit(editor))
      } yield ()

    private def setModal(modal: Modal.State): Callback =
      $.props >>= (_.state setState modal)

    private def clearModal: Callback =
      setModal(Modal.none)

    private def renderNotFound(ep: ExternalPubid): VdomElement = {
      val projectName: String = pxProject.value().name
      val id         : String = PlainText pubid ep
      NoContentMessage.becauseNotFound(
        s"$id doesn't exist.",
        TagMod(
          *.errorDesc,
          s"$id is not, and has never been, a requirement in $projectName.",
          <.br(*.errorBr),
          "Had it been deleted or assigned a new ID, you'd still be able to see it here.")
      )(*.errorCont)
    }

    def render(p: DynamicProps): VdomElement =
      <.main(
        p.state.value renderOrElse {
          refreshPx()
          pxData.value() match {
            case \/-(data)             => renderDetail(p, data)
            case -\/(_: LookupFailure) => renderNotFound(p.extPubid)
          }
        })

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private val titleCellRef = Ref[html.Element]
    private val tableRef     = Ref[html.Table]
    private val tableBase    = <.table.withRef(tableRef)(*.detailTable)

    val onMount: Callback =
      for {
        table <- tableRef.get
        _     <- TableNavigationFeature.SpecialCases(table)(tableNavExceptions)
      } yield ()

    private def tableNavExceptions: TableNavigationFeature.SpecialCases.Handler =
      ctx => {
        def cell(row: Int) = ctx.bodyRow(row).children(1)
        def isFirstRow     = ctx.target == cell(0)
        def isLastRow      = ctx.target == cell(-1)
        def focusTitle     = titleCellRef.get.map(_.focus()).toCallback
        CallbackOption.keyCodeSwitch(ctx.event) {
          case KeyCode.Up   if isFirstRow => focusTitle
          case KeyCode.Down if isLastRow  => focusTitle
        }
      }

    private def renderDetail(props: DynamicProps, data: Data): VdomElement = {
      import data.{project, req, pubidText}

      val pw           = data.pxProjectWidgets.value()
      val reusableView = data.pxView.value()
      val view         = reusableView.value
      val reqProps     = props.reqProps(req.id)
      val fieldName    = pxProjectConfig.value().fieldName

      def reqEditor(fk: FieldKey.ForSomeReq): EditorFeature.ReadWrite.For[fk.type] =
        reqProps.editor(fk, data.pxProjectWidgets, data.filterDead)

      def renderPageHeader: VdomElement =
        HeaderRow.Props(
          pubidText    = pubidText,
          live         = data.live,
          titleEditor  = reqEditor(FieldKey.reqTitle(req.id)),
          titleView    = Reusable.byRef(view.title),
          filterDead   = StateSnapshot.withReuse(props.filterDead.value)(setFilterDead),
          tableRef     = tableRef,
          titleCellRef = titleCellRef,
        ).render

      def renderRows =
        tableBase(
          <.tbody(
            data.rows.toVdomArray(renderRow)))

      def renderRow(row: Row): VdomElement = {

        def genericEditableRow(name      : String,
                               headerLive: Live = Live,
                               dataLive  : Live = data.live,
                               field     : FieldKey.ForSomeReq#AndArgs) =
          GenericEditableRow.Props(
            row        = row,
            name       = name,
            headerLive = headerLive,
            dataLive   = dataLive,
            field      = field,
            editor     = reqEditor(field.key),
            view       = reusableView,
          ).render

        def withoutReusability(name      : String,
                               headerLive: Live = Live,
                               dataLive  : Live = data.live)
                              (f         : Shared.DataCell => VdomNode) =
          Shared.renderRow(
            row        = row,
            name       = name,
            headerLive = headerLive,
            dataLive   = dataLive,
          )(f)

        def useCaseStepsCell(name: String, f: UseCaseData => UseCaseStepTree.StepData) = {
          val d = data.useCaseData.get
          withoutReusability(name)(_.nonDirectlyEditableNavParent(renderStepTree(d, f(d))))
        }

        row match {

          case Row.CustomField(id) =>
            val l = project.config.fields.customFields.need(id).live(project.config)
            val field: FieldKey.ForSomeReq#AndArgs =
              id match {
                case id: CustomField.Text.Id        => FieldKey.CustomTextField(id).andArgs(bigTextEditorStyle)
                case id: CustomField.Tag.Id         => FieldKey.CustomFieldTags(id).andArgs(())
                case id: CustomField.Implication.Id => FieldKey.Implications(-\/(id)).andArgs(())
              }
            genericEditableRow(
              name       = fieldName(id),
              headerLive = l,
              dataLive   = data.live & l,
              field      = field,
            )

          case Row.ReqType =>
            ReqTypeRow.Props(
              reqType          = data.reqType,
              live             = data.live,
              filterDead       = data.filterDead,
              editor           = reqEditor(FieldKey.ReqType),
              view             = reusableView,
              projectWidgets   = pw,
              reqTypes         = project.config.reqTypes,
              newReqState      = props.newReqState,
              newReqAsync      = props.newReqAsync,
              sspCreateContent = sspCreateContent,
              reqDetailRC      = reqDetailRC,
            ).render

          case Row.Life =>
            LifeRow.Props(
              reqId           = req.id,
              live            = data.live,
              allowLiveChange = req.allowLiveChange(project.config.reqTypes),
              delete          = deleteFn,
              restore         = restoreFn,
            ).render

          case Row.Implications =>
            ImplicationsRow.Props(
              pubidText = pubidText,
              live      = data.live,
              editorF   = reqEditor(FieldKey.Implications.byDir(Forwards)),
              editorB   = reqEditor(FieldKey.Implications.byDir(Backwards)),
              view      = reusableView,
            ).render

          case Row.OtherTags =>
            genericEditableRow(
              name  = StaticField.OtherTags.name,
              field = FieldKey.OtherTags.noArgs,
            )

          case Row.AllTags =>
            genericEditableRow(
              name  = StaticField.AllTags.name,
              field = FieldKey.AllTags.noArgs,
            )

          case Row.ImplicationGraph =>
            withoutReusability(StaticField.ImplicationGraph.name) { cell =>
              cell.nonDirectlyEditableNavParent(
                ImplicationGraph.Props.FocusReq(
                  ord         = data.ord,
                  focus       = req.id,
                  filterDead  = data.filterDead,
                  project     = project,
                  plainText   = data.pxPlainText.value(),
                  reqDetailRC = reqDetailRC,
                  webWorker   = webWorker
                ).render)
            }

          case Row.UseCaseStepsN => useCaseStepsCell(UiText.FieldNames.useCaseStepTreeN, _.stepsN)
          case Row.UseCaseStepsA => useCaseStepsCell(UiText.FieldNames.useCaseStepTreeA, _.stepsA)
          case Row.UseCaseStepsE => useCaseStepsCell(StaticField.ExceptionStepTree.name, _.stepsE)

          case Row.StepGraph =>
            withoutReusability(StaticField.StepGraph.name) { cell =>
              val ucId = data.useCaseData.get.uc.id
              cell.nonDirectlyEditableNavParent(UseCaseStepFlowGraph.Props(data.ord, ucId, pw.ctx, webWorker).render)
            }

          case Row.Codes =>
            genericEditableRow(
              name  = SpecialBuiltInField.Codes.name,
              field = FieldKey.Codes.noArgs,
            )

          case Row.DeletionReason =>
            withoutReusability(SpecialBuiltInField.DeletionReason.name) { cell =>
              cell.nonDirectlyEditableNavParent(view.deletionReason getOrElse emptySpan)
            }

          case Row.PastPubids =>
            withoutReusability(UiText.FieldNames.pastPubids, dataLive = Dead) { cell =>
              cell.nonDirectlyEditableNavParent(view.pastPubids)
            }
        }
      }

      def renderStepTree(ucData: UseCaseData, stepData: UseCaseStepTree.StepData) = {
        val cmdRunner    = AsyncFeature.Runner.D1(reqProps.async.read, runCmd(req.id))
        val addCmdRunner = AsyncFeature.Runner.D1(reqProps.async.read, runAddAndEditNewUseCaseStep(req.id))

        val renderBody: UseCaseStepTree.RenderBodyFn = args => {
          import FieldKey.UseCaseStep
          import args.id

          val editor = props.editorUCS(UseCaseStep(id), data.pxProjectWidgets, data.filterDead)

          def editorArgs = UseCaseStep.Args(
            Some(cmdRunner(Cell.UseCaseStepCtrls(id))),
            Some(addCmdRunner(Cell.AddUseCaseStep(id))))

          def addStepAfterSelf: CallbackOption[Unit] =
            for {
              _      <- CallbackOption.unless(editor.read.isOpen)
              step    = project.content.reqs.useCases.focusStep(id)
              addCmd <- CallbackOption.liftOption(UpdateContentCmd.addUseCaseStepAfter(step))
              _      <- CallbackOption.liftOptionCallback(addCmdRunner(Cell.AddUseCaseStep(id)).runOption(addCmd))
            } yield ()

          def onKeyDown(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
            UseCaseStepEditor.saveAndAddKeyCriterion.toCallbackOption(e) >> addStepAfterSelf

          val stepProps =
            EditorNavParent.Props(
              args.base,
              editor,
              editorArgs,
              pw.useCaseStepTextAndFlow(args.textAndFlow(), args.live),
              onKeyDown,
            )

          val ref = useCaseStepRef(id)

          EditorNavParent.Component.withRef(ref)(stepProps)
        }

        UseCaseStepTree.Props(
          ucData.uc,
          stepData,
          data.filterDead,
          project.content.reqs.useCases,
          renderBody,
          cmdRunner,
          addCmdRunner,
        ).render
      }

      <.div(
        renderPageHeader,
        renderRows)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private def wholeReqCmd(reqId: ReqId, cmd: UpdateContentCmd): Callback =
      $.props.flatMap(p =>
        p.reqProps(reqId).async.write(Cell.WholeReq).onFailureShowAndForget(
          sspUpdateContent(cmd)))

    private def delete(id: ReqId): Callback =
      CallbackTo {
        def run(cmd: UpdateContentCmd): Callback = wholeReqCmd(id, cmd) >> clearModal
        import Px.AutoValue._
        val data = DeletionFeature.deletionData(pxProject, NonEmptySet one id)
        val props = DeletionFeature.DeletionFormProps(data, pxProjectWidgetsNoCtx, pxTextSearch, run, clearModal)
        Some(Modal(props.render))
      } >>= setModal

    private def restore(id: ReqId): Callback =
      CallbackTo {
        def run(cmd: UpdateContentCmd): Callback = wholeReqCmd(id, cmd) >> clearModal
        import Px.AutoValue._
        val data = DeletionFeature.restorationData(pxProject, NonEmptySet one id)
        val props = DeletionFeature.RestorationFormProps(data, pxProjectWidgetsNoCtx, run, clearModal)
        Some(Modal(props.render))
      } >>= setModal

    private val deleteFn = Reusable.fn(delete _)
    private val restoreFn = Reusable.fn(restore _)

  } // Backend
}
