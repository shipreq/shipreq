package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.semantic.Header
import shipreq.webapp.base.ui.{BaseStyles, NoContentMessage}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.base.UiText
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.EditorNavParent
import shipreq.webapp.client.project.widgets._
import ExternalPubid.LookupFailure
import ProjectWidgets.emptySpan

object ReqDetail {

  private implicit val tableNavigationFeature = TableNavigationFeature.NoRowSpans

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[DynamicProps]("ReqDetail")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .build

  final case class StaticProps(updateIO             : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, VerifiedEvent.Seq],
                               reqDetailRC          : RouterCtl[ExternalPubid],
                               webWorker            : WebWorkerClient,
                               pxProject            : Px[Project],
                               pxViewReqDataCache   : Px[ViewReqDataCache],
                               pxTextSearch         : Px[TextSearch],
                               pxProjectWidgetsNoCtx: Px[ProjectWidgets.NoCtx]) {
    val pxProjectConfig = pxProject.map(_.config).withReuse
  }

  final case class DynamicProps(extPubid  : ExternalPubid,
                                filterDead: StateSnapshot[FilterDead],
                                reqProps  : ReqId => ReqProps,
                                editorUCS : EditorFeature.ReadWrite.ForUseCaseSteps,
                                state     : StateSnapshot[State])

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
  final class Data(sp              : StaticProps,
               val project         : Project,
               val req             : Req,
                   viewReqDataCache: ViewReqDataCache,
                   upstreamFD      : FilterDead) {

    val pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]] =
      Reusable.byRef(
        sp.pxProjectWidgetsNoCtx.map(_ withCtx ProjectText.Context.Req(req.id)))

    val pxPlainText: Px[PlainText.ForProject.AnyCtx] =
      pxProjectWidgets.value.map(_.plainText)

    val live = req.live(project.config.reqTypes)

    val filterDead = live match {
      case Live => upstreamFD
      case Dead => ShowDead
    }

    val rows: Vector[Row] = {
      val liveFilter = filterDead.filterFnBy((_: Field) live project.config)
      val fields = project.config.fields.fields.filter(f =>
        project.config.fields.applicability(req.reqTypeId, f.fieldId) is Applicable && liveFilter(f))
      fields.foldLeft(Row head filterDead)((q, f) => q ++ Row.fromField(f.fieldId))
    }

    val pubidText = PlainText.pubid(req.pubid, project)

    val viewData: ViewReq.Data =
      viewReqDataCache(filterDead)(req.id)

    val useCaseData: Option[UseCaseData] =
      req match {
        case uc: UseCase => Some(new UseCaseData(uc))
        case _           => None
      }

    val useCaseStepFilter: VectorTree.PartialLocation => Boolean =
      filterDead.filterFnBy(Live whenValid _.validity)
  }

  final class UseCaseData(val uc: UseCase) {
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
  // TODO Make everything private

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final class Backend(SP: StaticProps, $: BackendScope[DynamicProps, Unit]) {
    import SP._

    val pxExtPubid   = Px.props($).map(_.extPubid).withReuse.manualRefresh
    val pxUpstreamFD = Px.props($).map(_.filterDead.value).withReuse.manualRefresh

    private def refreshPx(): Unit =
      Px.refresh(pxExtPubid, pxUpstreamFD)

    val pxData: Px[LookupFailure \/ Data] =
      for {
        p <- pxProject
        e <- pxExtPubid
        v <- pxViewReqDataCache
        f <- pxUpstreamFD
      } yield
        e.lookup(p).map(new Data(SP, p, _, v, f))

    val cbData: CallbackOption[Data] =
      Callback(refreshPx()).toCBO >> pxData.toCallback.map(_.toOption).asCBO

    val setFilterDead: Reusable[StateSnapshot.SetFn[FilterDead]] =
      Reusable.byRef((v, cb) => $.props.flatMap(_.filterDead.setStateOption(v, cb)))

    private def mkRunCmdFn[Cmd <: UpdateContentCmd](onSuccess: VerifiedEvent.Seq => Callback) =
      Reusable.fn[ReqId, Cell, Cmd, Callback](
        (reqId, cell, cmd) =>
          $.props.flatMap(p =>
            p.reqProps(reqId).async.write(cell)(
              updateIO(cmd).rightFlatMap(onSuccess(_).asAsyncCallback)
            )
          )
      )

    val runCmd: ReqId ~=> (Cell ~=> (UpdateContentCmd ~=> Callback)) =
      mkRunCmdFn(_ => Callback.empty)

    val runAddAndEditNewUseCaseStep: ReqId ~=> (Cell ~=> (UpdateContentCmd.AddUseCaseStep ~=> Callback)) =
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

    def startUseCaseStepEditor(id: UseCaseStepId): Callback =
      for {
        data      ← cbData
        props     ← $.props.toCBO
        key       = EditorFeature.FieldKey.UseCaseStep(id)
        editor    = props.editorUCS(key, data.pxProjectWidgets, data.filterDead)
        ref       ← CallbackTo(useCaseStepRefs.get(id)).asCBO
        component ← ref.get
        _         ← CallbackOption.liftOptionCallback(component.backend.startEdit(editor))
      } yield ()

    def setModal(modal: Modal.State): Callback =
      $.props >>= (_.state setState modal)

    def clearModal: Callback =
      setModal(Modal.none)

    def renderNotFound(ep: ExternalPubid): VdomElement = {
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

    val emptyRow: VdomElement = <.span

    val impRowSubBase =
      <.td(*.generalImpsSide, ^.tabIndex := -1)

    def render(p: DynamicProps): VdomElement =
      <.main(
        BaseStyles.containerFull,
        p.state.value renderOrElse {
          refreshPx()
          pxData.value() match {
            case \/-(data)             => renderDetail(p, data)
            case -\/(_: LookupFailure) => renderNotFound(p.extPubid)
          }
        })

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private val headerStyle: Live => Header.Style =
      Live.memo(l => Header.Style(Header.Type.H1, other = *.headerText(l)))

    private val rowHeader: Live => VdomTag =
      Live.memo(l => <.th(*.detailTableKey(l)))

    private val rowData: Live => VdomTag =
      Live.memo(l =>
        <.td(
          *.detailTableValue(l),
          ^.tabIndex := -1))

    def renderDetail(props: DynamicProps, data: Data): VdomElement = {
      import data.{project, req, pubidText}

      val pw        = data.pxProjectWidgets.value()
      val reqProps  = props.reqProps(req.id)
      val reqEditor = reqProps.editor
      val fieldName = pxProjectConfig.value().fieldName
      val view      = data.viewData(pw).copy(fmtReqTypeShort = false)

      def renderHeader: VdomElement = {
        val hstyle = headerStyle(data.live)

        <.div(*.headerRow,

          <.div(*.headerPubid,
            Header(hstyle, pubidText + ":")),

          <.div(*.headerTitle,
            reqEditor(EditorFeature.FieldKey.reqTitle(req.id), data.pxProjectWidgets, data.filterDead)
              .themedRenderOr(())(
                Header(hstyle, view.title))),

          <.div(*.headerFilterDeadButton,
            FilterDeadButton.whenLive(data.live)(StateSnapshot.withReuse(props.filterDead.value)(setFilterDead))))
      }

      def renderRows =
        <.table(*.detailTable,
          <.tbody(
            data.rows.toVdomArray(renderRow)))

      def renderRow(row: Row): VdomElement = {

        val headerDataLive: (Live, Live) =
          row match {
            case Row.Codes
               | Row.ReqType
               | Row.Tags
               | Row.Implications
               | Row.ImplicationGraph
               | Row.UseCaseStepsN
               | Row.UseCaseStepsA
               | Row.UseCaseStepsE
               | Row.StepGraph        => (Live, data.live)
            case Row.DeletionReason
               | Row.PastPubids       => (Live, Dead)
            case Row.Life             => (Live, Live) // When req is dead, [Restore] should be highlighted is active
            case Row.CustomField(id)  =>
              val l = project.config.fields.customFields.need(id).live(project.config)
              (l, data.live & l)
          }

        <.tr(
          ^.key := row.key,
          rowHeader(headerDataLive._1)(renderRowTitle(row)),
          renderRowData(rowData(headerDataLive._2), row))
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      def renderRowTitle(row: Row): VdomNode =
        row match {
          case Row.CustomField(id)  => fieldName(id)
          case Row.Codes            => UiText.FieldNames.reqCodes
          case Row.ReqType          => UiText.FieldNames.reqType
          case Row.Tags             => UiText.FieldNames.tags
          case Row.Implications     => UiText.FieldNames.implications
          case Row.ImplicationGraph => UiText.FieldNames.implicationGraph
          case Row.UseCaseStepsN    => UiText.FieldNames.useCaseStepTreeN
          case Row.UseCaseStepsA    => UiText.FieldNames.useCaseStepTreeA
          case Row.UseCaseStepsE    => UiText.FieldNames.useCaseStepTreeE
          case Row.DeletionReason   => UiText.FieldNames.deletionReason
          case Row.StepGraph        => UiText.FieldNames.useCaseStepFlowGraph
          case Row.PastPubids       => UiText.FieldNames.pastPubids
          case Row.Life             => UiText.Life.field
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      // TODO Test that this applies applicability
      def renderRowData(cellBase: VdomTag, row: Row): VdomElement = {
        import EditorFeature.FieldKey

        def editableCell(key: FieldKey.ForSomeReq): VdomElement = {
          val editor = reqEditor(key, data.pxProjectWidgets, data.filterDead)
          EditorNavParent.Props(cellBase, editor, view.editable(key))
            .render
        }

        def nonDirectlyEditorNavParent(t: TagMod): VdomElement =
          cellBase(tableNavigationFeature.onKeyDown, t)

        def useCaseStepsCell(f: UseCaseData => UseCaseStepTree.StepData): VdomElement = {
          val d = data.useCaseData.get
          nonDirectlyEditorNavParent(renderStepTree(d, f(d)))
        }

        row match {
          case Row.CustomField(id: CustomField.Text.Id) =>
            editableCell(FieldKey.CustomTextField(id))

          case Row.CustomField(id: CustomField.Tag.Id) =>
            editableCell(FieldKey.Tags(Some(id)))

          case Row.CustomField(id: CustomField.Implication.Id) =>
            editableCell(FieldKey.Implications(-\/(id)))

          case Row.Codes =>
            editableCell(FieldKey.Codes)

          case Row.ReqType =>
            editableCell(FieldKey.ReqType)

          case Row.Tags =>
            editableCell(FieldKey.Tags(None))

          case Row.DeletionReason =>
            nonDirectlyEditorNavParent(view.deletionReason getOrElse emptySpan)

          case Row.PastPubids =>
            nonDirectlyEditorNavParent(view.pastPubids)

          case Row.Implications =>
            def renderHalf(dir: Direction) = {
              val key = FieldKey.Implications(\/-(dir))
              val editor = reqEditor(key, data.pxProjectWidgets, data.filterDead)
              EditorNavParent.Props(impRowSubBase, editor, view.editable(key))
                .render
            }
            nonDirectlyEditorNavParent(
              <.table(
                TableNavigationFeature.nestedTable,
                *.generalImpsCont,
                <.tbody(
                  <.tr(
                    renderHalf(Backwards),
                    <.td(*.generalImpsMiddle, s"→ $pubidText →"),
                    renderHalf(Forwards)))))

          case Row.ImplicationGraph =>
            nonDirectlyEditorNavParent(
              ImplicationGraph.Props(
                Some(req.id), data.filterDead,
                project.content.implications, project.content.reqs, project.config.reqTypes,
                data.pxPlainText.value(),
                reqDetailRC,
                webWorker
              ).render)

          case Row.UseCaseStepsN =>
            useCaseStepsCell(_.stepsN)

          case Row.UseCaseStepsA =>
            useCaseStepsCell(_.stepsA)

          case Row.UseCaseStepsE =>
            useCaseStepsCell(_.stepsE)

          case Row.StepGraph =>
            val ucId = data.useCaseData.get.uc.id
            nonDirectlyEditorNavParent(UseCaseStepFlowGraph.Props(ucId, project, pw.ctx, webWorker).render)

          case Row.Life =>
            nonDirectlyEditorNavParent(
              data.live match {
                case Live =>
                  LifeButton.Delete withStatusOnLeft delete(req.id)
                case Dead =>
                  LifeButton.Restore.withStatusOnLeft(
                    req.allowLiveChange(project.config.reqTypes) option restore(req.id))
              })
        }
      }

      def renderStepTree(ucData: UseCaseData, stepData: UseCaseStepTree.StepData) = {
        val cmdRunner    = AsyncFeature.Runner.D1(reqProps.async.read, runCmd(req.id))
        val addCmdRunner = AsyncFeature.Runner.D1(reqProps.async.read, runAddAndEditNewUseCaseStep(req.id))

        val renderBody: UseCaseStepTree.RenderBodyFn = args => {
          import EditorFeature.FieldKey.UseCaseStep
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
        renderHeader,
        renderRows)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def runActionNoAsync(cmd: UpdateContentCmd): Callback =
      updateIO(cmd).leftFlatTapSync(e => Callback.alert(e.value)).toCallback

    def delete(id: ReqId): Callback =
      CallbackTo {
        def run(cmd: UpdateContentCmd): Callback = runActionNoAsync(cmd) >> clearModal
        import Px.AutoValue._
        val data = DeletionFeature.deletionData(pxProject, NonEmptySet one id)
        val props = DeletionFeature.DeletionFormProps(data, pxProjectWidgetsNoCtx, pxTextSearch, run, clearModal)
        Some(Modal(props.render))
      } >>= setModal

    def restore(id: ReqId): Callback =
      CallbackTo {
        def run(cmd: UpdateContentCmd): Callback = runActionNoAsync(cmd) >> clearModal
        import Px.AutoValue._
        val data = DeletionFeature.restorationData(pxProject, NonEmptySet one id)
        val props = DeletionFeature.RestorationFormProps(data, pxProjectWidgetsNoCtx, run, clearModal)
        Some(Modal(props.render))
      } >>= setModal

  } // Backend
}
