package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{UpdateContentCmd, UpdateContentFn}
import shipreq.webapp.base.text._
import shipreq.webapp.client.app.reqtable.ColumnRenderer.RenderDeletionReason
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.app.Style.{reqdetail => *}
import shipreq.webapp.client.app.WebWorkerClient
import shipreq.webapp.client.data._
import shipreq.webapp.client.feature._
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.protocol.{ClientProtocol, ServerCall}
import shipreq.webapp.client.widgets.Checkbox
import shipreq.webapp.client.widgets.high.{DeletionForm, ImplicationGraph, ProjectWidgets, UseCaseStepFlowGraph}

object ReqDetail extends StaticPropComponent.Template("ReqDetail") {
  override protected def configureBackend = new Backend(_, _)
  override protected def configureRender  = _.renderBackend

  type InitEditor = ContentEditorFeature.D1.InitChild[Cell, Cell]

  case class StaticProps(cd                   : ClientData,
                         cp                   : ClientProtocol,
                         webWorker            : WebWorkerClient,
                         updateContentFn      : UpdateContentFn.Instance,
                         pxPlainTextNoCtx     : Px[PlainText.ForProject],
                         pxTextSearch         : Px[TextSearch],
                         pxProjectWidgetsNoCtx: Px[ProjectWidgets])

  case class DynamicProps(extPubid  : ExternalPubid,
                          filterDead: ReusableVar[FilterDead],
                          reqProps  : ReqId => ReqProps,
                          state     : ReusableVar[State])

  case class ReqProps(initEditor  : InitEditor,
                      asyncFeature: AsyncActionFeature  .D1.Feature[Cell, String],
                      edit        : ContentEditorFeature.D1.State.ReadOnly[Cell],
                      async       : AsyncActionFeature  .D1.State.ReadOnly[Cell, String])

  type State = Modal.State

  def initState: State =
    Modal.none

  /**
   * All data associated with a requirement required for this screen.
   *
   * Cached by its inputs.
   */
  class Data(sp        : StaticProps,
         val project   : Project,
         val req       : Req,
             upstreamFD: FilterDead) {

    val (pxPlainText, pxProjectWidgets) = {
      val textCtx: Option[ProjectText.Context] = req match {
        case uc: UseCase    => Some(ProjectText.Context.UseCase(uc.id))
        case _ : GenericReq => None
      }
      var t = sp.pxPlainTextNoCtx
      var w = sp.pxProjectWidgetsNoCtx
      for (c <- textCtx) {
        t = t.map(_ withCtx c)
        w = Px.apply2(w, t)(_ withPlainText _)
      }
      (t, w)
    }

    val live = req.live(project.config.reqTypes)

    val filterDead = live match {
      case Live => upstreamFD
      case Dead => ShowDead
    }

    val rows = {
      val liveFilter = filterDead.filterFnA((_: Field) live project.config)
      val fields = project.config.fields.fields.filter(f =>
        f.applicable(req.reqTypeId) :: Applicable && liveFilter(f))
      fields.foldLeft(Row head filterDead)(_ ++ Row.fromField(_))
    }

    val pubidText = PlainText.pubid(req.pubid, project)

    val codeSet = project.reqCodes.activeReqCodesByReqId(req.id)
    val codes  = MutableArray(codeSet).sortBySchwartzian(PlainText.reqCode).to[List]

    val tagDist        = DataLogic.tagFieldDist(project.config, filterDead, _ => true)
    val tagLookup      = DataLogic.tagLookup(project, filterDead)
    val generalTagSet  = DataLogic.generalTags(tagDist, tagLookup)(req.id)
    val tagOrderByName = DataLogic.tagOrderByName(project.config.tags)
    val tagOrderByPos  = DataLogic.tagOrderByPos(project.config.tags)
    val generalTags    = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).to[Vector]

    val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
      Memo { fid =>
        def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(req.id)
        MutableArray(tagSet).sortBy(tagOrderByPos.apply).to[Vector]
      }

    val pubidSortKeyFn  = DataLogic.pubidSortKeyFn(project.config)
    val impFilter       = DataLogic.impValueFilter(project.config, filterDead)
    val customImpLookup = DataLogic.customFieldImps(project, impFilter)

    private def sortPubids(pubids: TraversableOnce[Pubid]): Vector[Pubid] =
      MutableArray(pubids)
        .sortBySchwartzian(pubidSortKeyFn)
        .to[Vector]

    val generalImps: Direction => Vector[Pubid] =
      Direction.memo(dir =>
        sortPubids(
          project.implications(dir)(req.id)
            .iterator
            .map(project.reqs.req)
            .filter(impFilter)
            .map(_.pubid)))

    val customImps: CustomField.Implication => Vector[Pubid] =
      Memo(f =>
        sortPubids(customImpLookup(f)(req.id)))

    val useCaseData: Option[UseCaseData] =
      req match {
        case uc: UseCase => Some(new UseCaseData(uc))
        case _           => None
      }

    val useCaseStepFilter: VectorTree.PartialLocation => Boolean =
      filterDead.filterFnA(Live whenValid _.validity)
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final class Backend(SP: StaticProps, $: BackendScope) {
    import SP._
    import cd.pxProject

    val pxFieldNameFn = pxProject.map(Field.nameP)
    val pxExtPubid    = Px.bsMP($).propsM(_.extPubid)
    val pxUpstreamFD  = Px.bsMP($).propsM(_.filterDead.value)

    val pxData: Px[PubidQueryError \/ Data] =
      for {
        p <- pxProject
        e <- pxExtPubid
        f <- pxUpstreamFD
      } yield
        p.findReq(e).map(new Data(SP, p, _, f))

    val filterDeadCheckbox =
      Checkbox.filterDead(v => $.props.flatMap(_.filterDead set v))

    val showDeadFakeCheckbox =
      <.label(
        <.input.checkbox(^.checked := true, ^.disabled := true),
        UiText.Life.showDead)

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    val runCmd = ReusableFn[ReqId, Cell, UpdateContentCmd, Callback](
      (reqId, cell, cmd) =>
        $.props >>= (p =>
          p.reqProps(reqId).asyncFeature(cell).wrapAsync((s, f) =>
            updateIO(cmd, s, f))))

    def setModal(modal: Modal.State): Callback =
      $.props >>= (_.state set modal)

    def clearModal: Callback =
      setModal(Modal.none)

    type EditFeature = ContentEditorFeature.D1.Feature[Cell]

    def createEditFeature(initEditor  : InitEditor,
                          asyncFeature: AsyncActionFeature.D1.Feature[Cell, String],
                          data        : Data): EditFeature = {
      import ContentEditorFeature._
      import data.req

      val static = Static(
        initEditor.parent, initEditor.preview,
        pxProject, data.pxPlainText, data.pxProjectWidgets, pxTextSearch,
        updateIO)

      def generalImps(cell: Cell) = {
        val dir = cell.implicationDirection
        Editor.ImplicationsAll(req, dir, data.generalImps(dir))
      }

      @inline implicit def autoSome[P](e: Editor[P]): Option[Editor[P]] = Some(e)
      def edit(cell: Cell): Option[Editor[Cell]] =
        cell match {
          case Cell.Title                                        => Editor.ReqTitle(req, cell)
          case Cell.Code                                         => Editor.ReqCodesForReq(req)
          case Cell.ImplicationSrc
             | Cell.ImplicationTgt                               => generalImps(cell)
          case Cell.ReqType                                      => Editor.reqType(req)
          case Cell.Tags                                         => Editor.Tags(req, None)
          case Cell.CustomField(fid: CustomField.Tag        .Id) => Editor.Tags(req, Some(fid))
          case Cell.CustomField(fid: CustomField.Text       .Id) => Editor.CustomTextField(req, fid, cell)
          case Cell.CustomField(fid: CustomField.Implication.Id) => Editor.ImplicationsCustomField(req, fid)
          case Cell.UseCaseStep(id)                              => Editor.UseCaseStep(id, cell)
          case Cell.UseCaseStepCtrls(_)
             | Cell.AddUseCaseStep(_)
             | Cell.AddUseCaseTailStep(_)                        => None
        }

      initEditor.feature((cell, el) =>
        D0.Feature(static, asyncFeature(cell))(el, edit(cell)))
    }

    def renderNotFound(failureReason: String): ReactElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    val emptyRow: ReactElement = <.span

    def render(p: DynamicProps): ReactElement =
      p.state.value renderOrElse {
        Px.refresh(pxExtPubid, pxUpstreamFD)
        pxData.value() match {
          case \/-(data)                                => renderDetail(p, data)
          case -\/(PubidQueryError.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.extPubid.mnemonic.value} not found.")
          case -\/(PubidQueryError.InvalidPos(rt, len)) => renderNotFound(s"${PlainText pubid p.extPubid} not found.")
        }
      }

    def focus: Callback = Callback.empty // TODO

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    def renderDetail(props: DynamicProps, data: Data): ReactElement = {
      import data.{project, req, pubidText}

      val pw          = data.pxProjectWidgets.value()
      val state       = props.reqProps(req.id)
      val fieldName   = pxFieldNameFn.value()
      val editFeature = createEditFeature(state.initEditor, state.asyncFeature, data)
      val runCmd      = this.runCmd(req.id)

      def renderAsyncEditorOrValue(cell: Cell, view: => TagMod): TagMod = {
        def startEdit = editFeature(cell).startEdit(focus)
        TagMod(
          ^.onDblClick -->? startEdit,
          state.async(cell) renderOr (state.edit(cell) renderOr view))
      }

      def renderHeader: ReactElement =
        <.div(
          *.header,
          <.div(
            *.headerId,
            pubidText + ": "),
          <.div(
            *.headerTitle,
              renderAsyncEditorOrValue(Cell.Title, pw.reqTitle(req))))

      def renderRows =
        <.table(
          *.mainTable,
          <.tbody(
            data.rows.iterator.map(renderRow)))

      def renderRow(row: Row): ReactElement =
        <.tr(
          ^.key := row.key,
          <.th(
            *.rowTitle,
            renderRowTitle(row)),
          <.td(
            *.rowValue,
            renderRowData(row)))

      def renderRowTitle(row: Row): ReactNode =
        row match {
          case Row.CustomField(f)   => fieldName(f)
          case Row.Code             => UiText.FieldNames.reqCodes
          case Row.ReqType          => UiText.FieldNames.reqType
          case Row.Tags             => UiText.FieldNames.tags
          case Row.Implications     => UiText.FieldNames.implications
          case Row.ImplicationGraph => UiText.FieldNames.implicationGraph
          case Row.UseCaseStepsN    => UiText.FieldNames.useCaseStepTreeN
          case Row.UseCaseStepsA    => UiText.FieldNames.useCaseStepTreeA
          case Row.UseCaseStepsE    => UiText.FieldNames.useCaseStepTreeE
          case Row.DeletionReason   => UiText.FieldNames.deletionReason
          case Row.StepGraph        => UiText.FieldNames.useCaseStepFlowGraph
          case Row.Life             => UiText.Life.field
        }

      def renderImpCell(cell: Cell, pubids: => Vector[Pubid]) =
        renderAsyncEditorOrValue(
          cell,
          pw.implicationList(pubids))

      // TODO Much much overlap with Table.CellProps
      // TODO Test that this applies applicability
      // TODO Test can't edit dead req
      def renderRowData(row: Row): TagMod =
        row match {

          case Row.CustomField(f: CustomField.Text) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.customTextField(f.id)(req).fold(emptyRow)(w => w))

          case Row.Code =>
            renderAsyncEditorOrValue(
              Cell.Code,
              pw.flatReqCodes(data.codes))

          case Row.ReqType =>
            renderAsyncEditorOrValue(
              Cell.ReqType,
              pw.reqTypeFull(req.reqTypeId)) // ---- Note for refactoring: reqTypeFull differs from how ReqTable does it

          case Row.Tags =>
            renderAsyncEditorOrValue(
              Cell.Tags,
              pw.tagList(data.generalTags))

          case Row.CustomField(f: CustomField.Tag) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.tagList(data.customTags(f.id)))

          case Row.Implications =>
            def one(cell: Cell) =
              renderImpCell(cell, data.generalImps(cell.implicationDirection))
            <.div(
              *.generalImpsCont,
              <.div(
                *.generalImpsSide,
                one(Cell.ImplicationSrc)),
              <.div(
                *.generalImpsMiddle,
                s"→ $pubidText →"),
              <.div(
                *.generalImpsSide,
                one(Cell.ImplicationTgt)))

          case Row.ImplicationGraph =>
            ImplicationGraph.Props.fromProject(req.id, data.filterDead, project, webWorker).render

          case Row.CustomField(f: CustomField.Implication) =>
            renderImpCell(Cell.CustomField(f.id), data.customImps(f))

          case Row.UseCaseStepsN => val d = data.useCaseData.get; renderStepTree(d, d.stepsN)
          case Row.UseCaseStepsA => val d = data.useCaseData.get; renderStepTree(d, d.stepsA)
          case Row.UseCaseStepsE => val d = data.useCaseData.get; renderStepTree(d, d.stepsE)

          case Row.StepGraph =>
            val ucId = data.useCaseData.get.uc.id
            UseCaseStepFlowGraph.Props(ucId, project.reqs.useCases, webWorker).render

          case Row.DeletionReason =>
            RenderDeletionReason.req(project, pw, req)

          case Row.Life =>
            data.live match {
              case Live =>
                TagMod(
                  UiText.Life.live + ".",
                  <.button(
                    ^.onClick --> delete(req.id),
                    UiText.Life.delete))

              case Dead =>
                TagMod(
                  UiText.Life.dead + ".",
                  req.allowLiveChange(project.config.reqTypes).option(
                    <.button(
                      ^.onClick --> restore(req.id),
                      UiText.Life.restore)))
            }
        }

      def renderStepTree(ucData: UseCaseData, stepData: UseCaseStepTree.StepData) = {
        val renderBody: UseCaseStepTree.RenderBodyFn = (id, live, textAndFlow) =>
          renderAsyncEditorOrValue(
            Cell.UseCaseStep(id),
            pw.useCaseStep(live, textAndFlow))

        UseCaseStepTree.Props(
          ucData.uc, stepData, data.filterDead, project.reqs.useCases.stepFlow, renderBody, state.async, runCmd)
          .render
      }

      def renderFilterDead: ReactNode =
        data.live match {
          case Live => filterDeadCheckbox(props.filterDead.value)
          case Dead => showDeadFakeCheckbox
        }

      <.div(
        renderHeader,
        renderFilterDead,
        renderRows)
    }

    def runActionNoAsync(cmd: UpdateContentCmd): Callback =
      updateIO(cmd, TCB.Success.nop, f => TCB.Failure(Callback.alert(f)))

    def delete(id: ReqId): Callback =
      CallbackTo {
        def run(cmd: UpdateContentCmd): Callback =
          runActionNoAsync(cmd) >> clearModal

        import Px.AutoValue._
        val props1 = DeletionForm.initProps1(pxProject, NonEmptySet one id, Set.empty)
        val props = DeletionForm.makeProps(props1, pxProjectWidgetsNoCtx, pxPlainTextNoCtx, pxTextSearch, run, clearModal)
        Some(Modal(DeletionForm.Component(props)))
      } >>= setModal

    def restore(id: ReqId): Callback =
      runActionNoAsync(UpdateContentCmd.RestoreContent(Set(id), Set.empty))

  } // Backend
}
