package shipreq.webapp.client.project.app.reqdetail

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{UpdateContentCmd, UpdateContentFn}
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.data._
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.{BaseStyles, EditTheme}
import shipreq.webapp.client.base.ui.semantic.Header
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.{FilterDeadButton, LifeButton}
import shipreq.webapp.client.project.widgets.{DeletionForm, ImplicationGraph, ProjectWidgets, UseCaseStepFlowGraph}
import ExternalPubid.LookupFailure

object ReqDetail {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[DynamicProps]("ReqDetail")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .build

  case class StaticProps(cd                   : ClientData,
                         cp                   : ClientProtocol,
                         reqDetailRC          : RouterCtl[ExternalPubid],
                         webWorker            : WebWorkerClient,
                         updateContentFn      : UpdateContentFn.Instance,
                         pxPlainTextNoCtx     : Px[PlainText.ForProject],
                         pxTextSearch         : Px[TextSearch],
                         pxProjectWidgetsNoCtx: Px[ProjectWidgets])

  case class DynamicProps(extPubid  : ExternalPubid,
                          filterDead: StateSnapshot[FilterDead],
                          reqProps  : ReqId => ReqProps,
                          editorUCS : EditorFeature.ReadWrite.ForUseCaseSteps,
                          state     : StateSnapshot[State])

  case class ReqProps(editor: EditorFeature.ReadWrite.ForReq,
                      async : AsyncFeature.ReadWrite.D1[Cell, String])

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

    val rows: Vector[Row] = {
      val liveFilter = filterDead.filterFnBy((_: Field) live project.config)
      val fields = project.config.fields.fields.filter(f =>
        f.applicable(req.reqTypeId) is Applicable && liveFilter(f))
      fields.foldLeft(Row head filterDead)((q, f) => q ++ Row.fromField(f.fieldId))
    }

    val pubidText = PlainText.pubid(req.pubid, project)

    val codeSet = project.reqCodes.activeReqCodesByReqId(req.id)
    val codes   = MutableArray(codeSet).sortBySchwartzian(PlainText.reqCode).to[List]

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
            .map(project.reqs.need)
            .filter(impFilter)
            .map(_.pubid)))

    val customImps: CustomField.Implication.Id => Vector[Pubid] =
      Memo(fid =>
        sortPubids(customImpLookup(fid)(req.id)))

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final class Backend(SP: StaticProps, $: BackendScope[DynamicProps, Unit]) {
    import SP._
    import cd.pxProject

    val pxFieldNameFn = pxProject.map(Field.nameByIdFromProject)
    val pxExtPubid    = Px.props($).map(_.extPubid).withReuse.manualRefresh
    val pxUpstreamFD  = Px.props($).map(_.filterDead.value).withReuse.manualRefresh

    val pxData: Px[LookupFailure \/ Data] =
      for {
        p <- pxProject
        e <- pxExtPubid
        f <- pxUpstreamFD
      } yield
        e.lookup(p).map(new Data(SP, p, _, f))

    val setFilterDead: FilterDead ~=> Callback =
      Reusable.fn(v => $.props.flatMap(_.filterDead setState v))

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    val runCmd = Reusable.fn[ReqId, Cell, UpdateContentCmd, Callback](
      (reqId, cell, cmd) =>
        $.props >>= (p =>
          p.reqProps(reqId).async.write(cell)((s, f) =>
            updateIO(cmd, s, f))))

    def setModal(modal: Modal.State): Callback =
      $.props >>= (_.state setState modal)

    def clearModal: Callback =
      setModal(Modal.none)

    def renderNotFound(failureReason: String): VdomElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    val emptyRow: VdomElement = <.span

    def render(p: DynamicProps): VdomElement =
      <.main(
        BaseStyles.containerFull,
        p.state.value renderOrElse {
          Px.refresh(pxExtPubid, pxUpstreamFD)
          pxData.value() match {
            case \/-(data)                              => renderDetail(p, data)
            case -\/(LookupFailure.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.extPubid.mnemonic.value} not found.")
            case -\/(LookupFailure.InvalidPos(rt, len)) => renderNotFound(s"${PlainText pubid p.extPubid} not found.")
          }
        })

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private val headerStyle: Live => Header.Style =
      Live.memo(l => Header.Style(Header.Type.H1, other = *.headerText(l)))

    def renderDetail(props: DynamicProps, data: Data): VdomElement = {
      import data.{project, req, pubidText}

      val pw        = data.pxProjectWidgets.value()
      val reqProps  = props.reqProps(req.id)
      val reqEditor = reqProps.editor
      val fieldName = pxFieldNameFn.value()
      val runCmd    = this.runCmd(req.id)

      def renderEditor(editor: EditorFeature.ReadWrite.ForCell, view: => TagMod): TagMod =
        editor.renderOr(TagMod(EditTheme.editableInline(editor.startEdit), view))

      def renderHeader: VdomElement = {
        val hstyle = headerStyle(data.live)

        <.div(*.headerRow,

          <.div(*.headerPubid,
            Header(hstyle, pubidText + ":")),

          <.div(*.headerTitle,
            renderEditor(
              reqEditor(EditorFeature.CellKey.Title),
              Header(hstyle, pw.reqTitle(req)))),

          <.div(*.headerFilterDeadButton,
            FilterDeadButton.whenLive(data.live)(StateSnapshot.withReuse(props.filterDead.value)(setFilterDead))))
      }

      val keyCell = <.th(*.detailTableKey(data.live))

      def renderRows =
        <.table(*.detailTable,
          <.tbody(
            data.rows.toVdomArray(renderRow)))

      def renderRow(row: Row): VdomElement =
        <.tr(
          ^.key := row.key,
          keyCell(renderRowTitle(row)),
          <.td(renderRowData(row)))

      def renderRowTitle(row: Row): VdomNode =
        row match {
          case Row.CustomField(id)  => fieldName(id)
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
          case Row.PastPubids       => UiText.FieldNames.pastPubids
          case Row.Life             => UiText.Life.field
        }

      def renderImpCell(scope: CustomField.Implication.Id \/ Direction, pubids: => Vector[Pubid]) =
        renderEditor(
          reqEditor(EditorFeature.CellKey.Implications(scope)),
          pw.implicationList(pubids))

      // TODO Much much overlap with Table.CellProps
      // TODO Test that this applies applicability
      def renderRowData(row: Row): TagMod = {
        var liveStyle = data.live

        val content: TagMod = row match {

          case Row.CustomField(id: CustomField.Text.Id) =>
            renderEditor(
              reqEditor(EditorFeature.CellKey.CustomTextField(id)),
              pw.customTextField(id)(req).fold(emptyRow)(w => w))

          case Row.Code =>
            renderEditor(
              reqEditor(EditorFeature.CellKey.Code),
              pw.reqCodes(data.codes))

          case Row.ReqType =>
            renderEditor(
              reqEditor(EditorFeature.CellKey.ReqType),
              pw.reqTypeFull(req.reqTypeId)) // ---- Note for refactoring: reqTypeFull differs from how ReqTable does it

          case Row.Tags =>
            renderEditor(
              reqEditor(EditorFeature.CellKey.Tags(None)),
              pw.tagList(data.generalTags))

          case Row.CustomField(id: CustomField.Tag.Id) =>
            renderEditor(
              reqEditor(EditorFeature.CellKey.Tags(Some(id))),
              pw.tagList(data.customTags(id)))

          case Row.Implications =>
            def one(dir: Direction) = renderImpCell(\/-(dir), data.generalImps(dir))
            <.table(*.generalImpsCont,
              <.tbody(
                <.tr(
                  <.td(*.generalImpsSide,
                    one(Backwards)),
                  <.td(*.generalImpsMiddle,
                    s"→ $pubidText →"),
                  <.td(*.generalImpsSide,
                    one(Forwards)))))

          case Row.ImplicationGraph =>
            ImplicationGraph.Props(
              Some(req.id), data.filterDead,
              project.implications, project.reqs, project.config.reqTypes,
              data.pxPlainText.value(),
              reqDetailRC,
              webWorker
            ).render

          case Row.CustomField(id: CustomField.Implication.Id) =>
            renderImpCell(-\/(id), data.customImps(id))

          case Row.UseCaseStepsN => val d = data.useCaseData.get; renderStepTree(d, d.stepsN)
          case Row.UseCaseStepsA => val d = data.useCaseData.get; renderStepTree(d, d.stepsA)
          case Row.UseCaseStepsE => val d = data.useCaseData.get; renderStepTree(d, d.stepsE)

          case Row.StepGraph =>
            val ucId = data.useCaseData.get.uc.id
            UseCaseStepFlowGraph.Props(ucId, project.reqs.useCases, webWorker).render

          case Row.DeletionReason =>
            ProjectWidgets.DeletionReason.forReq(req)(project.config.reqTypes, pw) getOrElse emptyRow

          case Row.Life =>
            liveStyle = Live // When req is dead, user can still Restore it, thus this cell shouldn't appear dead
            data.live match {
              case Live =>
                LifeButton.Delete withStatusOnLeft delete(req.id)
              case Dead =>
                LifeButton.Restore.withStatusOnLeft(
                  req.allowLiveChange(project.config.reqTypes) option restore(req.id))
            }

          case Row.PastPubids =>
            val idS = req.pastPubids(project.reqs.pubids)
            val idV = MutableArray(idS).sortBySchwartzian(DataLogic.pubidSortKeyFn(project.config)).to[Vector]
            pw pastPubids idV
        }

        content(*.detailTableValue(liveStyle))
      }

      def renderStepTree(ucData: UseCaseData, stepData: UseCaseStepTree.StepData) = {
        val renderBody: UseCaseStepTree.RenderBodyFn = (id, live, textAndFlow) =>
          renderEditor(
            props.editorUCS(EditorFeature.CellKey.UseCaseStep(id)),
            pw.useCaseStep(live, textAndFlow))

        UseCaseStepTree.Props(
          ucData.uc,
          stepData,
          data.filterDead,
          project.reqs.useCases.stepFlow,
          renderBody,
          reqProps.async.read,
          runCmd)
          .render
      }

      <.div(
        renderHeader,
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
