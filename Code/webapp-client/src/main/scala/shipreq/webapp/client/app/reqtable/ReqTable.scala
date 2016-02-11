package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{\/-, -\/}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{FilterAst, FilterSpec}
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.{Changes, ClientData}
import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.data.FilterDead
import shipreq.webapp.client.feature._
import shipreq.webapp.client.protocol.{ClientProtocol, ServerCall}
import shipreq.webapp.client.widgets.high.ProjectWidgets

object ReqTable extends StaticPropComponent.Template("ReqTable") {
  override protected def configureBackend = new Backend(_, _)
  override protected def configureRender  = _.renderBackend
  override protected def configure = _.configure(
    Listenable.install(_.static.cd, $ => (c: Changes) => $.props.static.state_$.modState(_ recvChanges c)))

  case class StaticProps(cd              : ClientData,
                         cp              : ClientProtocol,
                         createContentFn : CreateContentFn.Instance,
                         updateContentFn : UpdateContentFn.Instance,
                         pxPlainText     : Px[PlainText.ForProject],
                         pxTextSearch    : Px[TextSearch],
                         pxProjectWidgets: Px[ProjectWidgets],
                         reqDetailRC     : RouterCtl[ExternalPubid],
                         state_$         : CompState.Access[State])

  override type DynamicProps = State

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   selection   : RowSelection,
                   creation    : CreationInterface.State,
                   editStates  : ContentEditorFeature.TwoD.State[Row.SourceId, Column, Column],
                   asyncStates : AsyncState.TableState,
                   previewState: Preview.State,
                   modal       : Modal.State) {

    def recvChanges(changes: Changes): State =
      copy(project = changes.p2) // TODO This obviously affects other things
      // TODO A custom field removal/addition should affect ViewSettings

    def filterFailure(s: FilterEditor.State): State =
      copy(filter = s)

    def filterSuccess(fs: (FilterEditor.State, Option[FilterAst])): State = {
      val vs = viewSettings.copy(filter = fs._2)
      copy(viewSettings = vs, filter = fs._1)
    }

    def setFilterSpec(fs: FilterSpec): State = {
      val txt = FilterSpec toText fs
      FilterAst(project, fs) match {
        case \/-(ast) => filterSuccess(FilterEditor.State(txt, None), Some(ast))
        case -\/(err) => filterFailure(FilterEditor.State(txt, Some(err)))
      }
    }

    def setFilterDead(fd: FilterDead): State =
      if (viewSettings.filterDead ==* fd)
        this
      else
        State.viewSettings.modify(_ setFilterDead fd)(this)
  }

  object State {
    val sortCriteria = viewSettings ^|-> ViewSettings.order

    def init(cd: ClientData, fd: FilterDead, filterSpec: Option[FilterSpec]): State = {
      val proj = cd.project()
      var s = State(proj,
        ViewSettings.default(fd),
        FilterEditor.initialState,
        Selection.empty,
        CreationInterface.initState,
        ContentEditorFeature.TwoD.State.init,
        AsyncState.initState,
        PreviewFeature.initState,
        Modal.none)
      filterSpec.foreach(f => s = s setFilterSpec f)
      s
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend(SP: StaticProps, $: BackendScope) extends OnUnmount {
    import SP._
    import cd.pxProject

    // TODO Move these to scalajs-react?
    private def reusableStateFn[A](f: A => State => State): A ~=> Callback =
      ReusableFn(a => state_$.modState(f(a)))

    private def reusableSetState[A](l: Lens[State, A]): A ~=> Callback =
      reusableStateFn(l.set)

    private def reusableModState[A](l: Lens[State, A]): (A => A) ~=> Callback =
      reusableStateFn(l.modify)

    val setViewSettings = reusableSetState(State.viewSettings)
    val modViewSettings = reusableModState(State.viewSettings)
    val setSortCriteria = reusableSetState(State.sortCriteria)
    val setSelection    = reusableSetState(State.selection)
    val setModal        = reusableSetState(State.modal)
    val setCreation     = state_$ zoomL State.creation

    val pxViewSettings = Px.bsMP($).propsM(_.viewSettings)
    val pxFilterState  = Px.bsMP($).propsM(_.filter)
    val pxSelection    = Px.bsMP($).propsM(_.selection)

    val pxVsVar   = pxViewSettings map (ReusableVar(_)(setViewSettings))
    val pxVsCols  = pxViewSettings map (_.columns)
    val pxColName = pxProject map Column.NameResolver.byProject reuse
    val pxColRnd  = Px.apply3(pxProject, pxColName, pxProjectWidgets)(new ColumnRenderers(_, _, _))
    val pxColRnds = Px.apply2(pxVsCols, pxColRnd)(_ map _.apply)
    val pxRows    = Px.apply4(pxViewSettings, pxProject, pxPlainText, pxTextSearch)(Logic.rowsForTable).map(_.toVector)
    val pxStats   = Px.apply3(pxViewSettings, pxProject, pxRows)(Logic.stats)

    val pxRowsWithAsyncWholeRowStatuses: Px.ThunkM[Set[Row.SourceId]] =
      Px.bsMP($).propsM(_.asyncStates.iterator
        .filter(_._2.rowStatus.isDefined)
        .map(_._1)
        .toSet)

    val pxVisibleSelection =
      for {
        rs <- pxRows
        s  <- pxSelection
        wr <- pxRowsWithAsyncWholeRowStatuses
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val pxSortEditorProps =
      for {
        vs  <- pxViewSettings
        nr  <- pxColName
      } yield SortEditor.Props(vs.order, setSortCriteria, nr)

    val createIO: ServerCall[CreateContentCmd] =
      ServerCall.to(createContentFn, cp, cd)

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    val filterProps: FilterEditor.State => FilterEditor.Props = {
      import FilterEditor._
      val onFailure: OnFailure = ReusableFn(s => state_$.modState(_ filterFailure s))
      val onSuccess: OnSuccess = ReusableFn(i => state_$.modState(_.filterSuccess(i._1, i._2)))
      s => FilterEditor.Props(pxProject.value(), onFailure, onSuccess, s)
    }

    val pxFilterEditor: Px[ReusableVal[ReactElement]] =
      pxFilterState map filterProps map ReusableVal.renderComponent(FilterEditor.Component)

    val asyncFeature = AsyncState.Feature(state_$)(State.asyncStates)

    val previewFeature = new PreviewFeature(state_$, State.previewState)

    val contentEditorFeature = {
      import ContentEditorFeature._

      val static = Static(
        state_$, previewFeature, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch, updateIO)

      val edit: Row => Column => Option[Editor[FocusId]] = row => col => {
        @inline implicit def autoSome[P](e: Editor[P]): Option[Editor[P]] = Some(e)
        @inline def focusId = FocusId.AtCell(row.sourceId, col)

        def imps(row: GenericReqRow, rowLens: monocle.Optional[Row, Vector[Pubid]]) =
          rowLens.getOption(row).map(pubids =>
            Editor.ImplicationsAll(row.req, Column.implicationDirection(col), pubids))

        row match {
          case r: GenericReqRow => col match {
            case Column.Code                                              => Editor.ReqCodesForReq(r.req)
            case Column.Title                                             => Editor.GenericReqTitle(r.req, focusId)
            case Column.Tags                                              => Editor.Tags(r.req, None)
            case Column.ReqType                                           => Editor.ReqType(r.req)
            case Column.ImplicationSrc                                    => imps(r, Row.implicationSrc)
            case Column.ImplicationTgt                                    => imps(r, Row.implicationTgt)
            case Column.CustomField(id: CustomField.Text       .Id, Live) => Editor.CustomTextField(r.req, id, focusId)
            case Column.CustomField(id: CustomField.Tag        .Id, Live) => Editor.Tags(r.req, Some(id))
            case Column.CustomField(id: CustomField.Implication.Id, Live) => Editor.ImplicationsCustomField(r.req, id)
            case Column.Pubid
               | Column.DeletionReason
               | Column.CustomField(_, Dead)                              => None
          }

          case r: ReqCodeGroupRow => col match {
            case Column.Code              => Editor.ReqCodeForReqCodeGroup(r.group, r.reqCode)
            case Column.Title             => Editor.ReqCodeGroupTitle(r.group, focusId)
            case Column.Pubid
               | Column.ReqType
               | Column.Tags
               | Column.ImplicationSrc
               | Column.ImplicationTgt
               | Column.DeletionReason
               | Column.CustomField(_, _) => None
          }
        }
      }

      TwoD.Feature.withKeyId(static, asyncFeature, (_: Row).sourceId)(State.editStates, edit)
    }

    val creationInterface =
      new CreationInterface(setCreation, previewFeature, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch)

    // -----------------------------------------------------------------------------------------------------------------
    def render(s: DynamicProps): ReactElement = {
      Px.refresh(pxViewSettings, pxFilterState, pxSelection, pxRowsWithAsyncWholeRowStatuses)
      import Px.AutoValue._

      val cfg = s.project.config

      def vsProps = ViewSettingsEditor.Props(pxColName, cfg, pxVsVar, pxFilterEditor)

      def creationProps = CreationInterface.Props(createIO, s.creation, s.previewState)

      def tableProps = Table.Props(
        pxProject, pxRows, pxColName, pxColRnds, contentEditorFeature, s.editStates, s.asyncStates, pxVisibleSelection,
        modViewSettings)

      def selCtrlProps = SelectionCtrls.Props(
        pxVisibleSelection, cfg, pxRows, setModal, pxProject, pxProjectWidgets, pxPlainText, pxTextSearch, updateIO,
        asyncFeature)

      def mainScreen =
        <.div(
          ViewSettingsEditor.Component(vsProps),
          creationInterface.Component(creationProps),
          StatsSummary(pxStats),
          SelectionCtrls.Component(selCtrlProps),
          SortEditor.Component(pxSortEditorProps),
          Table.Component(tableProps))

      s.modal renderOrElse mainScreen
    }
  }

  // ===================================================================================================================

  val StatsSummary = ReactComponentB[TableStats]("Stats")
    .render_P(stats =>
      <.div(
        *.statsSummary,
        stats.summary))
    .configure(shouldComponentUpdate)
    .build
}
