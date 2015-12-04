package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{\/-, -\/}
import scalaz.syntax.equal._
import shipreq.base.util.PolyMap
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.filter.{FilterAst, FilterSpec}
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.{Changes, ChangeListener, ClientData}
import shipreq.webapp.client.app.ui.{Modal, ProjectWidgets, Selection}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.FilterDead
import shipreq.webapp.client.lib.ui.feature._
import shipreq.webapp.client.protocol.ClientProtocol
import edit.ColumnEditors

object ReqTable {

  val Component =
    ReactComponentB[Props]("ReqTable")
      .initialState_P(State.init)
      .renderBackend[Backend]
      .configure(ChangeListener.update[State](c => _.recvChanges(c)).install(_.cd))
      .componentWillReceiveProps(i => i.$.backend.willReceiveProps(i.$.props, i.nextProps))
      .build

  case class Props(cd             : ClientData,
                   cp             : ClientProtocol,
                   createContentFn: CreateContentFn.Instance,
                   updateContentFn: UpdateContentFn.Instance,
                   fd             : FilterDead,
                   filterSpec     : Option[FilterSpec]) {
    def component = Component(this)
  }

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   selection   : RowSelection,
                   creation    : CreationInterface.State,
                   editStates  : EditState.Table,
                   asyncStates : AsyncState.TableState,
                   //focusData  : PreviewFeature.FocusData[],
                   cellStates  : Cell.TableState,
                   modal       : Modal.State) {

    def recvChanges(changes: Changes): State =
      copy(project = changes.p2) // TODO This obviously affects other things
      // TODO A custom field removal/addition should affect ViewSettings

    def updateCellLoc(loc: Cell.Loc, state: Cell.State): State =
      copy(cellStates = cellStates.set(loc, state))

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
  }

  object State {
    val sortCriteria = viewSettings ^|-> ViewSettings.order

    def init(p: Props): State = {
      val proj = p.cd.project
      var s = State(proj,
        ViewSettings      .default(p.fd),
        FilterEditor      .initialState,
        Selection         .empty,
        CreationInterface .initState,
        EditState         .empty,
        AsyncState        .initState,
        Cell              .emptyTableState,
        Modal             .none)
      p.filterSpec.foreach(f => s = s setFilterSpec f)
      s
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {

    val ST = ReactS.FixCB[State]

    def willReceiveProps(oldProps: Props, nextProps: Props): Callback = {
      val updateFD =
        if (oldProps.fd ==* nextProps.fd) ST.nop else
          ST.modT(State.viewSettings.modify(_ setFilterDead nextProps.fd))

      val updateFS =
        nextProps.filterSpec.fold(ST.nop)(fs =>
          ST.modT(_ setFilterSpec fs))

      $.runState(updateFD >> updateFS)
    }

    val setViewSettings = ReusableFn($ zoomL State.viewSettings).setState
    val modViewSettings = ReusableFn($ zoomL State.viewSettings).modState
    val setSortCriteria = ReusableFn($ zoomL State.sortCriteria).setState
    val setSelection    = ReusableFn($ zoomL State.selection   ).setState
    val setModal        = ReusableFn($ zoomL State.modal       ).setState
    val setCreation     = $ zoomL State.creation

    val project      = Px.bs($).stateM(_.project)
    val viewSettings = Px.bs($).stateM(_.viewSettings)
    val filterState  = Px.bs($).stateM(_.filter)
    val selection    = Px.bs($).stateM(_.selection)

    val vsVar      = viewSettings map (ReusableVar(_)(setViewSettings))
    val vsCols     = viewSettings map (_.columns)
    val colName    = project map Column.NameResolver.byProject reuse
    val plainText  = project map PlainText.apply
    val textSearch = Px.apply2(project, plainText)(TextSearch.apply)
    val widgets    = Px.apply2(project, plainText)(ProjectWidgets.apply)
    val colRnd     = Px.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds    = Px.apply2(vsCols, colRnd)(_ map _.apply)
    val rows       = Px.apply4(viewSettings, project, plainText, textSearch)(Logic.rowsForTable).map(_.toVector)
    val stats      = Px.apply3(viewSettings, project, rows)(Logic.stats)

    val wholeRowsWithState: Px.ThunkM[Set[Row.SourceId]] =
      Px.bs($).stateM(_.cellStates.all.iterator
        .filter(_._2 match {
          case _: Cell.RowState.Cells | Cell.RowState.Empty => false
          case _: Cell.RowState.WholeRow => true
        })
        .map(_._1)
        .toSet)

    val visibleSelection =
      for {
        rs <- rows
        s  <- selection
        wr <- wholeRowsWithState
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val sortEditorProps =
      for {
        vs  <- viewSettings
        nr  <- colName
      } yield SortEditor.Props(vs.order, setSortCriteria, nr)

    val cellSetLocState: Cell.SetLocState =
      ReusableFn(loc => (s, cb) => $.modState(_.updateCellLoc(loc, s), cb))

    val cellModifyFn: Cell.ModifyFn =
      ReusableFn(f => $.modState(State.cellStates modify f))

    private def callServer[I, F <: (I =>|=> VerifiedEvents)](remoteFn: Props => RemoteFn.InstanceFor[F]): CallServer[I] =
      (i, sio, fio) => $.props >>= (p =>
        p.cp.call(remoteFn(p))(
          i,
          s => p.cd.applyEventsS(s) >> sio,
          f => p.cp.consumeGenericFailure(f) >> fio(p.cp.genericFailureToText(f))))

    val createIO: CallServer[CreateContentCmd] =
      callServer(_.createContentFn)

    val saveIO: CallServer[UpdateContentCmd] =
      callServer(_.updateContentFn)

    val filterProps: FilterEditor.State => FilterEditor.Props = {
      import FilterEditor._
      val onFailure: OnFailure = ReusableFn(s => $.modState(_ filterFailure s))
      val onSuccess: OnSuccess = ReusableFn(i => $.modState(_.filterSuccess(i._1, i._2)))
      s => FilterEditor.Props(project.value(), onFailure, onSuccess, s)
    }

    val filterEditor: Px[ReusableVal[ReactElement]] =
      filterState map filterProps map ReusableVal.renderComponent(FilterEditor.Component)

    val cellEditors: CellEditors =
      new CellEditorsImpl[State]($,
                                 State.editStates,
                                 State.asyncStates,
                                 project,
                                 plainText,
                                 widgets,
                                 textSearch,
                                 saveIO)

    val creationInterface = new CreationInterface(setCreation, project, plainText, widgets, textSearch)

    // -----------------------------------------------------------------------------------------------------------------
    def render(s: State): ReactElement = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings, filterState, selection, wholeRowsWithState)

      val cfg = s.project.config

      val vsProps = ViewSettingsEditor.Props(colName, cfg, vsVar, filterEditor)

      val creationProps = CreationInterface.Props(createIO, s.creation)

      val tableProps = Table.Props(
        project, rows, colName, colRnds, cellEditors, s.editStates,s.asyncStates, visibleSelection, modViewSettings)

      val selCtrlProps = SelectionCtrls.Props(
        visibleSelection, cfg, rows, setModal, project, widgets, plainText, textSearch, saveIO, cellModifyFn)

      def mainScreen =
        <.div(
          ViewSettingsEditor.Component(vsProps),
          creationInterface.Component(creationProps),
          StatsSummary(stats),
          SelectionCtrls.Component(selCtrlProps),
          SortEditor.Component(sortEditorProps),
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
