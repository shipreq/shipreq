package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.{\/-, -\/}
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.webapp.base.protocol.{CreateContentFn, CreateContentCmd, UpdateContentFn, UpdateContentCmd}
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{FilterAst, FilterSpec}
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.{Changes, ChangeListener, ClientData}
import shipreq.webapp.client.app.ui.{Selection, ProjectWidgets}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, TCB}
import shipreq.webapp.client.protocol.ClientProtocol
import edit.ColumnEditors

object ReqTable {

  val Component =
    ReactComponentB[Props]("ReqTable")
      .initialState_P(initialState)
      .renderBackend[Backend]
      .configure(ChangeListener.update[State](c => _.recvChanges(c)).install(_.cd))
      .componentWillReceiveProps(($, p) => $.backend.willReceiveProps($.props, p))
      .build

  case class Props(cd             : ClientData,
                   cp             : ClientProtocol,
                   createContentFn: CreateContentFn.Instance,
                   updateContentFn: UpdateContentFn.Instance,
                   fd             : FilterDead,
                   filterSpec     : Option[FilterSpec]) {
    def component = Component(this)
  }

  def initialState(p: Props): State = {
    val proj = p.cd.project
    var s = State(proj,
      ViewSettings.default(p.fd),
      FilterEditor.initialState,
      Selection.empty,
      CreationInterface.initState,
      Cell.emptyTableState)
    p.filterSpec.foreach(f => s = s setFilterSpec f)
    s
  }

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   selection   : RowSelection,
                   creation    : CreationInterface.State,
                   cellStates  : Cell.TableState) {

    def recvChanges(changes: Changes): State =
      copy(project = changes.p2) // TODO This obviously affects other things
      // TODO A custom field removal/addition should affect ViewSettings

    def updateCell(loc: Cell.Loc, state: Cell.State): State =
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
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {

    val ST = ReactS.FixCB[State]

    def willReceiveProps(oldProps: Props, nextProps: Props): Callback = {
      val updateFD =
        if (oldProps.fd ≟ nextProps.fd) ST.nop else
          ST.modT(State.viewSettings.modify(_ setFilterDead nextProps.fd))

      val updateFS =
        nextProps.filterSpec.fold(ST.nop)(fs =>
          ST.modT(_ setFilterSpec fs))

      $.runState(updateFD >> updateFS)
    }

    val setViewSettings = ReusableFn($ zoomL State.viewSettings).setState
    val modViewSettings = ReusableFn($ zoomL State.viewSettings).modState
    val setSortCriteria = ReusableFn($ zoomL State.sortCriteria).setState
    val setSelection    = ReusableFn($ zoomL State.selection).setState
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
    val selVis     = for {rs <- rows; s <- selection} yield s visible rs.foldLeft(Set.empty[Row.SourceId])(_ + _.sourceId)

    val sortEditorProps =
      for {
        vs  <- viewSettings
        nr  <- colName
      } yield SortEditor.Props(vs.order, setSortCriteria, nr)

    val modTable: Cell.ModTable = ReusableFn(loc => (s, cb) => $.modState(_.updateCell(loc, s), cb))
    // TODO OMG THE COPY-AND-PASTE!
    // TODO Too much repetition of (? => Events) calls
    val createIO: (CreateContentCmd, TCB.Success, String => TCB.Failure) => Callback =
      (i, sio, fio) => $.props >>= { p =>
        import p._
        val io = cp.call(createContentFn)(i,
          sio << cd.applyEvents(_),
          f => cp.consumeGenericFailure(f) >> fio(cp.genericFailureToText(f)))
        //IO(println(s"Fake-sending: $i")) >> io
        io
      }
    val saveIO: (UpdateContentCmd, TCB.Success, TCB.Failure) => Callback =
      (i, sio, fio) => $.props >>= { p =>
        import p._
        val io = cp.call(updateContentFn)(i,
          sio << cd.applyEvents(_),
          cp.consumeGenericFailure(_) >> fio)
        //IO(println(s"Fake-sending: $i")) >> io
        io
      }
    val colEditors = new ColumnEditors(project, plainText, widgets, textSearch, modTable, saveIO)

    val filterProps: FilterEditor.State => FilterEditor.Props = {
      import FilterEditor._
      val onFailure: OnFailure = ReusableFn(s => $.modState(_ filterFailure s))
      val onSuccess: OnSuccess = ReusableFn(i => $.modState(_.filterSuccess(i._1, i._2)))
      s => FilterEditor.Props(project.value(), onFailure, onSuccess, s)
    }

    val filterEditor: Px[ReusableVal[ReactElement]] =
      filterState map filterProps map ReusableVal.renderComponent(FilterEditor.Component)

    val creationInterface = new CreationInterface(setCreation, project, plainText, widgets, textSearch)

    def render(s: State) = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings, filterState, selection)

      val vsProps = ViewSettingsEditor.Props(colName, s.project.config, vsVar, filterEditor)

      val creationProps = CreationInterface.Props(createIO, s.creation)

      val tableProps = Table.Props(
        project, rows, colName, colRnds, colEditors, s.cellStates, selVis, setSelection, modViewSettings)

      <.div(
        ViewSettingsEditor.Component(vsProps),
        creationInterface.Component(creationProps),
        StatsSummary(stats),
        SortEditor.Component(sortEditorProps),
        Table.Component(tableProps))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  val StatsSummary = ReactComponentB[TableStats]("Stats")
    .render_P(stats =>
      <.div(
        *.statsSummary,
        stats.summary))
    .configure(shouldComponentUpdate)
    .build
}
