package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.{CreateContentFn, CreateContentCmd, UpdateContentFn, UpdateContentCmd}
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.{Changes, ChangeListener, ClientData}
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, TCB}
import shipreq.webapp.client.protocol.ClientProtocol
import edit.ColumnEditors

object ReqTable {

  val Component =
    ReactComponentB[Props]("WIP")
      .initialState_P(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(ChangeListener.update[State](c => _.recvChanges(c)).install(_.cd))
      .build

  case class Props(cd: ClientData, cp: ClientProtocol,
                   createContentFn: CreateContentFn.Instance,
                   updateContentFn: UpdateContentFn.Instance,
                   fd: FilterDead) {
    def component = Component(this)
  }

  def initialState(p: Props): State =
    State(p.cd.project,
      ViewSettings.default.copy(filterDead = p.fd),
      FilterEditor.initialState,
      CreationInterface.initState,
      Cell.emptyTableState)

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   creation    : CreationInterface.State,
                   cellStates  : Cell.TableState) {

    def recvChanges(changes: Changes): State =
      copy(project = changes.p2) // TODO This obviously affects other things

    def updateVS(newVS: ViewSettings): State =
      copy(viewSettings = newVS)

    def updateCell(loc: Cell.Loc, state: Cell.State): State =
      copy(cellStates = cellStates.set(loc, state))

    def filterFailure(s: FilterEditor.State): State =
      copy(filter = s)

    def filterSuccess(fs: (FilterEditor.State, Option[FilterAst])): State =
      updateVS(viewSettings.copy(filter = fs._2)).copy(filter = fs._1)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {

    val setViewSettings = ReusableFn($).modState.endoCall(_.updateVS)
    val setCreation     = $ zoomL State.creation

    val project      = Px.thunkM($.state.project)
    val viewSettings = Px.thunkM($.state.viewSettings)
    val filterState  = Px.thunkM($.state.filter)

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

    val modTable: Cell.ModTable = ReusableFn(loc => s => $.modState(_.updateCell(loc, s)))
    // TODO OMG THE COPY-AND-PASTE!
    // TODO Too much repetition of (? => Events) calls
    val createIO: (CreateContentCmd, TCB.Success, String => TCB.Failure) => Callback = (i, sio, fio) => {
      val p = $.props
      import p._
      val io = cp.call(createContentFn)(i,
        sio << cd.applyEvents(_),
        f => cp.consumeGenericFailure(f) >> fio(cp.genericFailureToText(f)))
      //IO(println(s"Fake-sending: $i")) >> io
      io
    }
    val saveIO: (UpdateContentCmd, TCB.Success, TCB.Failure) => Callback = (i, sio, fio) => {
      val p = $.props
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

    def render = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings, filterState)
      val s = $.state

      val customFields = s.project.config.fields.customFields

      val vsProps = ViewSettingsEditor.Props(colName, customFields, vsVar, filterEditor)

      val creationProps = CreationInterface.Props(createIO, s.creation)

      val tableProps = Table.Props(
        project, rows, colRnds, colEditors, s.cellStates)

      <.div(
        ViewSettingsEditor.Component(vsProps),
        creationInterface.Component(creationProps),
        StatsSummary(stats),
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
