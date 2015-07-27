package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.effect.IO
import shipreq.webapp.base.protocol.{UpdateContentFn, UpdateContentCmd}
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, TIO}
import shipreq.webapp.client.protocol.ClientProtocol
import edit.ColumnEditors

object ReqTable {

  val Component =
    ReactComponentB[Props]("WIP")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  case class Props(cd: ClientData, cp: ClientProtocol, remote: UpdateContentFn.Instance, fd: FilterDead) {
    def component = Component(this)
  }

  def initialState(p: Props): State =
    State(p.cd.project,
      ViewSettings.default.copy(filterDead = p.fd),
      FilterEditor.initialState,
      CreationInterface.initState,
      Cell.emptyTableState,
      None)

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   filter      : FilterEditor.State,
                   creation    : CreationInterface.State,
                   cellStates  : Cell.TableState,
                   focus       : Option[Table.Focus]) {

    def updateVS(newVS: ViewSettings): State = {
      val newFocus   = focus // TODO
      copy(viewSettings = newVS, focus = newFocus)
    }

    def updateFocus(newFocus: Option[Table.Focus]): State =
      copy(focus = newFocus)

    def updateCell(loc: Cell.Loc, state: Cell.State): State =
      copy(cellStates = cellStates.set(loc, state))

    def filterFailure(s: FilterEditor.State): State =
      copy(filter = s)

    def filterSuccess(fs: (FilterEditor.State, Option[FilterAst])): State =
      updateVS(viewSettings.copy(filter = fs._2)).copy(filter = fs._1)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Props, State]) {

    val setViewSettings = ReusableFn($).modStateIO.endoCall(_.updateVS)
    val setFocus        = ReusableFn($).modStateIO.endoCall(_.updateFocus)
    val setCreation     = ReusableFn($ _setStateL State.creation)

    val project      = Px.thunkM($.state.project)
    val viewSettings = Px.thunkM($.state.viewSettings)
    val filterState  = Px.thunkM($.state.filter)

    val vsVar      = viewSettings map (ReusableVar(_)(setViewSettings))
    val vsCols     = viewSettings map (_.columns)
    val colName    = project map Column.NameResolver.byProject
    val vsEditor   = colName map ViewSettingsEditor.apply
    val plainText  = project map PlainText.apply
    val textSearch = Px.apply2(project, plainText)(TextSearch.apply)
    val widgets    = Px.apply2(project, plainText)(ProjectWidgets.apply)
    val colRnd     = Px.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds    = Px.apply2(vsCols, colRnd)(_ map _.apply)
    val rows       = Px.apply4(viewSettings, project, plainText, textSearch)(Logic.rowsForTable).map(_.toVector)
    val stats      = Px.apply3(viewSettings, project, rows)(Logic.stats)

    val modTable: Cell.ModTable = ReusableFn($).modStateIO.endoCall2(_.updateCell)
    val saveIO: (UpdateContentCmd, TIO.Success, TIO.Failure) => IO[Unit] = (i, sio, fio) => {
      val p = $.props
      import p._
      val io = cp.call(remote)(i,
        sio << cd.applyEvents(_),
        cp.consumeGenericFailure(_) >> fio.io)
      //IO(println(s"Fake-sending: $i")) >> io
      io
    }
    val colEditors = new ColumnEditors(project, plainText, widgets, textSearch, modTable, saveIO)

    val filterComp = FilterEditor.component(
      FilterEditor.StaticProps(project,
        s      => $.modStateIO(_ filterFailure s),
        (a, b) => $.modStateIO(_.filterSuccess(a, b))))

    val filterEditor = filterState.map(ReusableVal renderComponent filterComp)

    def render = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings, filterState)
      val s = $.state

      val vsProps = ViewSettingsEditor.Props(vsVar, filterEditor)

      val creationProps = CreationInterface.Props(ReusableVar(s.creation)(setCreation))

      val tableProps = Table.Props(
        project, rows, colRnds, colEditors, s.cellStates, ReusableVar(s.focus)(setFocus))

      <.div(
        vsEditor(vsProps),
        CreationInterface.Component(creationProps),
        StatsSummary(stats),
        Table.Component(tableProps))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  val StatsSummary = ReactComponentB[TableStats]("Stats")
    .render(stats =>
    <.div(
      *.statsSummary,
      stats.summary))
    .configure(shouldComponentUpdate)
    .build
}
