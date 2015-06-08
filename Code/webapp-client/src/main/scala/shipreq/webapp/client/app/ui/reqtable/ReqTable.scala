package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.data.DataReusability._
import edit.ColumnEditors

object ReqTable {

  val WIP =
    ReactComponentB[Project]("WIP")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  // -------------------------------------------------------------------------------------------------------------------

  def initialState(p: Project): State =
    State(p, ViewSettings.default, Cell.emptyTableState, None)

  @Lenses
  case class State(project     : Project,
                   viewSettings: ViewSettings,
                   cellStates  : Cell.TableState,
                   focus       : Option[Table.Focus]) {

    def updateVS(newVS: ViewSettings): State = {
      val newFocus   = focus // TODO
      copy(viewSettings = newVS, focus = newFocus)
    }

    def updateFocus(newFocus: Option[Table.Focus]): State =
      copy(focus = newFocus)

    def updateCell(cmd: Cell.SetCmd): State =
      copy(cellStates = cellStates.set(cmd))
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Project, State]) {

    val setViewSettings = ReusableFn($).modStateIO.endoCall(_.updateVS)
    val setFocus        = ReusableFn($).modStateIO.endoCall(_.updateFocus)
    val setCell         = ReusableFn($).modStateIO.endoCall(_.updateCell)

    val project      = Px.thunkM($.state.project)
    val viewSettings = Px.thunkM($.state.viewSettings)

    val vsVar      = viewSettings map (ReusableVar(_)(setViewSettings))
    val vsCols     = viewSettings map (_.columns)
    val colName    = project map Column.NameResolver.byProject
    val vsEditor   = colName map ViewSettingsEditor.apply
    val plainText  = project map PlainText.apply
    val textSearch = Px.apply2(project, plainText)(TextSearch.apply)
    val widgets    = Px.apply2(project, plainText)(ProjectWidgets.apply)
    val colRnd     = Px.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds    = Px.apply2(vsCols, colRnd)(_ map _.apply)
    val rows       = Px.apply3(viewSettings, project, plainText)(Logic.rowsForTable(_, _, _).toVector)
    val stats      = Px.apply3(viewSettings, project, rows)(Logic.stats)
    val colEditors = new ColumnEditors(project, plainText, widgets, textSearch, setCell)

    def render = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings)
      val s = $.state

      val tableProps = Table.Props(
        project, rows, stats, colRnds, colEditors, s.cellStates, ReusableVar(s.focus)(setFocus))

      <.div(
        vsEditor(vsVar),
        Table.Component(tableProps))
    }
  }
}
