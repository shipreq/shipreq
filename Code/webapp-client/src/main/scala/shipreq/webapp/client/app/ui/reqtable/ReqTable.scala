package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import shipreq.base.util.Px
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.util._

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

    def updateCell(cmd: Cell.SetCmd): State = {
      val r1 = cellStates(cmd.row)
      val r2 = cmd.cellState.fold(r1 - cmd.col)(r1.updated(cmd.col, _))
      copy(cellStates = cellStates.updated(cmd.row, r2))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Backend($: BackendScope[Project, State]) {

    val setViewSettings = ReusableFn.modStateIO($)(_.updateVS)
    val setFocus        = ReusableFn.modStateIO($)(_.updateFocus)
    val setCell         = ReusableFn.modStateIO($)(_.updateCell)

    val project      = Px.thunkM($.props).reuseR
    val viewSettings = Px.thunkM($.state.viewSettings).reuseR

    val vsCols     = viewSettings map (_.columns)
    val colName    = project map Column.NameResolver.byProject
    val vsEditor   = colName map ViewSettingsEditor.apply
    val plainText  = project map PlainText.apply
    val textSearch = Px.apply2(project, plainText)(TextSearch.apply)
    val widgets    = Px.apply2(project, plainText)(ProjectWidgets.apply)
    val colRnd     = Px.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds    = Px.apply2(vsCols, colRnd)(_ map _.apply)
    val rows       = Px.apply3(viewSettings, project, plainText)(Logic.rowsForTable(_, _, _).toVector)
    val ces        = new ColumnEditors(project, plainText, widgets, textSearch, setCell)
    val content    = Px.apply2(colRnds, rows)(Table.Content(_, _, ces))

    def render = {
      import Px.AutoValue._
      Px.refresh(project, viewSettings)

      val s = $.state

      val focusV        = setFocus.extvar(s.focus)
      val viewSettingsV = setViewSettings.extvarR(viewSettings, Reusable.byRef)

      <.div(
        vsEditor(viewSettingsV),
        Table.Component(Table.Props(project, content, s.cellStates, focusV)))
    }
  }
}
