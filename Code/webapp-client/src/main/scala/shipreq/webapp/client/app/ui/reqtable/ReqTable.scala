package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import scalaz.effect.IO
import shipreq.base.util.Rx
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.util._

object ReqTable {

  val WIP =
    ReactComponentB[Project]("WIP")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  def initialState(p: Project): State =
    State(p, ViewSettings.default, None)

  case class State(project           : Project,
                   viewSettings      : ViewSettings,
                   focus             : Option[Table.Focus]) {

    def updateVS(newVS: ViewSettings): State = {
      val newFocus   = focus // TODO
      copy(viewSettings = newVS, focus = newFocus)
    }

    def updateFocus(newFocus: Option[Table.Focus]): State =
      copy(focus = newFocus)
  }

  // TODO modStateR can be in util
  def modStateR[S, A]($: BackendScope[_, S])(f: S => A => S): A ~=> IO[Unit] =
    ReusableFn(a => $.modStateIO(s => f(s)(a)))

  final class Backend($: BackendScope[Project, State]) {

    val project      = Rx.thunkM($.props).reuseR
    val viewSettings = Rx.thunkM($.state.viewSettings).reuseR

    val colName  = project map Column.NameResolver.byProject
    val vsEditor = colName map ViewSettingsEditor.apply
    val widgets  = project map (new ProjectWidgets(_))
    val colRnd   = Rx.apply3(project, colName, widgets)(new ColumnRenderers(_, _, _))
    val colRnds  = for {vs <- viewSettings; cr <- colRnd} yield vs.columns map cr.apply // TODO don't care if order changes
    val rows     = for {vs <- viewSettings; p <- project} yield Logic.rowsForTable(vs, p).toVector
    val content  = Rx.apply2(colRnds, rows)(Table.Content)

    val setViewSettings = modStateR($)(_.updateVS)
    val setFocus        = modStateR($)(_.updateFocus)

    def render = {
      import Rx.AutoValue._
      Rx.refresh(project, viewSettings)

      val focusV        = setFocus.extvar($.state.focus)
      val viewSettingsV = setViewSettings.extvarR(viewSettings, Reusable.byRef)

      <.div(
        vsEditor(viewSettingsV),
        Table.Component(Table.Props(project, content, focusV)))
    }
  }
}
