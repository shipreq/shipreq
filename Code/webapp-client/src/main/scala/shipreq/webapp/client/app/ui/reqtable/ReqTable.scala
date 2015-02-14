package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import monocle.macros.Lenser
import shipreq.webapp.base.data._

object ReqTable {

  case class State(cols: Vector[Column], sort: SortCriteria)
  object State {
    private[this] def l = Lenser[State]
    val cols = l(_.cols)
    val sort = l(_.sort)
  }

  val WIP =
    ReactComponentB[Project]("WIP")
      .initialState(State(Column.nonFieldValues.list.toVector, SortCriteria.default))
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  final class Backend($: BackendScope[Project, State]) {

    val C = $.focusStateL(State.cols)
    val S = $.focusStateL(State.sort)
    val n = Column.NameResolver($.props.fields.data.customFields, CustomField.nameP($.props))

    val ed = new ColumnEditor(n)

    def render =
      <.table(
        <.thead(
          <.tr(
            <.th("Columns"),
            <.th("Sorting"))),
        <.tbody(
          <.tr(
            <.td(
              ed.render(C.state, C.setStateIO(_))), // TODO this should remove from sort
            <.td(
              SortCriteriaEditor.Props(S.state, C.state.toSet, n, S.setStateIO(_)).component))))
  }
}
