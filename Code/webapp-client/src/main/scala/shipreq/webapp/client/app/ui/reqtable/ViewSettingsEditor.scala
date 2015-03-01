package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.ExternalVar
import shipreq.base.util.UnivEq

object ViewSettingsEditor {

  type Props = ExternalVar[ViewSettings]

  def apply(columnName: Column.NameResolver) =
    ReactComponentB[Props]("ViewSettingsEditor")
      .stateless
      .backend(new Backend(_, columnName))
      .render(_.backend.render)
      .build

  final class Backend($: BackendScope[Props, Unit], columnName: Column.NameResolver) {

    val columnsEditor = new ColumnsEditor(columnName)

    def render = {
      val p = $.props
      val vs = p.value

      def setColumns(cs: Vector[Column]): ViewSettings = {
        val icols = cs.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
          case i: Column.SortInconclusive => q + i
          case _: Column.SortConclusive   => q
        })
        ViewSettings(cs, vs.order.whitelistColumns(icols))
      }

      def columns =
        columnsEditor.render(vs.columns, p.set compose setColumns)

      def sortCriteria =
        SortCriteriaEditor.Props(vs.order, vs.columns.toSet, columnName, p setL ViewSettings._order).component

      <.table(
        <.thead(
          <.tr(
            <.th("Columns"),
            <.th("Sorting"))),
        <.tbody(
          <.tr(
            <.td(columns),
            <.td(sortCriteria))))
    }
  }
}
