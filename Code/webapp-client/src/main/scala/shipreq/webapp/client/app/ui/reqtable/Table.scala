package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import japgolly.scalacss.ScalaCssReact._
import DataImplicits._

object Table {

  case class Props(viewSettings: ViewSettings,
                   project     : Project,
                   columnName  : Column.NameResolver)

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .build

  final class Backend($: BackendScope[Props, Unit]) {
    def render: ReactElement = {
      val p = $.props

      // Init columns
      val widgets = new ProjectWidgets(p.project)
      val renderers = new ColumnRenderers(p.project, p.columnName, widgets)
      val crs = p.viewSettings.columns.map(renderers.apply)

      // Sort
      val rows = Logic.rowsForTable(p.viewSettings, p.project)

      // Render
      // TODO handle zero rows nicely. "33 reqs (SHRs?), 11 deleted, 3 excluded by filter."
      <.table(*.table,
        <.thead(
          <.tr(
            crs.map(cr =>
              <.th(
                cr.columnStyle,
                cr.header)))),
        <.tbody(
          rows.map(row =>
            <.tr(
              crs.map(cr =>
                <.td(
                  cr.columnStyle,
                  cr render row))))))
    }
  }
}
