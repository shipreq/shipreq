package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.widget._
import SCRATCH._
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
      val xxx = ColumnRenderer.thingy(p.project, p.columnName)
      val crs: Vector[ColumnRenderer] =
        p.viewSettings.columns.map(xxx)

      val rows = Logic.gatherReqs(p.viewSettings, p.project)

      // Add SHRs

      // Sort
      
      // Render
      // TODO handle zero rows nicely. "33 reqs (SHRs?), 11 deleted, 3 excluded by filter."
      <.table(
        <.thead(
          <.tr(
            crs.map(cr =>
              <.th(
                cr.header)))),
        <.tbody(
          rows.map(r =>
            <.tr(
              crs.map(cr =>
                <.td(
                  cr render r))))))
    }
  }
}
