/*
package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, ProjectConfig}
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.Checkbox

object ViewSettingsEditor {

  case class Props(columnName   : Column.NameResolver,
                   projectConfig: ProjectConfig,
                   vs           : StateSnapshot[ViewSettings],
                   filter       : Reusable[VdomElement])

  implicit val propsReuse = Reusability.caseClass[Props]

  val Component =
    ScalaComponent.builder[Props]("ViewSettingsEditor")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val toggleColumn = Reusable.fn((c: Column) =>
      $.props >>= { p =>
        val vs = p.vs.value
        val newCols =
          if (vs.isVisible(c))
            vs.columns.filterNot(_ ==* c) // Turn off
          else
            Some(vs.columns :+ c)         // Turn on
        val newVS = vs setColumns newCols.getOrElse(vs.columns)
        p.vs setState newVS
      })

    val filterDeadEditor = Checkbox.filterDead(
      Reusable.fn((fd: FilterDead) => $.props.runNow().vs.modState(_ setFilterDead fd)))

    val th = <.th(*.viewSettingsHeader)

    def render(p: Props) = {
      val vs = p.vs.value

      def columns = {
        val all = Column.all(p.projectConfig, vs.filterDead)
        val p2 = ColumnsEditor.Props(
          vs.columns.toNES,
          toggleColumn,
          p.columnName,
          all.toNES)
        ColumnsEditor.Component(p2)
      }

      <.table(
        <.thead(
          <.tr(
            th("Columns"),
            th("Sorting"),
            th("Filter"))),
        <.tbody(
          <.tr(
            <.td(columns),
            <.td("REMOVED"),
            <.td(
              <.div(
                filterDeadEditor(vs.filterDead),
                p.filter.value))
      )))
    }
  }
}
*/
