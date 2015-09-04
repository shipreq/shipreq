package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalaz.syntax.equal._
import shipreq.base.util.NonEmptySet
import shipreq.webapp.base.data.FieldSet
import shipreq.webapp.client.app.ui.Checkbox
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._

object ViewSettingsEditor {

  case class Props(columnName  : Column.NameResolver,
                   customFields: FieldSet.CustomFields,
                   vs          : ReusableVar[ViewSettings],
                   filter      : ReusableVal[ReactElement])

  implicit val propsReuse = Reusability.caseClass[Props]

  val Component =
    ReactComponentB[Props]("ViewSettingsEditor")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val toggleColumn = ReusableFn((c: Column) =>
      $.propsCB >>= { p =>
        val vs = p.vs.value
        val newCols =
          if (vs.isVisible(c))
            vs.columns.filterNot(_ ≟ c) // Turn off
          else
            Some(vs.columns :+ c)       // Turn on
        val newVS = vs setColumns newCols.getOrElse(vs.columns)
        p.vs set newVS
      })

    val filterDeadEditor = Checkbox.filterDead(
      ReusableFn.byName($.props.vs.mod).endoCall(_.setFilterDead))

    val th = <.th(*.viewSettingsHeader)

    def render(p: Props) = {
      val vs = p.vs.value

      def columns = {
        val all = Column.all(p.customFields.values).toNES.whole filter Column.filterDead(vs.filterDead)
        val p2 = ColumnsEditor.Props(
          vs.columns.toNES,
          toggleColumn,
          p.columnName,
          NonEmptySet force all)
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
                p.filter))
      )))
    }
  }
}
