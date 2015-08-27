package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import scalacss.ScalaCssReact._
import shipreq.base.util.UnivEq
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
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val filterDeadEditor = Checkbox.filterDead(
      ReusableFn.byName($.props.vs.mod).endoCall(_.setFilterDead))

    val th = <.th(*.viewSettingsHeader)

    def render = {
      val p = $.props
      val vs = p.vs.value

      def setColumns(s: ColumnsEditor.State): ViewSettings = {
        val icols = s.on.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
          case i: Column.SortInconclusive => q + i
          case _: Column.SortConclusive   => q
        })
        ViewSettings(s, vs.order.whitelistColumns(icols), vs.filter, vs.filterDead)
      }

      def columns =
        ColumnsEditor(
          vs.columnState,
          p.vs.set compose setColumns,
          p.columnName,
          p.customFields,
          Column filterDead vs.filterDead)

      def sortCriteria =
        SortCriteriaEditor.Props(vs.order, vs.columns.toNES, p.columnName, p.vs setL ViewSettings.order)
          .component

      <.table(
        <.thead(
          <.tr(
            th("Columns"),
            th("Sorting"),
            th("Filter"))),
        <.tbody(
          <.tr(
            <.td(columns),
            <.td(sortCriteria),
            <.td(
              <.div(
                filterDeadEditor(vs.filterDead),
                p.filter))
      )))
    }
  }
}
