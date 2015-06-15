package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import scalacss.ScalaCssReact._
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.client.app.ui.Checkbox
import shipreq.webapp.client.app.ui.Style.{reqtable => *}

object ViewSettingsEditor {

  type Component = ReactComponentC.ReqProps[Props, _, _, TopNode]

  case class Props(vs: ReusableVar[ViewSettings], filterProps: FilterEditor.Props)
  implicit val propsReuse = Reusability.caseclass2(Props.unapply)

  def apply(columnName: Column.NameResolver): Component =
    ReactComponentB[Props]("ViewSettingsEditor")
      .stateless
      .backend(new Backend(_, columnName))
      .render(_.backend.render)
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit], columnName: Column.NameResolver) {

    val columnsEditor = new ColumnsEditor(columnName)

    val filterDeadEditor = Checkbox.filterDead(
      ReusableFn.byName($.props.vs.mod).endoCall(_.setFilterDead))

    val th = <.th(*.viewSettingsHeader)

    def render = {
      val p = $.props
      val vs = p.vs.value

      def setColumns(cs: NonEmptyVector[Column]): ViewSettings = {
        val icols = cs.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
          case i: Column.SortInconclusive => q + i
          case _: Column.SortConclusive   => q
        })
        ViewSettings(cs, vs.order.whitelistColumns(icols), vs.filter, vs.filterDead)
      }

      def columns =
        columnsEditor.render(vs.filterDead, vs.columns, p.vs.set compose setColumns)

      def sortCriteria =
        SortCriteriaEditor.Props(vs.order, vs.columns.toNES, columnName, p.vs setL ViewSettings.order).component

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
                FilterEditor.Component(p.filterProps)))
      )))
    }
  }
}
