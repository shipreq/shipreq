package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import monocle.macros.Lenser
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UnivEq
import shipreq.webapp.client.util.EVar

case class ViewSettings(columns: Vector[Column],
                        order  : SortCriteria) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ≟ c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  def isOrdered(c: Column.SortInconclusive): Boolean =
    isOrdered(_ ≟ c)

  def isOrdered(f: Column.SortInconclusive => Boolean): Boolean =
    order.init.exists(_.column |> f)
}

object ViewSettings {
  private[this] def l = Lenser[ViewSettings]
  val _columns = l(_.columns)
  val _order   = l(_.order)

  def default =
    ViewSettings(Column.builtInValues.list.toVector, SortCriteria.default)
}

// =====================================================================================================================
object ViewSettingsEditor {

  type Props = EVar[ViewSettings]

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
