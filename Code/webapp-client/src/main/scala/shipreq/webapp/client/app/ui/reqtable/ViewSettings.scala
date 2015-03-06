package shipreq.webapp.client.app.ui.reqtable

import monocle.macros.Lenser
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._

case class ViewSettings(columns: Vector[Column],
                        order  : SortCriteria) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ≟ c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  def isOrdered(c: Column): Boolean =
    isOrdered(_ ≟ c)

  def isOrdered(f: Column => Boolean): Boolean =
    f(order.last.column) || isOrderedI(f)

  def isOrderedI(c: Column.SortInconclusive): Boolean =
    isOrderedI(_ ≟ c)

  def isOrderedI(f: Column.SortInconclusive => Boolean): Boolean =
    order.init.exists(_.column |> f)
}


object ViewSettings {
  private[this] def l = Lenser[ViewSettings]
  val _columns        = l(_.columns)
  val _order          = l(_.order)

  def default =
    ViewSettings(Column.builtInValues.list.toVector, SortCriteria.default)
}
