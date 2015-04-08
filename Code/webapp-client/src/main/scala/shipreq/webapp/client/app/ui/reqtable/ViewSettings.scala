package shipreq.webapp.client.app.ui.reqtable

import monocle.macros.Lenses
import scalaz.syntax.equal._
import shipreq.base.util.NonEmptyVector

@Lenses
case class ViewSettings(columns: NonEmptyVector[Column],
                        order  : SortCriteria) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ≟ c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)
}


object ViewSettings {
  def default =
    ViewSettings(Column.builtInValues, SortCriteria.default)
}
