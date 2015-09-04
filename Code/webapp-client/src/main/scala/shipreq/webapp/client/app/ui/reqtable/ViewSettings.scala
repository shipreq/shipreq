package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import scalaz.syntax.equal._
import shipreq.base.util.{UnivEq, NonEmptyVector}
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.client.lib.{ShowDead, HideDead, FilterDead}
import shipreq.webapp.client.util.On

@Lenses
case class ViewSettings(columns    : NonEmptyVector[Column],
                        order      : SortCriteria,
                        filter     : Option[FilterAst],
                        filterDead : FilterDead) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ≟ c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)

  def filterColumns(f: Column => Boolean): Option[ViewSettings] =
    columns.filter(f).map(cols =>
      ViewSettings(cols, order filterColumns f, filter, filterDead))

  def setFilterDead(fd: FilterDead): ViewSettings =
    filterColumns(Column filterDead fd) match {
      case Some(vs) => vs.copy(filterDead = fd)
      case None     => ViewSettings.default(fd)
    }

  /**
   * When `true`, render the reqcode column to resemble a tree. Meaning:
   *  - display reqcode groups.
   *  - replace common prefixes with indentation.
   *  - use a monospace font.
   */
  final val viewReqCodesAsTree: Boolean =
    order.init.headOption.exists(s =>
      s.column ≟ Column.Code)

  // Doesn't make sense showing ReqCodeGroups below everything they represent.
  final val viewReqCodeGroups: Boolean =
    order.init.headOption.exists(s =>
      (s.column ≟ Column.Code) && s.method.ascending)
}


object ViewSettings {
  implicit def equality   : UnivEq[ViewSettings]      = UnivEq.derive
  implicit val reusability: Reusability[ViewSettings] = Reusability.byEqual

  def default(fd: FilterDead = HideDead): ViewSettings = {
    import Column._
    val cols = NonEmptyVector[Column](Code, Pubid, Title, Tags)
    ViewSettings(cols, SortCriteria.default, None, fd)
  }
}
