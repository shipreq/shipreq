package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst

@Lenses
case class ViewSettings(columns    : NonEmptyVector[Column],
                        order      : SortCriteria,
                        filter     : Option[FilterAst],
                        filterDead : FilterDead) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ==* c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)

  def tryFilterColumns(f: Column => Boolean): Option[ViewSettings] =
    columns.filter(f).map(cols =>
      ViewSettings(cols, order filterColumns f, filter, filterDead))

  def filterColumns(f: Column => Boolean): ViewSettings =
    tryFilterColumns(f) getOrElse ViewSettings.default(filterDead)

  def setFilterDead(fd: FilterDead): ViewSettings =
    copy(filterDead = fd).filterColumns(Column filterDead fd)

  def setColumns(newCols0: NonEmptyVector[Column]): ViewSettings = {
    // Ensure mandatory columns are present
    val set = newCols0.toNES
    val newCols = newCols0 ++ Column.mandatory.filterNot(set.contains)

    // Filter order
    val icols = newCols.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
      case i: Column.SortInconclusive => q + i
      case _: Column.SortConclusive   => q
    })
    val newOrder = order.whitelistColumns(icols)

    ViewSettings(newCols, newOrder, filter, filterDead)
  }

  /**
   * When `true`, render the reqcode column to resemble a tree. Meaning:
   *  - display reqcode groups.
   *  - replace common prefixes with indentation.
   *  - use a monospace font.
   */
  final val viewReqCodesAsTree: Boolean =
    order.init.headOption.exists(s =>
      s.column ==* Column.Code)

  // Doesn't make sense showing ReqCodeGroups below everything they represent.
  final val viewReqCodeGroups: Boolean =
    order.init.headOption.exists(s =>
      (s.column ==* Column.Code) && s.method.ascending)
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
