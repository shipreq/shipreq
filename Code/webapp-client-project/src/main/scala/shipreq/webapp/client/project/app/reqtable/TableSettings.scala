package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.univeq._
import shipreq.webapp.base.filter.ValidFilter
import shipreq.webapp.base.lib.DataReusability._

@Lenses
final case class TableSettings(columns: NonEmptyVector[Column],
                               order  : SortCriteria,
                               filter : Option[ValidFilter]) {

  def isVisible(c: Column): Boolean =
    isVisible(_ ==* c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)

  def tryFilterColumns(f: Column => Boolean): Option[TableSettings] =
    columns.filter(f).map(cols =>
      TableSettings(cols, order filterColumns f, filter))

  def filterColumns(f: Column => Boolean): TableSettings =
    tryFilterColumns(f) getOrElse TableSettings.default

  def setColumns(newCols0: NonEmptyVector[Column]): TableSettings = {
    // Ensure mandatory columns are present
    val set = newCols0.toNES
    val newCols = newCols0 ++ Column.mandatory.filterNot(set.contains)

    // Filter order
    val icols = newCols.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
      case i: Column.SortInconclusive => q + i
      case _: Column.SortConclusive   => q
    })
    val newOrder = order.whitelistColumns(icols)

    TableSettings(newCols, newOrder, filter)
  }

  /**
   * When `true`, render the reqcode column to resemble a tree. Meaning:
   *  - display reqcode groups.
   *  - replace common prefixes with indentation.
   *  - use a monospace font.
   */
  val viewReqCodesAsTree: Boolean =
    order.init.headOption.exists(s =>
      s.column ==* Column.Code)

  // Doesn't make sense showing CodeGroups below everything they represent.
  val viewCodeGroups: Boolean =
    order.init.headOption.exists(s =>
      (s.column ==* Column.Code) && s.method.ascending)
}


object TableSettings {
  implicit def equality   : UnivEq[TableSettings]      = UnivEq.derive
  implicit val reusability: Reusability[TableSettings] = Reusability.byRefOrUnivEq

  def default: TableSettings = {
    import Column._
    val cols = NonEmptyVector[Column](Code, Pubid, Title, Tags)
    TableSettings(cols, SortCriteria.default, None)
  }
}
