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
  def default = {
    import Column._
    ViewSettings(
      NonEmptyVector(Code, Pubid, Title, Tags, ImplicationSrc),
      SortCriteria.default)
  }
}
