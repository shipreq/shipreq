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
case class ViewSettings(columnState: ColumnsEditor.State,
                        order      : SortCriteria,
                        filter     : Option[FilterAst],
                        filterDead : FilterDead) {

  val columns: NonEmptyVector[Column] =
    // Safe to force because ∃ at least 1 mandatory field
    NonEmptyVector force columnState.on.filter(Column filterDead filterDead)

  def isVisible(c: Column): Boolean =
    isVisible(_ ≟ c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)

  def setFilterDead(fd: FilterDead): ViewSettings =
    if (fd ≟ this.filterDead) this else {
      val o = filterDead match {
        case ShowDead => order
        case HideDead => order filterColumns Column.filterDead(fd)
      }
      ViewSettings(columnState, o, filter, fd)
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

  def default(allColumns: NonEmptyVector[Column],
              cnr: Option[Column.NameResolver] = None,
              fd: FilterDead = HideDead): ViewSettings = {
    // TODO Project knows custom field order. Use it here...right?
    import Column._
    val on = Vector[Column](Code, Pubid, Title, Tags)
    val off = {
      val t = allColumns.whole.filterNot(on.contains)
      cnr.fold(t)(t sortBy _.fn)
    }
    val all = on ++ off
    val cols = ColumnsEditor.State.init(all)(On <~ on.contains(_))
    ViewSettings(cols, SortCriteria.default, None, fd)
  }
}
