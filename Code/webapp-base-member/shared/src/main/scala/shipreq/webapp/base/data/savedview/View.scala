package shipreq.webapp.base.data.savedview

import japgolly.microlibs.nonempty.NonEmptyVector
import monocle.macros.Lenses
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.filter.Filter.Implicits._

@Lenses
final case class View(columns       : NonEmptyVector[Column],
                      order         : SortCriteria,
                      filterDead    : FilterDead,
                      filter        : Option[Filter.Valid],
                      impGraphConfig: Option[ImpGraphConfig]) {

  def referencesColumn(c: Column): Boolean =
    columns.exists(_ ==* c) ||
    order.init.exists(_.column ==* c) ||
    order.last.column ==* c

  def referencesReqType(id: ReqTypeId): Boolean =
    filter.exists(Filter.Valid.exists(
      text    = _ => false,
      regex   = _ => false,
      hashRef = _ => false,
      field   = _ => false,
      attr    = _ => false,
      reqSet  = _.exists(_.reqType ==* id),
      reqType = _ ==* id))

  def isVisible(c: Column): Boolean =
    isVisible(_ ==* c)

  def isVisible(f: Column => Boolean): Boolean =
    columns.exists(f)

  @inline def isOrdered (c: Column)                             = order.isOrdered(c)
  @inline def isOrdered (f: Column => Boolean)                  = order.isOrdered(f)
  @inline def isOrderedI(c: Column.SortInconclusive)            = order.isOrderedI(c)
  @inline def isOrderedI(f: Column.SortInconclusive => Boolean) = order.isOrderedI(f)

  /** Return a version of this where:
    *
    * - all mandatory columns are present
    * - only visible columns are used in sort criteria
    * - filterDead is ShowDead if colouring by a dead tag
    */
  def makeCorrect(pc: ProjectConfig): View = {

    // Ensure mandatory columns are present
    val newCols = columns ++ Column.mandatory.iterator.filterNot(columns.toNES.contains)

    // Filter order
    val icols = newCols.foldLeft(UnivEq.emptySet[Column.SortInconclusive])((q, c) => c match {
      case i: Column.SortInconclusive => q + i
      case _: Column.SortConclusive   => q
    })
    val newOrder = order.whitelistColumns(icols)

    // Check if we need to override filterDead
    val forceShowDead =
      if (filterDead is ShowDead)
        false
      else
        impGraphConfig match {
          case Some(ImpGraphConfig(_, _, ImpGraphConfig.Colours.ByTag(tagGroupId))) =>
            pc.tags.needTagGroup(tagGroupId).live is Dead
          case _ =>
            false
        }

    val view1 = View(newCols, newOrder, filterDead, filter, impGraphConfig)
    val view2 = if (forceShowDead) view1.copy(filterDead = ShowDead) else view1

    view2
  }

  def filterByFilterDead(pc: ProjectConfig)(l: Column => Live): View =
    filterDead.filterFn.value.fold(this)(f => filterColumns(pc)(f compose l))

  def filterColumns(pc: ProjectConfig)(f: Column => Boolean): View =
    tryFilterColumns(pc)(f) getOrElse withColumns(View.default.columns, pc)

  def tryFilterColumns(pc: ProjectConfig)(f: Column => Boolean): Option[View] =
    columns.filter(f).map(withColumns(_, pc))

  def withColumns(newCols0: NonEmptyVector[Column], pc: ProjectConfig): View =
    copy(columns = newCols0).makeCorrect(pc)

  def orderByColumn(c: Column): View =
    copy(order = order.want(c))

  def withFilter(f: Option[Filter.Valid]): View =
    copy(filter = f)

  /**
   * When `true`, render the reqcode column to resemble a tree. Meaning:
   *  - display reqcode groups.
   *  - replace common prefixes with indentation.
   *  - use a monospace font.
   */
  val viewReqCodesAsTree: Boolean =
    order.init.headOption.exists(s =>
      s.column ==* Column.Code)

  val viewCodeGroups: Boolean =
    order.init.headOption.exists(s =>
      (s.column ==* Column.Code) &&
        s.method.ascending // Doesn't make sense showing CodeGroups below everything they represent
    )
}


object View {
  implicit def equality: UnivEq[View] = UnivEq.derive

  def default: View =
    default(HideDead)

  def default(fd: FilterDead): View = {
    import Column._
    val cols = NonEmptyVector[Column.BuiltIn](Pubid, Title)

    val order = SortCriteria(
      Vector.empty, // (Column.Code / SortMethod.AscThenBlanks, Column.Title / SortMethod.BlanksThenAsc),
      SortCriteria.defaultConclusive)

    View(cols, order, fd, None, None)
  }
}
