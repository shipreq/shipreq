package shipreq.webapp.client.app.ui.reqtable

import scalaz.{Apply, NonEmptyList}
import shipreq.base.util.{UnivEq, Must}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.TransitiveClosure
import DataImplicits._

private[reqtable] object Logic {

  // ===================================================================================================================
  // Expansion

  private type Expanded[+A] = NonEmptyList[List[A]]
  private type Expander[A]  = (() => Set[A]) => Expanded[A]

  private final val emptyExpansions: NonEmptyList[Expansion] =
    NonEmptyList(Expansion.none)

  private final val emptyExpanded: Expanded[Nothing] =
    NonEmptyList(Nil)

  @inline private def isEmptyExp[A](e: Expanded[A]): Boolean =
    e eq emptyExpanded

  /**
   * Nothing to expand = [ [] ]
   * Don't expand      = [ [a,b,c] ]
   * Expanded          = [ [a], [b], [c] ]
   *
   * @param visible Is the column visible? If not, just pretend everything is empty.
   * @param expand  When visible, does the data need to be expanded?
   */
  private def expander[A](visible: Boolean, expand: => Boolean): Expander[A] = {
    @inline def nonEmpty(f: (A, List[A]) => Expanded[A]): Expander[A] =
      _().toList match {
        case Nil    => emptyExpanded
        case h :: t => f(h, t)
      }

    def doExpand: Expander[A] =
      nonEmpty((h, t) =>
        NonEmptyList.nel(h :: Nil, t.map(_ :: Nil)))

    def dontExpand: Expander[A] =
      nonEmpty((h, t) => NonEmptyList(h :: t))

    if (visible) {
      if (expand) doExpand else dontExpand
    } else
      _ => emptyExpanded
  }

  private def expanderC[A](vs: ViewSettings, c: Column.SortInconclusive): Expander[A] =
    expander(vs isVisible c, vs isOrdered c)

  private def expansions(impSrcs: Expanded[Pubid], impTgts: Expanded[Pubid], codes: Expanded[ReqCode]): NonEmptyList[Expansion] =
    if (isEmptyExp(codes) && isEmptyExp(impSrcs) && isEmptyExp(impTgts))
      emptyExpansions
    else
      Apply[NonEmptyList].apply3(impSrcs, impTgts, codes)(Expansion.apply)

  // ===================================================================================================================
  // MultiValues

  private final val emptyMultiValues: MultiValues =
    MultiValues(Nil, UnivEq.emptyMap, UnivEq.emptyMap)

  private def impColFn(p: Project): CustomField.Implication.Id => Req.Id => List[Pubid] = {
    lazy val tc = TransitiveClosure.auto[Req.Id](p.reqs.data.reqs.keys)(
      p.reqFieldData.data.implications.srcToTgt.apply)

    fid => {
      // (source of implication for this column) → (all it transitively implies)
      val srcs: Stream[(Pubid, Set[Req.Id])] =
        mustResolve(Stream.empty,
          p.customField(fid).map(f =>
            p.reqs.data.reqsByType(f.reqTypeId)
              .toStream
              .map(_.tmap2(_.pubId, _.id |> tc.nonRefl))))

      if (srcs.isEmpty)
        Function const Nil
      else
        id => srcs.filter(_._2 contains id).map(_._1).toList
    }
  }

  private def impColValuesFn(vs: ViewSettings, p: Project): Req.Id => Map[CustomField.Implication.Id, List[Pubid]] = {
    val impCols =
      vs.columns.collect{ case Column.CustomField(id: CustomField.Implication.Id) => id }
    if (impCols.isEmpty)
      Function const UnivEq.emptyMap
    else {
      val fnfn     = impColFn(p)
      val fnsByCol = impCols.map(_.mapStrengthR(fnfn)).toMap
      id => fnsByCol.mapValues(_(id))
    }
  }

  private def tagValuesFn(vs: ViewSettings, p: Project): Req.Id => (List[ApplicableTag.Id], Map[CustomField.Tag.Id, List[ApplicableTag.Id]]) = {
    implicit val tagTree = p.tags.data
    val reqTags = p.reqFieldData.data.tags

    val tagCols =
      vs.columns.collect{ case Column.CustomField(id: CustomField.Tag.Id) => id }

    // Traversing the tag tree for used columns is better than calculating the full
    // transitive closure at O(V²) space and O(V²+VE) time.
    def tagsForColumn(fid: CustomField.Tag.Id): Set[Tag.Id] = {
      val m = p.customField(fid).flatMap(field => tagTree(field.tagId).flatMap(_.transitiveChildren))
      m.fold(failedMust(UnivEq.emptySet), identity)
    }
    val tagsByColumn = tagCols.map(_.mapStrengthR(tagsForColumn)).toMap

    id => {
      val tags   = reqTags(id).toList
      val cfTags = tagsByColumn.mapValues(legal => tags.filter(legal contains _))
      (tags, cfTags)
    }
  }

  private def multiValuesFn(vs: ViewSettings, p: Project): Req.Id => MultiValues = {
    val tagValuesFn    = this.tagValuesFn(vs, p)
    val impColValuesFn = this.impColValuesFn(vs, p)
    id => {
      val (tags, cfTags) = tagValuesFn(id)
      val cfImps         = impColValuesFn(id)
      MultiValues(tags, cfTags, cfImps)
    }
  }

  // ===================================================================================================================
  // Gathering

  /**
   * Gathers [[Row]]s for display in [[ReqTable]].
   * Performs expansion.
   * Does not perform any sorting.
   */
  def gather(vs: ViewSettings, p: Project): Stream[Row] = {

    // NOTE:
    // * Column.ImplicationSrc isn't transitive; custom implication columns are.
    //   There can potentially be overlap but culling this could be misleading.

    // Init
    val expandImpSrcs = expanderC[Pubid](vs, Column.ImplicationSrc)
    val expandImpTgts = expanderC[Pubid](vs, Column.ImplicationTgt)
    val expandCodes   = expanderC[ReqCode](vs, Column.Code)
    val pReqs         = p.reqs.data
    val pReqCodes     = p.reqCodes.data
    val pImplications = p.reqFieldData.data.implications
    val multiValuesFn = this.multiValuesFn(vs, p)

    def pubids(s: Set[Req.Id]): Set[Pubid] =
      s.foldLeft(UnivEq.emptySet[Pubid])((q, id) =>
        pReqs.reqM(id).fold(failedMust(q), q + _.pubId))

    // Traverse reqs
    p.reqs.data.reqs.vstreamf {
      case r: GenericReq =>
        val id = r.id

        // Remove deleted

        // Filter

        // Expansion
        val impSrcs = expandImpSrcs(() => pImplications.tgtToSrc(id) |> pubids)
        val impTgts = expandImpTgts(() => pImplications.srcToTgt(id) |> pubids)
        val codes   = expandCodes  (() => pReqCodes.byTarget(id))
        val exps    = expansions(impSrcs, impTgts, codes)

        // Build
        val mv = multiValuesFn(id)
        exps.list.toStream.map(GenericReqRow(r, _, mv))
    }

  }

  // ===================================================================================================================
  // Sorting

  def sort(criteria: SortCriteria, p: Project)(rows: Stream[Row]): List[Row] = {
    import Sorter._

    // Prepare sorters
    val sorter  = new FusedSorters(criteria.init map inconclusive, criteria.last |> conclusive)
    val setup   = new Setup(p)
    val prepare = sorter.prepFn(setup)
    val rowMod  = sorter.rowModFn.map(_(setup, KeepDir)).getOrElse((r: Row) => r)
    import sorter.{T => Datum}

    // Prepare data
    val data = new Array[Datum](rows.length)
    var i = 0
    rows.foreach { r =>
      data(i) = prepare(rowMod(r))
      i = i + 1
    }

    // Sort
    scala.util.Sorting.quickSort(data)(sorter.sortFn.toOrdering)

    // Unpack results
    data.foldRight[List[Row]](Nil)((d, q) => sorter.row(d) :: q)
  }

  // AFTER SORTING
  // - consolidateAdjacentDups
  // - Add SHRs
}
