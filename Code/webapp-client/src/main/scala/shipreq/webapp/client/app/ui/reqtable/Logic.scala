package shipreq.webapp.client.app.ui.reqtable

import scala.reflect.ClassTag
import scalaz.NonEmptyList
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
    expander(vs isVisible c, vs isOrderedI c)

  private def impColValueFn(p: Project): CustomField.Implication.Id => Req.Id => Set[Pubid] = {
    lazy val tc = TransitiveClosure.auto[Req.Id](p.reqs.data.reqs.keys)(
      p.reqFieldData.data.implications.srcToTgt.apply)

    fid => {
      // (source of implication for this column) → (all it transitively implies)
      val srcs: Stream[(Pubid, Set[Req.Id])] =
        mustResolve(Stream.empty,
          p.customField(fid).map(f =>
            p.reqs.data.reqsByType(f.reqTypeId)
              .toStream
              .map(_.tmap2(_.pubid, _.id |> tc.nonRefl))))

      if (srcs.isEmpty)
        Function const UnivEq.emptySet
      else
        id => srcs.filter(_._2 contains id).map(_._1).toSet
    }
  }

  private def impColValueExpander(vs: ViewSettings, p: Project): Req.Id => Map[CustomField.Implication.Id, Expanded[Pubid]] = {
    val valueFn = impColValueFn(p)
    customFieldExpander[CustomField.Implication.Id, Pubid](vs, valueFn)
  }

  private def tagColValueExpander(vs: ViewSettings, p: Project): Req.Id => Map[CustomField.Tag.Id, Expanded[ApplicableTag.Id]] = {
    // Traversing the tag tree for used columns is better than calculating the full
    // transitive closure at O(V²) space and O(V²+VE) time.
    implicit val tagTree = p.tags.data
    def tagsForColumn(fid: CustomField.Tag.Id): Set[Tag.Id] = {
      val m = p.customField(fid).flatMap(field => tagTree(field.tagId).flatMap(_.transitiveChildren))
      m.fold(failedMust(UnivEq.emptySet), identity)
    }

    val reqTags = p.reqFieldData.data.tags
    customFieldExpander[CustomField.Tag.Id, ApplicableTag.Id](vs, c => {
      val legal = tagsForColumn(c)
      id => reqTags(id) filter legal.contains
    })
  }

  private def customFieldExpander[K <: CustomField.Id : ClassTag, V]
      (vs: ViewSettings, f: K => Req.Id => Set[V]): Req.Id => Map[K, Expanded[V]] = {

    val cols = vs.columns.collect{ case Column.CustomField(id: K) => id }

    val expandersPerCol = cols.map { c =>
        val expander = expanderC[V](vs, Column.CustomField(c))
        val dataFn   = f(c)
        val fn       = (id: Req.Id) => expander(() => dataFn(id))
        (c, fn)
      }.toMap

    id => expandersPerCol.mapValues(_(id))
  }

  private def expandMapValues[K, V](src: Map[K, Expanded[V]]): NonEmptyList[Map[K, List[V]]] = {
    type M = Map[K, List[V]]
    def go(keys: List[K], cur: M, r: List[M]): NonEmptyList[M] =
      keys match {
        case Nil =>
          NonEmptyList.nel(cur, r)
        case k :: ks =>
          @inline def next(ms: List[M], v: List[V]) = go(ks, cur.updated(k, v), ms)
          NonEmptyList.nonEmptyList.foldMapLeft1(src(k))(next(r, _))((r2, v) => next(r2.list, v))
      }
    go(src.keys.toList, Map.empty, Nil)
  }

  private def expansions(impSrcs: Expanded[Pubid],
                         impTgts: Expanded[Pubid],
                         codes  : Expanded[ReqCode],
                         cfImps : Map[CustomField.Implication.Id, Expanded[Pubid]],
                         cfTags : Map[CustomField.Tag.Id,         Expanded[ApplicableTag.Id]]): NonEmptyList[Expansion] =
    if (   isEmptyExp(codes)
        && isEmptyExp(impSrcs)
        && isEmptyExp(impTgts)
        && cfImps.values.forall(isEmptyExp)
        && cfTags.values.forall(isEmptyExp))
      emptyExpansions
    else
      for {
        a <- impSrcs
        b <- impTgts
        c <- codes
        d <- expandMapValues(cfImps)
        e <- expandMapValues(cfTags)
      } yield Expansion(a, b, c, d, e)

  // ===================================================================================================================
  // MultiValues

  private def tagValuesFn(vs: ViewSettings, p: Project): Req.Id => List[ApplicableTag.Id] = {
    val reqTags = p.reqFieldData.data.tags
    id => reqTags(id).toList
  }

  private def multiValuesFn(vs: ViewSettings, p: Project): Req.Id => MultiValues = {
    val tagValuesFn = this.tagValuesFn(vs, p)
    id => {
      val tags = tagValuesFn(id)
      MultiValues(tags)
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
    val expandImpCols = impColValueExpander(vs, p)
    val expandTagCols = tagColValueExpander(vs, p)

    val pReqs         = p.reqs.data
    val pReqCodes     = p.reqCodes.data
    val pImplications = p.reqFieldData.data.implications
    val multiValuesFn = this.multiValuesFn(vs, p)

    def pubids(s: Set[Req.Id]): Set[Pubid] =
      s.foldLeft(UnivEq.emptySet[Pubid])((q, id) =>
        pReqs.reqM(id).fold(failedMust(q), q + _.pubid))

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
        val cfImps  = expandImpCols(id)
        val cfTags  = expandTagCols(id)
        val exps    = expansions(impSrcs, impTgts, codes, cfImps, cfTags)

        // Build
        val mv = multiValuesFn(id)
        exps.list.toStream.map(GenericReqRow(r, _, mv))
    }
  }

  // ===================================================================================================================
  // Sorting

  def sort(vs: ViewSettings, p: Project)(rows: Stream[Row]): Stream[Row] = {
    import Sorter._

    // Prepare sorters
    val sorter  = new FusedSorters(vs.order.init map inconclusive, vs.order.last |> conclusive)
    val setup   = new Setup(p)
    val prepare = sorter.prepFn(setup)
    val rowMod  = Sorter.consolidateRowModFns(Sorter.sortUnspecified(vs) :: sorter.rowModFn :: Nil)
    val rowEndo = rowMod.map(_(setup, KeepDir)) getOrElse ((r: Row) => r)
    import sorter.{T => Datum}

    // Prepare data
    val data = new Array[Datum](rows.length)
    var i = 0
    rows.foreach { r =>
      data(i) = prepare(rowEndo(r))
      i = i + 1
    }

    // Sort
    scala.util.Sorting.quickSort(data)(sorter.sortFn.toOrdering)

    // Unpack results
    data.toStream map sorter.row
  }

  // TODO AFTER SORTING: consolidateAdjacentDups
  // TODO AFTER SORTING: Add SHRs

  // ===================================================================================================================
  def rowsForTable(vs: ViewSettings, p: Project): Stream[Row] =
    gather(vs, p) |> sort(vs, p)
}
