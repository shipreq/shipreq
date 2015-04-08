package shipreq.webapp.client.app.ui.reqtable

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaz.OneAnd
import scalaz.syntax.equal._
import scalaz.syntax.semigroup._
import shipreq.base.util.{UnivEq, NonEmptyVector, Vector1}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import DataImplicits._

private[reqtable] object Logic {

  // ===================================================================================================================
  // Expansion

  private type Expanded[A] = NonEmptyVector[Vector[A]]
  private type Expander[A] = (() => Set[A]) => Expanded[A]

  private final val emptyExpansions: NonEmptyVector[Expansion] =
    NonEmptyVector(Expansion.none)

  @inline private def emptyExpanded[A]: Expanded[A] =
    NonEmptyVector(Vector.empty)

  @inline private def isEmptyExp[A](e: Expanded[A]): Boolean =
    e.head.isEmpty && e.tail.isEmpty

  /**
   * Nothing to expand = [ [] ]
   * Don't expand      = [ [a,b,c] ]
   * Expanded          = [ [a], [b], [c] ]
   *
   * @param visible Is the column visible? If not, just pretend everything is empty.
   * @param expand  When visible, does the data need to be expanded?
   */
  private def expander[A](visible: Boolean, expand: => Boolean): Expander[A] = {
    @inline def nonEmpty(f: (A, Vector[A]) => Expanded[A]): Expander[A] =
      e => {
        val v = e().toVector
        if (v.isEmpty)
          emptyExpanded
        else
          f(v.head, v.tail)
      }

    def doExpand: Expander[A] =
      nonEmpty((h, t) =>
        NonEmptyVector(Vector1(h), t map Vector1))

    def dontExpand: Expander[A] =
      nonEmpty((h, t) => NonEmptyVector(h +: t))

    if (visible) {
      if (expand) doExpand else dontExpand
    } else
      _ => emptyExpanded
  }

  private def expanderC[A](vs: ViewSettings, c: Column.SortInconclusive): Expander[A] =
    expander(vs isVisible c, vs isOrderedI c)

  private def impColValueFn(p: Project): CustomField.Implication.Id => Req.Id => Set[Pubid] =
    fid => {
      // (source of implication for this column) → (all it transitively implies)
      val srcs: Stream[(Pubid, Set[Req.Id])] =
        mustResolve(
          p.customField(fid).map(f =>
            p.reqs.data.reqsByType(f.reqTypeId)
              .toStream
              .map(_.tmap2(_.pubid, _.id |> p.implicationSrcToTgtTC.nonRefl)))
        )(Stream.empty)

      if (srcs.isEmpty)
        Function const UnivEq.emptySet
      else
        id => srcs.filter(_._2 contains id).map(_._1).toSet
    }

  private def impColValueExpander(vs: ViewSettings, p: Project): Req.Id => Map[CustomField.Implication.Id, Expanded[Pubid]] = {
    val valueFn = impColValueFn(p)
    customFieldExpander[CustomField.Implication.Id, Pubid](vs, valueFn)
  }

  private def tagColValueExpander(vs: ViewSettings, p: Project): Req.Id => Map[CustomField.Tag.Id, Expanded[ApplicableTag.Id]] = {
    val reqTags = p.reqFieldData.data.tags
    customFieldExpander[CustomField.Tag.Id, ApplicableTag.Id](vs, c => {
      val legal = mustResolve(p.tagColumnDistribution.tagIdsForColumn(c))(UnivEq.emptySet)
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

  private def expandMapValues[K, V](src: Map[K, Expanded[V]]): NonEmptyVector[Map[K, Vector[V]]] = {
    type M = Map[K, Vector[V]]
    def go(keys: Vector[K], cur: M, r: Vector[M]): NonEmptyVector[M] =
      if (keys.isEmpty)
        NonEmptyVector(cur, r)
      else {
        val k  = keys.head
        val ks = keys.tail
        @inline def next(ms: Vector[M], v: Vector[V]) = go(ks, cur.updated(k, v), ms)
        src(k).foldMapLeft1(next(r, _))((r2, v) => next(r2.whole, v))
      }
    go(src.keys.toVector, Map.empty, Vector.empty)
  }

  private def expansions(impSrcs: Expanded[Pubid],
                         impTgts: Expanded[Pubid],
                         codes  : Expanded[ReqCode],
                         cfImps : Map[CustomField.Implication.Id, Expanded[Pubid]],
                         cfTags : Map[CustomField.Tag.Id,         Expanded[ApplicableTag.Id]]): NonEmptyVector[Expansion] =
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

  private def tagValuesFn(vs: ViewSettings, p: Project): Req.Id => Vector[ApplicableTag.Id] = {
    val reqTags = p.reqFieldData.data.tags
    val tagsUsedInColumns = mustResolve(p.tagColumnDistribution.tagIdsUsedInColumns)(UnivEq.emptySet)
    id => reqTags(id).filterNot(tagsUsedInColumns.contains).toVector
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
        exps.toStream.map(GenericReqRow(r, _, mv))
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

  // ===================================================================================================================

  // TODO AFTER SORTING: Add SHRs

  def mergeAdjacent[A](input: Stream[A])(m: (A, A) => Option[A]): Stream[A] = {
    @tailrec def go(seen: Stream[A], last: A, queue: Stream[A]): Stream[A] = {
      @inline def res = seen append (last #:: Stream.empty)
      if (queue.isEmpty)
        res
      else {
        val h = queue.head
        val t = queue.tail
        m(last, h) match {
          case None    => go(res, h, t)
          case Some(a) => go(seen, a, t)
        }
      }
    }

    if (input.isEmpty)
      input
    else {
      go(Stream.empty, input.head, input.tail)
    }
  }

  def consolidateAdjacentDups(rows: Stream[Row]): Stream[Row] =
    mergeAdjacent(rows)((x, y) =>
      (x, y) match {
        case (a: GenericReqRow, b: GenericReqRow) =>
          if (a.req.id ≟ b.req.id)
            Some(GenericReqRow(a.req, a.exp |+| b.exp, a.mv |+| b.mv)) // TODO resort
          else
            None
      }
    )

  // ===================================================================================================================
  def rowsForTable(vs: ViewSettings, p: Project): Stream[Row] =
    gather(vs, p) |> sort(vs, p) |> consolidateAdjacentDups
}
