package shipreq.webapp.client.app.ui.reqtable

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scalaz.syntax.equal._
import scalaz.syntax.semigroup._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.lib.{HideDead, ShowDead, FilterDead}
import DataImplicits._

private[reqtable] object Logic {

  private type TagFilter = EndoFn[Set[ApplicableTagId]]

  private def tagFilter(vs: ViewSettings, p: Project): TagFilter =
    vs.filterDead.filter
      .fold[TagFilter](identity) { f =>
        val bl = p.atags.filter(t => !f(t.live)).map(_.id)
        (_: Set[ApplicableTagId]) -- bl
      }

  private type TagLookup = ReqId => Set[ApplicableTagId]

  private def tagLookup(p: Project): TagLookup = {
    val reqTags    = p.reqFieldData.data.tags
    val tagsInText = p.tagsInText
    id => reqTags(id) | tagsInText(id)
  }

  // ===================================================================================================================
  // Expansion

  private type Expanded[A] = NonEmptyVector[Vector[A]]
  private type Expander[A] = (() => Set[A]) => Expanded[A]

  private final val emptyExpansions: NonEmptyVector[Expansion] =
    NonEmptyVector.one(Expansion.none)

  @inline private def emptyExpanded[A]: Expanded[A] =
    NonEmptyVector.one(Vector.empty)

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
      nonEmpty((h, t) => NonEmptyVector.one(h +: t))

    if (visible) {
      if (expand) doExpand else dontExpand
    } else
      _ => emptyExpanded
  }

  private def expanderC[A](vs: ViewSettings, c: Column.SortInconclusive): Expander[A] =
    expander(vs isVisible c, vs isOrderedI c)

  private def impColValueFn(p: Project, fd: FilterDead): CustomField.Implication.Id => ReqId => Set[Pubid] =
    fid => {

      val reqsOfSubjectType: Stream[Req] =
        mustResolve(
          p.customField(fid).map(f =>
            p.reqs.data.reqsByType(f.reqTypeId).toStream)
        )(Stream.empty)

      // (source of implication for this column) → (all it transitively implies)
      val srcs: Stream[(Pubid, Set[ReqId])] =
        fd(reqsOfSubjectType)(_.live)
          .map(r => (r.pubid, p.implicationSrcToTgtTC(r.id)))

      id => srcs.filter(_._2 contains id).map(_._1).toSet
    }

  private def impColValueExpander(vs: ViewSettings, p: Project, ap: Applicability): Req => Map[CustomField.Implication.Id, Expanded[Pubid]] = {
    val valueFn = impColValueFn(p, vs.filterDead)
    customFieldExpander[CustomField.Implication.Id, Pubid](vs, ap, valueFn)
  }

  private def tagColValueExpander(vs        : ViewSettings,
                                  p         : Project,
                                  ap        : Applicability,
                                  tagColDist: TagColumnDistribution.TagIds,
                                  tagLookup : TagLookup,
                                  tagFilter : TagFilter): Req => Map[CustomField.Tag.Id, Expanded[ApplicableTagId]] = {
    customFieldExpander[CustomField.Tag.Id, ApplicableTagId](vs, ap, c => {
      val legal = mustResolve(tagColDist inColumn c)(UnivEq.emptySet)
      id => tagFilter(tagLookup(id) & legal)
    })
  }

  private def customFieldExpander[K <: CustomFieldId : ClassTag, V: UnivEq]
      (vs: ViewSettings, ap: Applicability, f: K => ReqId => Set[V]): Req => Map[K, Expanded[V]] = {

    val cols = vs.columns.whole.collect { case c@ Column.CustomField(id: K, _) => (id, c) }

    val expandersPerCol = cols.map { ct =>
        val colId    = ct._1
        val col      = ct._2
        val applic   = ap(col)
        val expander = expanderC[V](vs, col)
        val dataFn   = f(colId)
        val fn       = (r: Req) => expander(() => applic.choose(r, na = UnivEq.emptySet[V])(dataFn(r.id)))
        (colId, fn)
      }.toMap

    req => expandersPerCol mapValuesNow (_(req))
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
                         codes  : Expanded[ReqCode.Value],
                         cfImps : Map[CustomField.Implication.Id, Expanded[Pubid]],
                         cfTags : Map[CustomField.Tag.Id,         Expanded[ApplicableTagId]]): NonEmptyVector[Expansion] =
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
      } yield Expansion(a, b, c, Vector.empty[ReqCodeTreeItem], d, e)

  // ===================================================================================================================
  // MultiValues

  private def tagValuesFn(vs        : ViewSettings,
                          p         : Project,
                          tagColDist: TagColumnDistribution.TagIds,
                          tagLookup : TagLookup,
                          tagFilter : TagFilter): ReqId => Vector[ApplicableTagId] = {
    val tagsUsedInColumns = mustResolve(tagColDist.usedInColumns)(UnivEq.emptySet)
    id => tagFilter(tagLookup(id) -- tagsUsedInColumns).toVector
  }

  private def multiValuesFn(vs        : ViewSettings,
                            p         : Project,
                            tagColDist: TagColumnDistribution.TagIds,
                            tagLookup : TagLookup,
                            tagFilter : TagFilter): ReqId => MultiValues = {
    val tagValuesFn = this.tagValuesFn(vs, p, tagColDist, tagLookup, tagFilter)
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

    // NOTES:
    //
    // * Column.ImplicationSrc isn't transitive; custom implication columns are.
    //   There can potentially be overlap but culling this could be misleading.
    //
    // * The Tags column is not expanded. Only custom tag columns are.

    // The Tags column:
    // 1. never displays tags allocated to live tag-columns.
    // 2. doesn't display tags allocated to visible, dead tag-columns.
    val tagColDist: TagColumnDistribution.TagIds =
      vs.filterDead match {
        case HideDead => p.liveTagColumnDistribution
        case ShowDead => TagColumnDistribution(p, f => f.live match {
          case Live => true
          case Dead => vs isVisible Column.CustomField(f.id, Dead)
        })
      }

    val filterDead    = vs.filterDead.filterFn
    val tagLookup     = this.tagLookup(p)
    val tagFilter     = this.tagFilter(vs, p)
    val applicability = Applicability(p)
    val expandImpSrcs = expanderC[Pubid](vs, Column.ImplicationSrc)
    val expandImpTgts = expanderC[Pubid](vs, Column.ImplicationTgt)
    val expandCodes   = expanderC[ReqCode.Value](vs, Column.Code)
    val expandImpCols = impColValueExpander(vs, p, applicability)
    val expandTagCols = tagColValueExpander(vs, p, applicability, tagColDist, tagLookup, tagFilter)

    val pReqs         = p.reqs.data
    val pReqCodes     = p.reqCodes.data.activeReqCodesByTarget
    val pImplications = p.reqFieldData.data.implications
    val multiValuesFn = this.multiValuesFn(vs, p, tagColDist, tagLookup, tagFilter)

    def pubid(reqId: ReqId): Option[Pubid] =
      pReqs.reqM(reqId).fold[Option[Pubid]](failedMust(None), req =>
        if (filterDead(req.live)) Some(req.pubid) else None)

    def pubids(s: Set[ReqId]): Set[Pubid] =
      s.foldLeft(UnivEq.emptySet[Pubid])((q, id) =>
        pubid(id).fold(q)(q + _))

    val reqRows =
      p.reqs.data.reqs.vstreamf {
        case r: GenericReq =>
          if (filterDead(r.live)) {
            val id = r.id

            // Expansion
            val impSrcs = expandImpSrcs(() => pImplications.tgtToSrc(id) |> pubids)
            val impTgts = expandImpTgts(() => pImplications.srcToTgt(id) |> pubids)
            val codes   = expandCodes  (() => pReqCodes(id))
            val cfImps  = expandImpCols(r)
            val cfTags  = expandTagCols(r)
            val exps    = expansions(impSrcs, impTgts, codes, cfImps, cfTags)

            // Build
            val mv = multiValuesFn(id)
            exps.toStream.map(GenericReqRow(r, _, mv))

          } else
            Stream.empty[GenericReqRow]
      }

    val reqCodeGroupRows: Stream[ReqCodeGroupRow] =
      if (vs.viewReqCodeGroups)
        p.reqCodes.data.cataA(Stream.empty[ReqCodeGroupRow])((q, c, d) => d.target match {
          case _: ReqId        => q
          case g: ReqCodeGroup =>
            // TODO: Filter
            val groupAndId = g and d.id
            ReqCodeGroupRow(groupAndId, c, None) #:: q
        })
      else
        Stream.empty

    reqRows append reqCodeGroupRows
  }

  // ===================================================================================================================
  // Sorting

  def sort(vs: ViewSettings, p: Project, pt: PlainText.ForProject)(rows: Stream[Row]): Stream[Row] = {
    import Sorter._

    // Prepare sorters
    val sorter  = new FusedSorters(vs.order.init map inconclusive, vs.order.last |> conclusive)
    val setup   = new Setup(p, pt)
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
  // Post-processing

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
        case (_: GenericReqRow,   _: ReqCodeGroupRow)
           | (_: ReqCodeGroupRow, _: GenericReqRow)
           | (_: ReqCodeGroupRow, _: ReqCodeGroupRow) => None
      }
    )

  /** Map with history */
  def hmap[I, V[x] <: GenTraversable[x], A >: Null <: AnyRef, B >: Null, S[_], O]
      (input: GenTraversable[I], extract: I => V[A], put: (I, V[B]) => O, f: A => B, g: (A, B, A) => B)
      (implicit cbfV: CanBuildFrom[Nothing, B, V[B]], cbfS: CanBuildFrom[Nothing, O, S[O]]): S[O] = {
    var lastA: A = null
    var lastB: B = null
    val bs = cbfS()
    for (i <- input) {
      val bv = cbfV()
      for (a <- extract(i)) {
        val b = if (lastA eq null) f(a) else g(lastA, lastB, a)
        bv += b
        lastA = a
        lastB = b
      }
      bs += put(i, bv.result())
    }
    bs.result()
  }

  def mkReqCodeTree[I, V[x] <: GenTraversable[x], S[_], O]
      (input: GenTraversable[I], extract: I => V[ReqCode.Value], put: (I, V[ReqCodeTreeItem]) => O)
      (implicit cbfV: CanBuildFrom[Nothing, ReqCodeTreeItem, V[ReqCodeTreeItem]], cbfS: CanBuildFrom[Nothing, O, S[O]])
      : S[O] =
    hmap[I, V, ReqCode.Value, ReqCodeTreeItem, S, O](
      input, extract, put,
      c => ReqCodeTreeItem(Vector.empty, c),
      mkReqCodeTreeItem)

  def mkReqCodeTreeItem(prevV: ReqCode.Value, prevI: ReqCodeTreeItem, cur: ReqCode.Value): ReqCodeTreeItem = {
    import ReqCodeTreeItem._
    import VectorCase._

    @tailrec
    def go(ci: Vector[Indent], cv: ReqCode.Value)(pi: Vector[Indent], pv: ReqCode.Value): ReqCodeTreeItem =
      cv.tail match {
        case NonEmpty(nextc) if cv.head ≟ pv.head =>
          pv.tail match {
            case Empty() =>
              ReqCodeTreeItem(ci :+ IndentChild, nextc)
            case NonEmpty(nextp) =>
              pi match {
                case Empty() =>
                  go(ci :+ IndentSpace(pv.head.value.length), nextc)(Vector.empty, nextp)
                case NonEmpty(is) =>
                  go(ci :+ is.head, nextc)(is.tail, nextp)
              }
          }
        case _ =>
          ReqCodeTreeItem(ci, cv)
      }

    go(Vector.empty, cur)(prevI.indent, prevV)
  }

  val addReqCodeTreeToRows: EndoFn[Stream[Row]] = rows =>
    mkReqCodeTree[Row, Vector, Stream, Row](
      rows,
      Row.reqCodes.get,
      (i, bs) => Row.reqCodeTree.set(bs)(i))


  def stats(vs: ViewSettings, p: Project, rows: Iterable[Row]): TableStats = {

    // Scan rows
    var _codeGroups      = 0
    var _counts          = UnivEq.emptyMap[ReqId, Int]
    var _liveVisibleReqs = 0
    rows foreach {
      case _: ReqCodeGroupRow =>
        _codeGroups += 1
      case r: GenericReqRow =>
        val id = r.req.id
        val c = _counts.getOrElse(id, 0)
        if (c == 0 && r.live :: Live)
          _liveVisibleReqs += 1
        _counts = _counts.updated(id, c + 1)
    }

    // Find expansions
    var _expandedReqs  = 0
    var _expansionRows = 0
    for (c <- _counts.values if c > 1) {
      _expandedReqs  += 1
      _expansionRows += c
    }

    TableStats(vs.filterDead,
      liveVisibleReqs  = _liveVisibleReqs,
      liveFilteredReqs = p.reqs.data.reqs.size - p.reqs.data.deadCount - _liveVisibleReqs,
      deadReqs         = p.reqs.data.deadCount,
      expandedReqs     = _expandedReqs,
      expansionRows    = _expansionRows,
      codeGroups       = _codeGroups)
  }

  // ===================================================================================================================
  def rowsForTable(vs: ViewSettings, p: Project, pt: PlainText.ForProject): Stream[Row] = {
    def maybe(cond: Boolean, f: EndoFn[Stream[Row]]): EndoFn[Stream[Row]] = if (cond) f else identity

    gather(vs, p) |>
      sort(vs, p, pt) |>
      consolidateAdjacentDups |>
      maybe(vs.viewReqCodesAsTree, addReqCodeTreeToRows)
  }
}
