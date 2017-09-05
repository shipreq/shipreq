package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.annotation.tailrec
import scala.collection.Traversable
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scalaz.std.anyVal.intInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.equal._
import scalaz.syntax.semigroup._
import scalaz.syntax.traverse1._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.ValidFilter
import shipreq.webapp.base.text.Atom.AnyIssue
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.util.ReqCodeTreeItem
import DataImplicits._
import DataLogic.{ReqTags, TagLookup}
import MTrie.Ops
import Debug._

/*
 * Deletion complicates everything. See `Requirements/analysis-deletion.ods` for details.
 */
private[reqtable] object Logic {

  private final class IssueLookup(p: Project, fd: FilterDead) {
    import AtomScan._

    type Issues = Vector[AnyIssue]

    private val get = fd.ldStatAccessor[Issues]

    private def forLoc(loc: IssueLoc): Issues =
      get(p.atomScan.issues(loc))

    def forReq(id: ReqId): Issues =
      forLoc(InReq(id))

    def forReqCode(id: ReqCodeId): Issues =
      forLoc(InRCG(id))
  }

  private def issueLookup(p: Project, fd: FilterDead): IssueLookup =
    new IssueLookup(p, fd)

  // ===================================================================================================================
  // Expansion

  private type Expanded[A] = NonEmptyVector[Vector[A]]
  private type Expander[A] = (() => Set[A]) => Expanded[A]

  private final val emptyExpansions: NonEmptyVector[Expansion] =
    NonEmptyVector.one(Expansion.empty)

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

  private def expanderC[A](ts: TableSettings, c: Column.SortInconclusive): Expander[A] =
    expander(ts isVisible c, ts isOrderedI c)

  private def impColValueFn(p: Project, fd: FilterDead): CustomField.Implication.Id => ReqId => Set[Pubid] = {
    val filter = DataLogic.impValueFilter(p.config, fd)
    DataLogic.customFieldImps(p, filter)
  }

  private def impColValueExpander(ts: TableSettings,
                                  fd: FilterDead,
                                  p : Project,
                                  ap: Applicability[Column, ReqTypeId]): Req => Map[CustomField.Implication.Id, Expanded[Pubid]] =
    customFieldExpander(ts, ap, impColValueFn(p, fd))

  private def tagFieldValueExpander(ts          : TableSettings,
                                    ap          : Applicability[Column, ReqTypeId],
                                    tagFieldDist: TagFieldDistribution.TagIds,
                                    tagLookup   : TagLookup): Req => Map[CustomField.Tag.Id, Expanded[ApplicableTagId]] =
    customFieldExpander(ts, ap, fid => DataLogic.customFieldTags(tagFieldDist, tagLookup, fid))

  private def customFieldExpander[K <: CustomFieldId : ClassTag, V: UnivEq]
      (vs: TableSettings,
       ap: Applicability[Column, ReqTypeId],
       f : K => ReqId => Set[V]): Req => Map[K, Expanded[V]] = {

    val cols = vs.columns.whole.collect { case c@ Column.CustomField(id: K) => (id, c) }

    val expandersPerCol = cols.map { ct =>
        val colId    = ct._1
        val col      = ct._2
        val expander = expanderC[V](vs, col)
        val dataFn   = f(colId)
        val fn       = (r: Req) => expander(() =>
          ap(r.reqTypeId, col) match {
            case Applicable    => dataFn(r.id)
            case NotApplicable => UnivEq.emptySet[V]
          })
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

  private def expansions(imps   : Direction.Values[Expanded[Pubid]],
                         codes  : Expanded[ReqCode.Value],
                         cfImps : Map[CustomField.Implication.Id, Expanded[Pubid]],
                         cfTags : Map[CustomField.Tag.Id,         Expanded[ApplicableTagId]]): NonEmptyVector[Expansion] =
    if (   isEmptyExp(codes)
        && isEmptyExp(imps(Backwards))
        && isEmptyExp(imps(Forwards))
        && cfImps.values.forall(isEmptyExp)
        && cfTags.values.forall(isEmptyExp))
      emptyExpansions
    else
      for {
        a <- imps(Backwards)
        b <- imps(Forwards)
        c <- codes
        d <- expandMapValues(cfImps)
        e <- expandMapValues(cfTags)
      } yield {
        val imps2 = Direction.Values {
          case Backwards => a
          case Forwards  => b
        }
        Expansion(imps2, c, Vector.empty[ReqCodeTreeItem], d, e)
      }

  // ===================================================================================================================
  // MultiValues

  private def multiValuesFn(tagFieldDist: TagFieldDistribution.TagIds,
                            tagLookup   : TagLookup): ReqId => MultiValues = {
    val tagValuesFn = DataLogic.generalTags(tagFieldDist, tagLookup)
    id => {
      val tags = tagValuesFn(id).toVector
      MultiValues(tags)
    }
  }

  // ===================================================================================================================
  // Gathering

  /**
   * Gathers [[Row]]s for display in [[Table]].
   * Performs expansion.
   * Does not perform any sorting.
   */
  def gather[C[_]](p : Project,
                   s : TableSettings,
                   fd: FilterDead,
                   pt: PlainText.ForProject.NoCtx,
                   ts: TextSearch)
                  (implicit cbf: CanBuildFrom[Nothing, Row, C[Row]]): C[Row] = {

    // NOTES:
    //
    // * Column.ImplicationSrc isn't transitive; custom implication columns are.
    //   There can potentially be overlap but culling this could be misleading.
    //
    // * The Tags column is not expanded. Only custom tag columns are.

    val filterDeadReq = fd.filterFnBy[Req](_ live p.config.reqTypes)
    val filterDeadRCG = fd.filterFnBy[CodeGroup](_.live)
    val filterDead    = Filters(filterDeadReq, filterDeadRCG)
    val tagFieldDist  = DataLogic.tagFieldDist(p.config, fd, s isVisible Column.CustomField(_))
    val tagLookup     = DataLogic.tagLookup(p, fd)
    val issueLookup   = this.issueLookup(p, fd)
    val applicability = Column.applicabilityForReq(p.config.applicability)
    val expandImps    = Direction.memo(dir => expanderC[Pubid](s, Column.Implications(dir)))
    val expandCodes   = expanderC[ReqCode.Value](s, Column.Code)
    val expandImpCols = impColValueExpander(s, fd, p, applicability)
    val expandTagCols = tagFieldValueExpander(s, applicability, tagFieldDist, tagLookup)

    // The segregation of live/dead is because live reqs can have inactive reqcodes (leftovers of CodeRefs).
    // It would be erroneous to display inactive reqs for a live req.
    val reqCodesByReq = Live.memo {
      case Live =>
        p.reqCodes.activeReqCodesByReqId
      case Dead =>
        UnivEq.emptySetMultimap[ReqId, ReqCode.Value] ++
          p.reqCodes.inactiveIdsByReqId.m.mapValuesNow(_ map p.reqCodes.reqCode)
    }

    val pImplications = p.implications
    val multiValuesFn = this.multiValuesFn(tagFieldDist, tagLookup)

    def pubid(reqId: ReqId): Option[Pubid] = {
      val req = p.reqs.need(reqId)
      if (filterDead fa req)
        Some(req.pubid)
      else
        None
    }

    def pubids(s: Set[ReqId]): Set[Pubid] =
      s.foldLeft(UnivEq.emptySet[Pubid])((q, id) =>
        pubid(id).fold(q)(q + _))

    val opOpFilter = s.filter.map(filter(_, p, pt, ts, issueLookup, tagLookup))

    /**
     * Full = the cumulative result off all factors that would contribute to potentially filter content.
     *
     * A result of `None` here means the filter has ruled out everything.
     */
    val fullFilter: Option[Filters] =
      opOpFilter match {
        case None               => Some(filterDead)
        case Some(Some(filter)) => Some(filterDead && filter)
        case Some(None)         => None
      }

    /** Was a filter expression used (i.e. a filter that isn't FilterDead) */
    def filterExprUsed = opOpFilter.exists(_.isDefined)

    /** When a filter expression is present, we still want to show relevant CodeGroups of visible rows. */
    val restoreFilteredRCGs = s.viewCodeGroups && filterExprUsed

    // Create rows
    val output = cbf()
    fullFilter.foreach { filter =>
      val restorableRCGs   = DataLog.list[Row.ForCodeGroup].disableUnless(restoreFilteredRCGs)
      val codesSeen        = DataLog.mtrie[ReqCode.Node].disableUnless(restoreFilteredRCGs)
      val seeExpandedCodes = codesSeen.addFn[Expanded[ReqCode.Value]](add => _.foreach(_ foreach add))

      // Add requirements
      for (r <- p.reqs.reqIterator)
        if (filter fa r) {
          val id = r.id
          val live = r live p.config.reqTypes

          // Expansion
          val imps    = Direction.Values(dir => expandImps(dir)(() => pImplications(dir)(id) |> pubids))
          val codes   = expandCodes  (() => reqCodesByReq(live)(id))
          val cfImps  = expandImpCols(r)
          val cfTags  = expandTagCols(r)
          val exps    = expansions(imps, codes, cfImps, cfTags)

          // Build
          val mv = multiValuesFn(id)
          exps.foreachWithIndex((exp, i) =>
            output += Row.ForReq(r, live, exp, mv, i))

          seeExpandedCodes(codes)
        }

      // Add CodeGroups
      if (s.viewCodeGroups)
        for (g <- p.reqCodes.groups) {
          val code = p.reqCodes reqCode g.id
          val row = Row.ForCodeGroup(g, code, None)
          if (filter fb g) {
            codesSeen.add(row.reqCode)
            output += row
          } else
            if (filterDeadRCG(g))
              restorableRCGs.add(row)
        }

      // Add back filtered out CodeGroups
      if (restoreFilteredRCGs) {
        val visTrie = codesSeen.get()
        for (row <- restorableRCGs.get())
          if (visTrie.dropPath(row.reqCode).nonEmpty)
            output += row
      }
    }
    output.result()
  }

  // ===================================================================================================================
  //  Filtering

  type Filters = FilterFn.Pair[Req, CodeGroup]

  @inline def Filters(req      : Req       => Boolean = FilterFn.`n/a`,
                      codeGroup: CodeGroup => Boolean = FilterFn.`n/a`): Filters =
    FilterFn.Pair(req, codeGroup)

  /**
   * @return None means filter everything out. Function const false. Fail-early to an empty set. No results.
   */
  def filter(vf         : ValidFilter,
             p          : Project,
             pt         : PlainText.ForProject.NoCtx,
             ts         : TextSearch,
             issueLookup: IssueLookup,
             tagLookup  : TagLookup): Option[Filters] = {

    import ValidFilter._, Attr.{AnyIssue, AnyTag}
    type F  = Filters
    type R  = Option[Filters]
    type FR = Req => Boolean
    type FG = CodeGroup => Boolean
    @inline implicit def autoSomeFilter(f: Filters): R = Some(f)

    // Possible optimisations:
    // - overlap between has Tag & Presence/Lack(AnyTag)
    // - overlap between has Issue & Presence/Lack(AnyIssue)
    // - overlap between WholeType & SomeOfType
    // - cycle in ImpliesAnyOf & ImpliedByAnyOf is impossible to satisfy
    // - lack & presence of α
    // - AnyOf with 2 contradictions = always pass
    // - AllOf with 2 contradictions = always fail
    // - AnyOf stops when match found, AllOf stops when non-match found. DeMorgan to the faster case.
    // - Remove duplicates
    // - Implications in AnyOf can be merged "{implies:MF-1 implies:MF-2}" = "implies:MF-{1,2}"

    def interpretN(fs: Min2Set[ValidFilter], f: (F, F) => F): R =
      ValidFilter.orderFastestFirst(fs.toMin2Vector)
        .traverse(interpret)
        .map(_ reduce f)

    def byTag(f: Set[ApplicableTagId] => Boolean) =
      Filters(req = r => tagLookup(r.id) exists f)

    def byIssueType(f: Vector[AnyIssue] => Boolean) =
      Filters(
        r => f(issueLookup.forReq(r.id)),
        g => f(issueLookup.forReqCode(g.id)))

    def byImplication(reqs: ValidFilter.ReqIds, tc: TransitiveClosure[ReqId]): R = {
      val whitelist = reqs.foldLeft(Set.empty[ReqId])(_ ++ tc(_))
      if (whitelist.isEmpty)
        None
      else
        Filters(req = whitelist contains _.id)
    }

    def interpret(subj: ValidFilter): R =
      subj match {
        case Reqs(ids)            => Filters(req = r => ids.contains(r.id))
        case ReqType(rt)          => Filters(req = _.reqTypeId ==* rt)
        case Tag(tag)             => byTag(_ contains tag)
        case Presence(AnyTag)     => byTag(_.nonEmpty)
        case Presence(AnyIssue)   => byIssueType(_.nonEmpty)
        case CustomIssue(it)      => byIssueType(_.exists(_.typ ==* it))
        case Lack(a)              => interpret(Not(Presence(a)))
        case AllOf(as)            => interpretN(as, _ && _)
        case AnyOf(as)            => interpretN(as, _ || _)
        case Not(Not(expr))       => interpret(expr)
        case Not(expr)            => interpret(expr).map(!_)
        case ImpliesAnyOf(reqs)   => byImplication(reqs, p.implicationTgtToSrcTC)
        case ImpliedByAnyOf(reqs) => byImplication(reqs, p.implicationSrcToTgtTC)

        case Text(substr) =>
          val f = ts.ignoreCaseSingleSpaces.searchFilter(substr)
          f.contramap(_.id, _.id): F

        case TextPattern(pat) =>
          val m: String => Boolean = pat.matcher(_).matches
          Filters(
            r => {
              def title  = m(pt reqTitle r)
              def custom = p.config.liveCustomTextFields.exists(f => pt.customTextField(f.id)(r) exists m)
              title || custom
            },
            g => m(pt codeGroupTitle g))
      }

    interpret(vf)
  }

  // ===================================================================================================================
  // Sorting

  def sort(p: Project, ts: TableSettings, pt: PlainText.ForProject.NoCtx)(rows: Iterable[Row]): MutableArray[Row] = {
    import Sorter._

    val sorter  = new FusedSorters(ts.order.init map inconclusive, ts.order.last |> conclusive)
    val setup   = new Setup(p, pt)
    val prepare = sorter.prepFn(setup)
    val rowMod  = Sorter.consolidateRowModFns(Sorter.sortUnspecified(ts) :: sorter.rowModFn :: Nil)
    val rowEndo = rowMod.map(_(setup, KeepDir)) getOrElse ((r: Row) => r)

    MutableArray.map(rows)(r => prepare(rowEndo(r)))
      .sort(sorter.sortFn.toOrdering)
      .map(sorter.row)
  }

  // ===================================================================================================================
  // Post-processing

  def mergeAdjacent[A, C[_]](input: Iterator[A])(merge: (A, A) => Option[A])
                            (implicit cbf: CanBuildFrom[Nothing, A, C[A]]): C[A] = {

    val results = cbf()
    if (input.hasNext) {
      @tailrec def go(prev: A): Unit = {
        if (input.isEmpty)
          results += prev
        else {
          val next = input.next()
          merge(prev, next) match {
            case None         => results += prev; go(next)
            case Some(merged) => go(merged)
          }
        }
      }
      go(input.next())
    }
    results.result()
  }

  def consolidateAdjacentDups[C[_]](rows: Iterator[Row])(implicit cbf: CanBuildFrom[Nothing, Row, C[Row]]): C[Row] =
    mergeAdjacent(rows)((x, y) =>
      (x, y) match {
        case (a: Row.ForReq, b: Row.ForReq) if a.req.id ==* b.req.id =>
          Some(Row.ForReq(a.req, a.live, a.exp |+| b.exp, a.mv |+| b.mv, a.instanceId min b.instanceId)) // TODO resort
        case _ =>
          None
      }
    )

  /**
   * Map with history.
   */
  def hmap[I, V[x] <: Traversable[x], A >: Null <: AnyRef, B >: Null, S[_], O]
      (input  : Traversable[I],
       extract: I => V[A],
       put    : (I, V[B]) => O,
       firstA : A => B,
       foldA  : (A, B, A) => B)
      (implicit cbfV: CanBuildFrom[Nothing, B, V[B]], cbfS: CanBuildFrom[Nothing, O, S[O]]): S[O] = {
    var lastA: A = null
    var lastB: B = null
    val bs = cbfS()
    for (i <- input) {
      val bv = cbfV()
      for (a <- extract(i)) {
        val b = if (lastA eq null) firstA(a) else foldA(lastA, lastB, a)
        bv += b
        lastA = a
        lastB = b
      }
      bs += put(i, bv.result())
    }
    bs.result()
  }

  def mkReqCodeTree[I, V[x] <: Traversable[x], S[_], O]
      (input  : Traversable[I],
       extract: I => V[ReqCode.Value],
       put    : (I, V[ReqCodeTreeItem]) => O)
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
        case NonEmpty(nextc) if cv.head ==* pv.head =>
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

  val addReqCodeTreeToRows: EndoFn[Vector[Row]] = rows =>
    mkReqCodeTree[Row, Vector, Vector, Row](
      rows,
      Row.reqCodes.get,
      (row, items) => Row.reqCodeTree.set(items)(row))


  def stats(p: Project, rows: TraversableOnce[Row]): TableContentStats = {

    // Scan rows
    var codeGroups        = 0
    var rowsByReq         = UnivEq.emptyMap[ReqId, Int]
    val uniqueReqsInTable = LiveDeadStat.newBuilder[Int]
    rows foreach {
      case r: Row.ForReq =>
        val id = r.req.id
        val c = rowsByReq.getOrElse(id, 0)
        rowsByReq = rowsByReq.updated(id, c + 1)
        if (c == 0)
          uniqueReqsInTable.add(r.live, 1)
      case _: Row.ForCodeGroup =>
        codeGroups += 1
    }

    // Find expansions
    var expandedReqs  = 0
    var expansionRows = 0
    rowsByReq.valuesIterator.filter(_ > 1).foreach { c =>
      expandedReqs  += 1
      expansionRows += c
    }

    val deadReqsInProject = p.deadReqCount
    val liveReqsInProject = p.reqs.size - deadReqsInProject

    val reqsFilteredOut = LiveDeadStat(
      live = liveReqsInProject - uniqueReqsInTable.live,
      dead = deadReqsInProject - uniqueReqsInTable.dead)

    TableContentStats(
      uniqueReqsInTable = uniqueReqsInTable.result(),
      reqsFilteredOut   = reqsFilteredOut,
      expandedReqs      = expandedReqs,
      expansionRows     = expansionRows,
      codeGroups        = codeGroups)
  }

  // ===================================================================================================================
  def rowsForTable(p: Project,
                   s: TableSettings,
                   fd: FilterDead,
                   pt: PlainText.ForProject.NoCtx,
                   ts: TextSearch): Vector[Row] = {

    def r1: Array       [Row] = gather(p, s, fd, pt, ts)
    def r2: MutableArray[Row] = sort(p, s, pt)(r1)
    val r3: Vector      [Row] = consolidateAdjacentDups(r2.iterator)
    val r4: Vector      [Row] = if (s.viewReqCodesAsTree) addReqCodeTreeToRows(r3) else r3
    r4
  }
}
