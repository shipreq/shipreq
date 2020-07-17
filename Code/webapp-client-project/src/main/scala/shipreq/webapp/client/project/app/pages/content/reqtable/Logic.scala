package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.collection.{Factory, Iterable}
import scala.reflect.ClassTag
import scalaz.syntax.semigroup._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.filter.{CompiledFilter, Filter}
import shipreq.webapp.base.sort.FusedSorters
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.util.ReqCodeTreeItem

/*
 * Deletion complicates everything. See `Requirements/analysis-deletion.ods` for details.
 */
private[reqtable] object Logic {
  import DataLogic.TagLookup
  import MTrie.Ops

  // ===================================================================================================================
  // Expansion

  /**
    * Nothing to expand = [ [] ]
    * Don't expand      = [ [a,b,c] ]
    * Expanded          = [ [a], [b], [c] ]
    */
  private case class Expanded[+A](exp: NonEmptyVector[Vector[A]], original: Vector[A]) {
    def isEmpty: Boolean =
      exp.head.isEmpty && exp.tail.isEmpty

    def expansions: NonEmptyVector[Expansion[A]] =
      exp.map(Expansion(_, original))
  }

  private object Expanded {

    val empty: Expanded[Nothing] =
      Expanded(NonEmptyVector.one(Vector.empty), Vector.empty)

    /**
     * [[UnivEq]] is required just as an extra check because [[Expander]] provides a `Set[A]`.
     *
     * @param visible Is the column visible? If not, just pretend everything is empty.
     * @param expand  When visible, does the data need to be expanded?
     */
    private def build[A: UnivEq](visible: Boolean, expand: => Boolean, ordering: => Option[Ordering[A]]): Expander[A] = {
      @inline def nonEmpty(f: (A, Vector[A]) => NonEmptyVector[Vector[A]]): Expander[A] =
        dataFn => {
          val set = dataFn()
          if (set.isEmpty)
            empty
          else {
            val vec =
              ordering match {
                case Some(ord) => MutableArray(set).sort(ord).iterator().toVector
                case None      => set.toVector
              }
            val exp = f(vec.head, vec.tail)
            Expanded(exp, vec)
          }
        }

      def doExpand: Expander[A] =
        nonEmpty((h, t) => NonEmptyVector(Vector1(h), t map Vector1))

      def dontExpand: Expander[A] =
        nonEmpty((h, t) => NonEmptyVector.one(h +: t))

      if (visible) {
        if (expand) doExpand else dontExpand
      } else
        _ => empty
    }

    def forColumn[A: UnivEq](view: View, c: Column.SortInconclusive, ordering: => Option[Ordering[A]]): Expander[A] = {
      lazy val orderCriteria = view.order.init.find(_.column ==* c)
      build(
        view.isVisible(c),
        orderCriteria.isDefined,
        if (orderCriteria.exists(_.method.descending)) ordering.map(_.reverse) else ordering)
    }
  }

  private type Expander[A] = (() => Set[A]) => Expanded[A]

  private final val emptyExpansions: NonEmptyVector[Expansions] =
    NonEmptyVector.one(Expansions.empty)

  private def fieldExpander[A: UnivEq](view    : View,
                                       col     : Column.SortInconclusive,
                                       ap      : ProjectApplicability[Column, ReqTypeId],
                                       dataFn  : ReqId => Set[A],
                                       ordering: => Option[Ordering[A]]): Req => Expanded[A] = {
    val expander = Expanded.forColumn[A](view, col, ordering)
    r => expander(() =>
      ap(r.reqTypeId, col) match {
        case Applicable    => dataFn(r.id)
        case NotApplicable => UnivEq.emptySet[A]
      })
  }

  private def customFieldExpander[K <: CustomFieldId : ClassTag, V: UnivEq](view: View,
                                                                            ap: ProjectApplicability[Column, ReqTypeId],
                                                                            f: K => ReqId => Set[V],
                                                                            ordering: => Option[Ordering[V]]): Req => Map[K, Expanded[V]] = {

    val cols = view.columns.whole.collect { case c@ Column.CustomField(id: K) => (id, c) }

    val expandersPerCol = cols.map { ct =>
        val colId  = ct._1
        val col    = ct._2
        val dataFn = f(colId)
        val fn     = fieldExpander(view, col, ap, dataFn, ordering)
        (colId, fn)
      }.toMap

    req => expandersPerCol.mapValuesNow(_(req))
  }

  private def impExpander(view: View,
                          fd  : FilterDead,
                          p   : Project,
                          ap  : ProjectApplicability[Column, ReqTypeId]): Req => Map[CustomField.Implication.Id, Expanded[Pubid]] =
    customFieldExpander(
      view,
      ap,
      p.dataLogic.customFieldImps(fd)(_).getPubids,
      Some(Sorter.orderingForImpField(p.dataLogic)))

  private def impExpander(dir      : Direction,
                          view     : View,
                          dataLogic: DataLogic): Expander[Pubid] =
    Expanded.forColumn[Pubid](
      view,
      Column.Implications(dir),
      Some(Sorter.orderingForImpField(dataLogic)))

  private def tagFieldExpander(view        : View,
                                    ap          : ProjectApplicability[Column, ReqTypeId],
                                    dataLogic   : DataLogic,
                                    tagFieldDist: TagFieldDistribution.TagIds,
                                    tagLookup   : TagLookup): Req => Map[CustomField.Tag.Id, Expanded[ApplicableTagId]] =
    customFieldExpander(
      view,
      ap,
      fid => DataLogic.customFieldTags(tagFieldDist, tagLookup, fid),
      Some(Sorter.orderingForTagField(dataLogic)))

  private def otherTagsExpander(view        : View,
                                     ap          : ProjectApplicability[Column, ReqTypeId],
                                     dataLogic   : DataLogic,
                                     tagFieldDist: TagFieldDistribution.TagIds,
                                     tagLookup   : TagLookup): Req => Expanded[ApplicableTagId] = {
    fieldExpander(
      view,
      Column.OtherTags,
      ap,
      DataLogic.otherTags(tagFieldDist, tagLookup),
      Some(Sorter.orderingForOtherTags(dataLogic)))
  }

  private def allTagsExpander(view     : View,
                                   ap       : ProjectApplicability[Column, ReqTypeId],
                                   dataLogic: DataLogic,
                                   tagLookup: TagLookup): Req => Expanded[ApplicableTagId] = {
    fieldExpander(
      view,
      Column.AllTags,
      ap,
      tagLookup(_).all,
      Some(Sorter.orderingForAllTags(dataLogic)))
  }

  private def codeExpander(view: View): Expander[ReqCode.Value] =
    Expanded.forColumn[ReqCode.Value](
      view,
      Column.Code,
      None)

  private def expandMapValues[K, V](src: Map[K, Expanded[V]]): NonEmptyVector[Map[K, Expansion[V]]] = {
    type M = Map[K, Expansion[V]]
    def go(keys: Vector[K], cur: M, r: Vector[M]): NonEmptyVector[M] =
      if (keys.isEmpty)
        NonEmptyVector(cur, r)
      else {
        val k  = keys.head
        val ks = keys.tail
        @inline def next(ms: Vector[M], v: Expansion[V]) = go(ks, cur.updated(k, v), ms)
        src(k).expansions.foldMapLeft1(next(r, _))((r2, v) => next(r2.whole, v))
      }
    go(src.keys.toVector, Map.empty, Vector.empty)
  }

  private def expansions(imps     : Direction.Values[Expanded[Pubid]],
                         codes    : Expanded[ReqCode.Value],
                         cfImps   : Map[CustomField.Implication.Id, Expanded[Pubid]],
                         cfTags   : Map[CustomField.Tag.Id,         Expanded[ApplicableTagId]],
                         otherTags: Expanded[ApplicableTagId],
                         allTags  : Expanded[ApplicableTagId],
                        ): NonEmptyVector[Expansions] =
    if (   codes.isEmpty
        && imps(Backwards).isEmpty
        && imps(Forwards).isEmpty
        && otherTags.isEmpty
        && allTags.isEmpty
        && cfImps.values.forall(_.isEmpty)
        && cfTags.values.forall(_.isEmpty)
    )
      emptyExpansions
    else
      for {
        a <- imps(Backwards).expansions
        b <- imps(Forwards).expansions
        c <- codes.expansions
        d <- expandMapValues(cfImps)
        e <- expandMapValues(cfTags)
        f <- otherTags.expansions
        g <- allTags.expansions
      } yield {
        val imps2 = Direction.Values {
          case Backwards => a
          case Forwards  => b
        }
        Expansions(
          implications = imps2,
          reqCodes     = c,
          reqCodeTree  = Expansion.empty,
          cfImps       = d,
          cfTags       = e,
          otherTags    = f,
          allTags      = g,
        )
      }

  // ===================================================================================================================
  // Gathering

  /**
   * Gathers [[Row]]s for display in [[Table]].
   * Performs expansion.
   * Does not perform any sorting with the exception of expansion values.
   */
  def gather[C[_]](p             : Project,
                   view          : View,
                   filterCompiler: Filter.Valid.Compiler)
                  (implicit cbf: Factory[Row, C[Row]]): C[Row] = {

    // NOTES:
    //
    // * Column.ImplicationSrc isn't transitive; custom implication columns are.
    //   There can potentially be overlap but culling this could be misleading.

    val dataLogic       = p.dataLogic
    val fd              = view.filterDead
    val filterDeadReq   = fd.filterFn.contramap[Req](_ live p.config.reqTypes)
    val filterDeadRCG   = fd.filterFn.contramap[CodeGroup](_.live)
    val filterDead      = CompiledFilter(filterDeadReq, filterDeadRCG, OptionalBoolFn.empty)
    val tagFieldDist    = DataLogic.tagFieldDist(p.config, fd, Some(f => view isVisible Column.CustomField(f)))
    val tagLookup       = dataLogic.tagLookup(fd)
    val applicability   = Column.applicabilityForReq(p.config.applicability)
    val expandImps      = Direction.memo(impExpander(_, view, dataLogic))
    val expandCodes     = codeExpander(view)
    val expandImpCols   = impExpander(view, fd, p, applicability)
    val expandTagCols   = tagFieldExpander(view, applicability, dataLogic, tagFieldDist, tagLookup)
    val expandOtherTags = otherTagsExpander(view, applicability, dataLogic, tagFieldDist, tagLookup)
    val expandAllTags   = allTagsExpander(view, applicability, dataLogic, tagLookup)

    // The segregation of live/dead is because live reqs can have inactive reqcodes (leftovers of CodeRefs).
    // It would be erroneous to display inactive reqs for a live req.
    val reqCodesByReq = Live.memo {
      case Live =>
        p.content.reqCodes.activeReqCodesByReqId
      case Dead =>
        UnivEq.emptySetMultimap[ReqId, ReqCode.Value] ++
          p.content.reqCodes.inactiveIdsByReqId.m.mapValuesNow(_ map p.content.reqCodes.reqCode)
    }

    val pImplications = p.content.implications

    def pubid(reqId: ReqId): Option[Pubid] = {
      val req = p.content.reqs.need(reqId)
      if (filterDead.req(req))
        Some(req.pubid)
      else
        None
    }

    def pubids(s: Set[ReqId]): Set[Pubid] =
      s.foldLeft(UnivEq.emptySet[Pubid])((q, id) =>
        pubid(id).fold(q)(q + _))

    val opOpFilter: Option[CompiledFilter] =
      view.filter.map(filterCompiler)

    /** Full = the cumulative result off all factors that would contribute to potentially filter content. */
    val fullFilter: CompiledFilter =
      opOpFilter.fold(filterDead)(filterDead && _)

    /** Was a filter expression used (i.e. a filter that isn't FilterDead) */
    def filterExprUsed = opOpFilter.exists(_.nonEmpty)

    /** When a filter expression is present, we still want to show relevant CodeGroups of visible rows. */
    val restoreFilteredRCGs = view.viewCodeGroups && filterExprUsed

    // Create rows
    val rows = {
      val output              = cbf.newBuilder
      val restorableRCGs      = DataLog.list[Row.ForCodeGroup].disableUnless(restoreFilteredRCGs)
      val codesSeen           = DataLog.mtrie[ReqCode.Node].disableUnless(restoreFilteredRCGs)
      val seeExpandedCodes    = codesSeen.addFn[Expanded[ReqCode.Value]](add => _.exp.foreach(_ foreach add))
      val fieldRulesByReqType = p.config.fieldRules(fd)

      // Add requirements
      for (r <- p.content.reqs.reqIterator())
        if (fullFilter.req(r)) {
          val id         = r.id
          val live       = r live p.config.reqTypes
          val fieldRules = fieldRulesByReqType(r.reqTypeId)

          // Expansion
          val imps      = Direction.Values(dir => expandImps(dir)(() => pImplications(dir)(id) |> pubids))
          val codes     = expandCodes  (() => reqCodesByReq(live)(id))
          val cfImps    = expandImpCols(r)
          val cfTags    = expandTagCols(r)
          val otherTags = expandOtherTags(r)
          val allTags   = expandAllTags(r)
          val exps      = expansions(imps, codes, cfImps, cfTags, otherTags, allTags)

          // Build
          exps.foreachWithIndex((exp, i) =>
            output += Row.ForReq(r, live, p.invalidTagsPerReq(id), exp, fieldRules, i))

          seeExpandedCodes(codes)
        }

      // Add CodeGroups
      if (view.viewCodeGroups)
        for (g <- p.content.reqCodes.groups) {
          val code = p.content.reqCodes reqCode g.id
          val row = Row.ForCodeGroup(g, code, None)
          if (fullFilter.codeGroup(g)) {
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

      output.result()
    }

    rows
  }

  // ===================================================================================================================
  // Sorting

  def sorter(p: Project, view: View, pt: PlainText.ForProject.NoCtx): IterableOnce[Row] => MutableArray[Row] = {
    val init   = view.order.init map Sorter.inconclusive
    val last   = view.order.last |> Sorter.conclusive
    val sorter = new FusedSorters(NonEmptyVector.end(init, last))
    val setup  = new Sorter.Setup(p, pt)
    sorter.result(setup, None)
  }

  // ===================================================================================================================
  // Post-processing

  def mergeAdjacent[A, C[_]](input: Iterator[A])(merge: (A, A) => Option[A])
                            (implicit cbf: Factory[A, C[A]]): C[A] = {

    val results = cbf.newBuilder
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

  def consolidateAdjacentDups[C[_]](rows: Iterator[Row])(implicit cbf: Factory[Row, C[Row]]): C[Row] =
    mergeAdjacent(rows)((x, y) =>
      (x, y) match {
        case (a: Row.ForReq, b: Row.ForReq) if a.req.id ==* b.req.id =>
          Some(Row.ForReq(
            req         = a.req,
            live        = a.live,
            invalidTags = a.invalidTags,
            exp         = a.exp |+| b.exp,
            fieldRules  = a.fieldRules,
            instanceId  = a.instanceId min b.instanceId))
        case _ =>
          None
      }
    )

  /**
   * Map with history.
   */
  def hmap[I, V[x] <: Iterable[x], A >: Null <: AnyRef, B >: Null, S[_], O]
      (input  : Iterable[I],
       extract: I => V[A],
       put    : (I, V[B]) => O,
       firstA : A => B,
       foldA  : (A, B, A) => B)
      (implicit cbfV: Factory[B, V[B]], cbfS: Factory[O, S[O]]): S[O] = {
    var lastA: A = null
    var lastB: B = null
    val bs = cbfS.newBuilder
    for (i <- input) {
      val bv = cbfV.newBuilder
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

  def mkReqCodeTree[I, V[x] <: Iterable[x], S[_], O]
      (input  : Iterable[I],
       extract: I => V[ReqCode.Value],
       put    : (I, V[ReqCodeTreeItem]) => O)
      (implicit cbfV: Factory[ReqCodeTreeItem, V[ReqCodeTreeItem]], cbfS: Factory[O, S[O]])
      : S[O] =
    hmap[I, V, ReqCode.Value, ReqCodeTreeItem, S, O](
      input, extract, put,
      c => ReqCodeTreeItem(Vector.empty, c),
      mkReqCodeTreeItem)

  def mkReqCodeTreeItem(prevV: ReqCode.Value, prevI: ReqCodeTreeItem, cur: ReqCode.Value): ReqCodeTreeItem = {
    import ReqCodeTreeItem._
    import VectorCase.{Empty, NonEmpty}

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


  def stats(p: Project, rows: IterableOnce[Row]): TableContentStats = {

    // Scan rows
    var codeGroups        = 0
    var rowsByReq         = UnivEq.emptyMap[ReqId, Int]
    val uniqueReqsInTable = LiveDeadStat.Builder.ofInts()
    rows.iterator foreach {
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
    val liveReqsInProject = p.content.reqs.size - deadReqsInProject

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

  // TODO Use Px in Logic.rowsForTable
  // Thinks like tag/issue lookup, filter compiler, etc can be cached by their own dependencies rather than
  // holistically externally

  def rowsForTable(p : Project,
                   v : View,
                   pt: PlainText.ForProject.NoCtx,
                   fc: Filter.Valid.Compiler): Vector[Row] = {

    def r1: Array       [Row] = gather(p, v, fc)
    def r2: MutableArray[Row] = sorter(p, v, pt)(r1)
    val r3: Vector      [Row] = consolidateAdjacentDups(r2.iterator())
    val r4: Vector      [Row] = if (v.viewReqCodesAsTree) addReqCodeTreeToRows(r3) else r3
    r4
  }
}
