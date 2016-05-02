package shipreq.webapp.client.app.reqtable

import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import nyaya.util.Multimap
import scalaz.{\/, \/-, -\/, Equal}
import scalaz.std.AllInstances._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, PlainText, Text, TextSearch, ProjectText}
import shipreq.webapp.client.app.reqtable.{SortCriterion => SC, Column => C}
import shipreq.webapp.client.test.ClientTestSettings._
import SortMethod._
import Sorter._
import Text.Equality._
import LogicTestUtil._

object LogicPropTest extends TestSuite {
  val nop = Eval.pass()

  case class LogicTests(vs: ViewSettings, p: Project) {
    val E = EvalOver(this)

    type S[A] = Stream[A]

    val expectVisible: ReqId => Boolean =
      if (vs.filterDead == HideDead)
        id => p.reqs.need(id).live(p.config.reqTypes) :: Live
      else
        _ => true

    val plainText   = PlainText(p, ProjectText.Context.None)
    val textSearch  = TextSearch(p, plainText)
    val gathered    = Logic.gather(vs, p, plainText, textSearch)
    val gatheredG   = gathered.filterT[ReqRow]
    val rowReqCodes = gathered.flatMap(codesInRow(_).toStream)
    val rowGReqIds  = gatheredG.map(_.req.id).toSet
    val srcGReqIds  = p.reqs.idIterator.filterT[GenericReqId].filter(expectVisible).toSet
    val finalRows   = Logic.rowsForTable(vs, p, plainText, textSearch)
    val tableStats  = Logic.stats(vs, p, finalRows)

    val expectedVisibleReqCodes = {
      val b = Set.newBuilder[ReqCode.Value]
      p.reqCodes.activeReqCodesByReqId.values.foreach(b ++= _)
      if (vs.viewReqCodeGroups)
        p.reqCodes.groups.foreach(g =>
          if ((g.live :: Live) || (vs.filterDead :: ShowDead))
            b += p.reqCodes.reqCode(g.id))
      b.result()
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Gathering

    def noEmptyAndNonEmptyReqCodesMixed = {
      val data: Stream[Vector[Vector[ReqCode.Value]]] =
        Multimap.empty[ReqId, Vector, Vector[ReqCode.Value]]
          .addPairs(gatheredG.map(r => (r.req.id, r.exp.reqCodes)): _*)
          .m.values.toStream
      E.forall(data)(l =>
        E.test("Either all empty or all non-empty", !(l.exists(_.isEmpty) && l.exists(_.nonEmpty))))
    }

    // TODO doesn't check expanded implications
    // expansions per expandable A
    //   - list A = req.A
    //   - if req.A then no rows without As

    // NOTE: ReqCodes *can* be duplicated. Imagine sorting by MF > Code.
    def reqCodeProps =
      E.test("", vs.isVisible(C.Code)) ==> (
        E.allPresent("all req codes are displayed", expectedVisibleReqCodes, rowReqCodes)
        ∧ noEmptyAndNonEmptyReqCodesMixed)

    def gather =
      ( E.distinct("Rows", gathered.toStream)
      ∧ E.allPresent("each generic req id has a row", srcGReqIds, rowGReqIds)
      ∧ reqCodeProps
      ) rename "Logic.gather"

    // -----------------------------------------------------------------------------------------------------------------
    // Sorting

    implicit def textOrd[T <: Atom.Base] =
      implicitly[Ordering[String]].on[T#OptionalText](t => plainText.format(Live, t).toLowerCase)

    def universalSort = {
      val revOrder  = vs.order.reverse
      val revCri    = vs.copy(order = revOrder)
      val sorted    = Logic.sort(vs, p, plainText)(gathered)
      val sortedV   = sorted.toVector
      def criRev    = E.equal("cri.rev.rev = cri", revOrder.reverse, vs.order)
      def sortTwice = E.equal("sort.sort = sort", Logic.sort(vs, p, plainText)(sortedV).toVector, sortedV)
      def sortRev   = reverseSortOnReverseCri(sorted, revCri)
      (criRev ∧ sortRev ∧ sortTwice) rename "Universal sort props"
    }

    def reverseSortOnReverseCri(origSorted: S[Row], revCri: ViewSettings): EvalL = {
      /*
      def rev[A](c: Column, l: Vector[A]): Vector[A] =
        if (revCri isOrdered c) l.reverse else l

      def revmap[K <: CustomFieldId : ClassTag, A](m: Map[K, Vector[A]]): Map[K, Vector[A]] =
        m.map{ case (k, v) => (k, rev(Column.CustomField(k), v)) }

      def reverseExpansion(exp: Expansion): Expansion = {
        val Expansion(impS, impT, codes, cfImps, cfTags) = exp
        Expansion(
          rev(C.ImplicationSrc, impS),
          rev(C.ImplicationTgt, impT),
          rev(C.Code, codes),
          revmap(cfImps),
          revmap(cfTags))
      }

      def reverseMultiValues(mv: MultiValues): MultiValues = {
        val MultiValues(tags) = mv
        MultiValues(rev(C.Tags, tags))
      }

      def reverseRows(rs: Vector[Row]): Vector[Row] =
        rs.reverse.map {
          case GenericReqRow(r, e, mv) => GenericReqRow(r, reverseExpansion(e), reverseMultiValues(mv))
        }

      val reversed = Logic.sort(revCri.order, p)(gathered)
      E.equal("sort(cri) = rev(sort(cri.rev))", reverseRows(reversed), origSorted)
      */
      E.pass
    }

    def sortCri(c: SC.Inconclusive): SortCriteria =
      this.vs.order.copy(init = Vector(c))

    def gatherOn(c: C.SortInconclusive, sc: SortCriteria): Vector[Row] =
      if (vs isVisible c)
        gathered
      else
        Logic.gather(ViewSettings(columnState(p, c), sc, vs.filter, vs.filterDead), p, plainText, textSearch)

    def sortCriAndGather(c: SC.Inconclusive) =
      sortCri(c).mapStrengthR(gatherOn(c.column, _))

    def sortBy(c: SC.Inconclusive) = {
      val (sc, input) = sortCriAndGather(c)
      Logic.sort(newViewSettingsForSort(sc), p, plainText)(input)
    }

    def newViewSettingsForSort(sc: SortCriteria): ViewSettings =
      vs.copy(order = sc)

    /** @return error \/ (blank, non-blank) */
    def separateBlanks[A](expectBlanksFirst: Boolean, asi: Iterable[A])(isBlank: A => Boolean): String \/ (List[A], List[A]) = asi.toList match {
      case Nil =>
        \/-(Nil, Nil)
      case as@ (h :: t) =>
        val firstBlockBlank = isBlank(h)
        val b1Cond: A => Boolean = if (firstBlockBlank) isBlank else !isBlank(_)
        val block1 = h :: t.takeWhile(b1Cond)
        val block2 = as drop block1.length
        val (b,nb) = if (firstBlockBlank) (block1, block2) else (block2, block1)
        def show = {
          val (s1,s2) = (block1,block2).mapEach(_.map(a => if (isBlank(a)) "." else "#").mkString(""))
          s"[$s1|$s2]"
        }
        def fail(e: String) = -\/(s"$e: $show")
        if (block2.isEmpty)
          if (firstBlockBlank) \/-(block1, Nil) else \/-(Nil, block1)
        else if (block2 exists b1Cond)
          fail("Blank and non-blanks not separated")
        else if (expectBlanksFirst != firstBlockBlank)
          fail(s"Blocks in wrong order")
        else
          \/-(b, nb)
    }

    def E_bnbBlocks[A](name: String, bp: BlankPlacement, as: Iterable[A])(isBlank: A => Boolean, f: (List[A], List[A]) => EvalL): EvalL = {
      val expectBlanksFirst = bp match {case BlanksFirst => true; case BlanksLast => false}
      E.either(s"$name make separate blank/non-blank blocks", separateBlanks(expectBlanksFirst, as)(isBlank))(f.tupled)
    }

    def E_sorted[A <: AnyRef](name: String, as: TraversableOnce[A], dirChange: Dir)(implicit ord: Ordering[A]): EvalL = {
      val actual = as.toVector
      val expect = dirChange(actual.sorted)(_.reverse)
      implicit val eq = Equal.equal[A]((a, b) => (a eq b) || (a == b) || ord.equiv(a, b)) // use of ord is slow - avoid
      E.equal(name + " are sorted", actual, expect)
    }

    type IndivSortCB = (ConsiderBlanks, BlankPlacement, Dir) => EvalL
    type IndivSortIB = (IgnoreBlanks  ,                 Dir) => EvalL

    def sortByPubid: IndivSortIB = (sm, dir) => {
      val sc     = SortCriteria(Vector.empty, SC.Conclusive(C.Pubid, sm))
      val sorted = Logic.sort(newViewSettingsForSort(sc), p, plainText)(gathered)
      val na     = ("", -1)
      val pubids = sorted.map {
        case r: ReqRow          => pubidExtract(p)(r.req.pubid)
        case r: ReqCodeGroupRow => na
      }
      E_sorted("Pubids", pubids, dir)
    }

    def sortByReqCode: IndivSortCB = (sm, bp, dir) => {
      val sorted     = sortBy(SC.InconclusiveCB(C.Code, sm))
      val data       = sorted map firstCodePerRow
      val name       = s"ReqCodes ($sm)"
      val intra      = sorted.map(codesInRow(_).toList).filter{case _ :: _ :: _ => true; case _ => false}.map(_ map PlainText.reqCode)
      def eachRow    = E.forall(intra)(E_sorted(s"Codes within a single row are sorted.", _, dir))
      def wholeTable = E_bnbBlocks(name, bp, data)(_.isEmpty, (_, nb) => E_sorted(name, nb, dir))
      (wholeTable ∧ eachRow) rename name
    }

    def sortByTitle: IndivSortCB = (sm, bp, dir) => {
      val name       = s"Title ($sm)"
      val sorted     = sortBy(SC.InconclusiveCB(C.Title, sm))
      val data       = sorted.map {
        case r: ReqRow          => r.req.title
        case r: ReqCodeGroupRow => r.group.title
      }
      E_bnbBlocks(name, bp, data)(_.isEmpty, (_, nb) => E_sorted(name, nb, dir))
    }

    def sortCB(t: IndivSortCB): EvalL =
      ( t(BlanksThenAsc,  BlanksFirst, KeepDir)
      ∧ t(AscThenBlanks,  BlanksLast,  KeepDir)
      ∧ t(BlanksThenDesc, BlanksFirst, FlipDir)
      ∧ t(DescThenBlanks, BlanksLast,  FlipDir))

    def sortIB(t: IndivSortIB): EvalL =
      t(Asc, KeepDir) ∧ t(Desc, FlipDir)

    // Let's make it real obvious what we're omitting or potentially forgetting
    def individualSort: C => EvalL = {
      case C.ReqType         => nop
      case C.Pubid           => sortIB(sortByPubid)
      case C.Code            => sortCB(sortByReqCode)
      case C.Title           => sortCB(sortByTitle)
      case C.Tags            => nop
      case C.ImplicationSrc  => nop
      case C.ImplicationTgt  => nop
      case C.DeletionReason  => nop
      case C.CustomField(id, _) =>
        id match {
          case i: CustomField.Implication.Id => nop
          case i: CustomField.Tag        .Id => nop
          case i: CustomField.Text       .Id => nop
        }
    }

    def individualSorts: EvalL =
      C.all(ProjectConfig.empty).map(individualSort).reduce(_ ∧ _)

    def sorting =
      (individualSorts ∧ universalSort) rename "Logic.sort"

    // -----------------------------------------------------------------------------------------------------------------
    def stats =
      E.equal("stats.visibleRows", tableStats.visibleRows, finalRows.size) ∧
      E.equal("stats.visibleReqs", tableStats.visibleReqs, rowGReqIds.size)

    def uniqueKeys =
      E.distinct("Row keys must be unique", finalRows.map(_.id.key))

    def all = gather ∧ sorting ∧ stats ∧ uniqueKeys
  }

  def gen: Gen[LogicTests] =
    for {
      p  <- RandomData.project
      vs <- RandomReqTableData.viewSettings(p, allowFilter = false)
    } yield
      LogicTests(vs, p)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all)//(implicitly[Settings].setSeed(0).setDebug.setSampleSize(20))
  }
}
