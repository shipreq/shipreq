package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.util.Multimap
import monocle.Optional
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaz.{\/, \/-, -\/, Equal}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import utest._

import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.FilterAst
import shipreq.webapp.base.text.{Atom => TextAtom, TextSearch, PlainText, Text}
import shipreq.webapp.base.test._, BaseTestUtil._
import shipreq.webapp.base.util.{Optics, ReqCodeTreeItem}
import shipreq.webapp.client.app.ui.reqtable.{SortCriterion => SC, Column => C}
import shipreq.webapp.client.lib.{FilterDead, ShowDead, HideDead}
import shipreq.webapp.client.test.ClientTestSettings._
import SortMethod._
import Sorter._
import Text.Equality._

object LogicTest extends TestSuite {

  private def codesInRow(r: Row): Vector[ReqCode.Value] =
    // Don't use optics here
    r match {
      case r: GenericReqRow   => r.exp.reqCodes
      case r: ReqCodeGroupRow => Vector1(r.reqCode)
    }

  private def tagsInRow(r: Row): Vector[ApplicableTagId] =
    // Don't use optics here
    r match {
      case r: GenericReqRow   => r.mv.tags
      case r: ReqCodeGroupRow => Vector.empty
    }

  def pubidExtract(p: Project)(pid: Pubid): (String, Int) =
    (p.reqType(pid.reqTypeId).fold(sys.error, _.mnemonic.value), pid.pos.value)

  def pubidToStr(p: Project)(pid: Pubid): String = {
    val (a, b) = pubidExtract(p)(pid)
    s"$a-$b"
  }

  def firstCodePerRow(r: Row): String = {
    val c = codesInRow(r)
    if (c.isEmpty) "" else PlainText.reqCode(c.head)
  }

  def applicableTag(p: Project): ApplicableTagId => ApplicableTag =
    id => p.tags.data.get(id).map(_.tag) match {
      case Some(t: ApplicableTag) => t
      case x => sys.error(s"Not an ApplicableTag: $x")
    }

  // ===================================================================================================================
  val nop = Eval.pass()

  case class LogicTests(vs: ViewSettings, p: Project) {
    val E = EvalOver(this)

    type S[A] = Stream[A]

    val expectVisible: ReqId => Boolean =
      if (vs.filterDead == HideDead)
        id => p.reqs.data.req(id).get.live ≟ Live
      else
        _ => true

    val plainText   = PlainText(p)
    val textSearch  = TextSearch(p, plainText)
    val gathered    = Logic.gather(vs, p, plainText, textSearch)
    val gatheredG   = gathered.filterT[GenericReqRow]
    val rowReqCodes = gathered.flatMap(codesInRow(_).toStream)
    val rowGReqIds  = gatheredG.map(_.req.id).toSet
    val srcGReqIds  = p.reqs.data.reqs.keys.filterT[GenericReqId].filter(expectVisible).toSet
    val finalRows   = Logic.rowsForTable(vs, p, plainText, textSearch)
    val tableStats  = Logic.stats(vs, p, finalRows)

    val expectedVisibleReqCodes =
      p.reqCodes.data.cataA(Set.empty[ReqCode.Value])((q, c, d) => d.target match {
        case id: ReqId       => if (expectVisible(id))    q + c else q
        case _: ReqCodeGroup => if (vs.viewReqCodeGroups) q + c else q
      })

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
      ( E.distinct("Rows", gathered)
      ∧ E.allPresent("each generic req id has a row", srcGReqIds, rowGReqIds)
      ∧ reqCodeProps
      ) rename "Logic.gather"

    // -----------------------------------------------------------------------------------------------------------------
    // Sorting

    implicit def textOrd[T <: Text.Generic] =
      implicitly[Ordering[String]].on[T#OptionalText](t => plainText.format(t).toLowerCase)

    def universalSort = {
      val revOrder  = vs.order.reverse
      val revCri    = vs.copy(order = revOrder)
      val sorted    = Logic.sort(vs, p, plainText)(gathered)
      def criRev    = E.equal("cri.rev.rev = cri", revOrder.reverse, vs.order)
      def sortTwice = E.equal("sort.sort = sort", Logic.sort(vs, p, plainText)(sorted.toStream), sorted)
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

    def gatherOn(c: C.SortInconclusive, sc: SortCriteria): Stream[Row] =
      if (vs isVisible c)
        gathered
      else
        Logic.gather(ViewSettings(NonEmptyVector(c), sc, vs.filter, vs.filterDead), p, plainText, textSearch)

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

    def E_sorted[A: Ordering: Equal](name: String, as: Iterable[A], dirChange: Dir): EvalL = {
      val ass = as.toStream
      E.equal(name + " are sorted", ass, dirChange(ass.sorted)(_.reverse))
    }

    type IndivSortCB = (ConsiderBlanks, BlankPlacement, Dir) => EvalL
    type IndivSortIB = (IgnoreBlanks  ,                 Dir) => EvalL

    def sortByPubid: IndivSortIB = (sm, dir) => {
      val sc     = SortCriteria(Vector.empty, SC.Conclusive(C.Pubid, sm))
      val sorted = Logic.sort(newViewSettingsForSort(sc), p, plainText)(gathered)
      val na     = ("", -1)
      val pubids = sorted.map {
        case r: GenericReqRow   => pubidExtract(p)(r.req.pubid)
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
      val sorted     = sortBy(SC.InconclusiveCB(C.Title, sm))
      val data       = sorted.map {
        case r: GenericReqRow   => r.req.title
        case r: ReqCodeGroupRow => r.group.title
      }
      val name       = s"Desc ($sm)"
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
      case C.CustomField(id, _) =>
        id match {
          case i: CustomField.Implication.Id => nop
          case i: CustomField.Tag        .Id => nop
          case i: CustomField.Text       .Id => nop
        }
    }

    def individualSorts: EvalL =
      C.all(None).map(individualSort).reduce(_ ∧ _)

    def sorting =
      (individualSorts ∧ universalSort) rename "Logic.sort"

    // -----------------------------------------------------------------------------------------------------------------
    def stats =
      E.equal("stats.visibleRows", tableStats.visibleRows, finalRows.size) ∧
      E.equal("stats.visibleReqs", tableStats.visibleReqs, rowGReqIds.size)

    def all = gather ∧ sorting ∧ stats
  }

  def gen: Gen[LogicTests] =
    for {
      p  <- RandomData.project
      vs <- RandomReqTableData.viewSettings(p, allowFilter = false)
    } yield
      LogicTests(vs, p)

  // ===================================================================================================================
  // Unit tests
  // Fucking IntelliJ crashes typing these tests inline

  object UnitTest {
    import ProjectDsl._
    import UnsafeTypes._
    import SampleProject.Values._

    private lazy val PD  = SampleProject.project
    private lazy val PA  = TestOptics.customReqTypesLive.set(Live)(PD)
    private      val sep = "  "
    private      val z   = "∅"
    private      val _z  = (_: Any) => z
    private type Rows    = Stream[Row]
    private type Filter  = Option[FilterAst]

    private case class PCache(p: Project, pt: PlainText.ForProject, ts: TextSearch)
    private var _pcache: List[PCache] = Nil
    private def pcache(p: Project): PCache =
      _pcache.find(_.p eq p).getOrElse {
        val pt = PlainText(p)
        val c = PCache(p, pt, TextSearch(p, pt))
        _pcache ::= c
        c
      }

    private def testUnsorted[A: Equal](p: Project, c: C, f: Filter, fd: FilterDead, extract: Rows => A)(expect: A): Unit =
      testUnsorted2(p, NonEmptyVector one c, f, fd, extract)(expect)

    private def testUnsorted2[A: Equal](p: Project, cs: NonEmptyVector[C], f: Filter, fd: FilterDead, extract: Rows => A)(expect: A): Unit = {
      val vs = ViewSettings(cs, SortCriteria.default.copy(init = Vector.empty), f, fd)
      val pc = pcache(p)
      import pc.{pt, ts}
      val r = Logic.gather(vs, p, pt, ts) |> Logic.sort(vs, p, pt) |> Logic.consolidateAdjacentDups
      assertEq(extract(r), expect)
    }

    private def vsSortedByCB(c: C.SortInconclusive with C.HasBlanks, sm: ConsiderBlanks, f: Filter, fd: FilterDead): ViewSettings =
      ViewSettings(NonEmptyVector(c), SortCriteria.default.copy(init = Vector(SC.InconclusiveCB(c, sm))), f, fd)

    private def testCB[A: Equal](p: Project, c: C.SortInconclusive with C.HasBlanks, f: Filter, fd: FilterDead, extract: Rows => A)(tests: Seq[(ConsiderBlanks, A)]) = {
      val pc = pcache(p)
      import pc.{pt, ts}
      for ((sm, expect) <- tests) {
        val vs = vsSortedByCB(c, sm, f, fd)
        val r = Logic.gather(vs, p, pt, ts) |> Logic.sort(vs, p, pt) |> Logic.consolidateAdjacentDups
        assertEq(sm.toString, extract(r), expect)
      }
    }

    private def allSortsCBA[A](z: A, zcount: Int)(f: (A, A) => A, asc: A, desc: A): Seq[(ConsiderBlanks, A)] = {
      if (zcount < 1) fail("zcount must be ≥ 1")
      val zz: A = if (zcount > 1) Stream.fill(zcount)(z).reduce(f) else z
      (BlanksThenAsc  -> f(zz, asc))  ::
      (AscThenBlanks  -> f(asc, zz))  ::
      (BlanksThenDesc -> f(zz, desc)) ::
      (DescThenBlanks -> f(desc, zz)) :: Nil
    }

    private def allSortsCB(zcount: Int, asc: String, desc: String): Seq[(ConsiderBlanks, String)] =
      allSortsCBA(z, zcount)(_ + sep + _, asc, desc)

    private def vsSortedByIB(c: C.SortInconclusive with C.NoBlanks, sm: IgnoreBlanks, f: Filter, fd: FilterDead): ViewSettings =
      ViewSettings(NonEmptyVector(c), SortCriteria.default.copy(init = Vector(SC.InconclusiveIB(c, sm))), f, fd)

    private def testIB[A: Equal](p: Project, c: C.SortInconclusive with C.NoBlanks, f: Filter, fd: FilterDead, extract: Rows => A)(tests: Seq[(IgnoreBlanks, A)]) = {
      val pc = pcache(p)
      import pc.{pt, ts}
      for ((sm, expect) <- tests) {
        val vs = vsSortedByIB(c, sm, f, fd)
        val r = Logic.gather(vs, p, pt, ts) |> Logic.sort(vs, p, pt) |> Logic.consolidateAdjacentDups
        assertEq(sm.toString, extract(r), expect)
      }
    }

    private def allSortsIB[A](asc: A, desc: A): Seq[(IgnoreBlanks, A)] =
      (Asc  -> asc) :: (Desc -> desc) :: Nil

    private def rowToStr(f: GenericReqRow => String, g: ReqCodeGroupRow => String): Row => String =
      rowToStr(f, g, identity)

    private def rowToStr(f: GenericReqRow => String, g: ReqCodeGroupRow => String, h: String => String): Row => String = {
      case r: GenericReqRow   => h(f(r))
      case r: ReqCodeGroupRow => h(g(r))
    }

    private def rowToAsToStr[A](f: GenericReqRow => Vector[A])(h: A => String): Row => String =
      rowToAsToStr(f, _ => Vector.empty)(h)

    private def rowToAsToStr[A](f: GenericReqRow => Vector[A], g: ReqCodeGroupRow => Vector[A])(h: A => String): Row => String = {
      val i = (_: Vector[A]).ifelse(_.isEmpty, _z, _ map h mkString ",")
      rowToStr(i compose f, i compose g)
    }

    private def rowToAsToStr2[A](f: GenericReqRow => Vector[A])(g: GenericReqRow => A => String) =
      rowToStr(r => f(r).ifelse(_.isEmpty, _z, _ map g(r) mkString ","), _z)

    private def rowToCustomText(pt: PlainText.ForProject, id: CustomField.Text.Id): Row => String = {
      val f = pt.customTextField(id)
      rowToStr(r => f(r.req.id) getOrElse z, _z)
    }

    private def rowToImpTxt(p: Project, lens: Optional[Row, Vector[Pubid]], dir: String): Row => String = {
      def fmtEach(s: Pubid, t: Pubid) = pubidToStr(p)(s) + dir + pubidToStr(p)(t)
      val f = rowToAsToStr2(lens.getOption(_) getOrElse Vector.empty)(r => fmtEach(_, r.req.pubid))
      impTxtConsolidate compose f
    }

    private val impTxtConsolidate: String => String = {
      val pubid = "[A-Z]+-\\d+"
      val pubids = s"$pubid(?:,$pubid)*"
      val imp = "[<>]"
      val regex = s"((?:^| )$pubids)($imp$pubid)(,$pubids($imp$pubid))".r
      @tailrec def go(s: String): String = {
        val t = regex.replaceAllIn(s, m => {
          if (m.group(2) == m.group(4))
            m.group(1) + m.group(3)
          else
            m.group(0)
        })
        if (s == t) s else go(t)
      }
      go
    }

    private def rowToSrcImpTxt(p: Project): Row => String =
      rowToImpTxt(p, Row.implicationSrc, ">")

    private def rowToTgtImpTxt(p: Project): Row => String =
      rowToImpTxt(p, Row.implicationTgt, "<")

    private def rowToCustomImpTxt(p: Project, id: CustomField.Implication.Id): Row => String =
      rowToImpTxt(p, Row.cfImp(id), ">")

    private def rowToTagTxt(p: Project, lens: Optional[Row, Vector[ApplicableTagId]]): Row => String = {
      val fmtEach = applicableTag(p).andThen(_.key.value)
      rowToAsToStr(lens.getOption(_) getOrElse Vector.empty)(fmtEach)
    }

    private def rowToPubid(p: Project): Row => String =
      rowToStr(_.req.pubid |> pubidToStr(p), _z)

    private def rowToStrAp2(a: Row => String, b: Row => String)(f: (String, String) => String): Row => String =
      r => f(a(r), b(r))

    private def prefixWithPubid(p: Project, f: Row => String): Row => String =
      rowToStrAp2(rowToPubid(p), f)((a, b) => if (b ≟ z) z else a + ":" + b)

    implicit private def rowFnToRowsFn(f: Row => String): Rows => String =
      _ map f mkString sep

    implicit private def customFieldToColumn(id: CustomFieldId) =
      C.CustomField(id, if (id == relField) Dead else Live)

    // -----------------------------------------------------------------------------------------------------------------

    val fieldSetL = Project.fields ^|-> RevAnd.data

    def modCustomFields(f: EndoFn[IMap[CustomFieldId, CustomField]]): EndoFn[Project] =
      fieldSetL.modify { fs =>
        val cf = f(fs.customFields)
        FieldSet(cf, StaticField.values.whole ++ cf.values.toVector.map(_.fieldId))
      }

    def clearCustomFields =
      modCustomFields(_ replaceUnderlying Map.empty)

    def testTags_sorted1(): Unit = {
      def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
      val p       = GReq().times(2) + t(2) + t(3) + t(11) + t(12) + t(11, 12) + t(12, 11) !! PA
      val p2      = clearCustomFields(p)
      val fmtRows = rowToTagTxt(p, Row.tags)
      // The Tags column is *not* expanded. Only custom tag columns are.
      testCB(p2, C.Tags, None, ShowDead, fmtRows)(allSortsCB(2,
        asc  = "defer  defer,wip  defer,wip  pri=high  pri=med  wip",
        desc = "wip,defer  wip,defer  wip  pri=med  pri=high  defer"))
    }

    def testTags_sorted2(): Unit = {
      def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
      val p       = GReq() + t(2) + t(11) + t(12) + t(11, 12, 2) + t(3, 12, 11) !! PA
      val p2      = modCustomFields(_.filterK(_ == priField))(p)
      val fmtRows = rowToTagTxt(p, Row.tags)
      // The Tags column is *not* expanded. Only custom tag columns are.
      testCB(p2, C.Tags, None, ShowDead, fmtRows)(allSortsCB(2,
        asc  = "defer  defer,wip  defer,wip  wip",
        desc = "wip,defer  wip,defer  wip  defer"))
    }

    /** When tags aren't being sorted by SortCriteria they should be sorted by some default. */
    def testTags_unsorted(): Unit = {
      def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
      val p       = t(11, 12, 2, 3, 4) ! PA
      val p2      = clearCustomFields(p)
      val fmtRows = rowToTagTxt(p, Row.tags)
      testUnsorted(p2, C.Tags, None, ShowDead, fmtRows)("defer,pri=high,pri=low,pri=med,wip")
    }

    def testCustomTagField_sorted1(): Unit = {
      def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
      val p       = GReq() + t(2) + t(3) + t(2, 3) + t(11, 12, 22, 24, 26) ! PA
      val fmtRows = prefixWithPubid(p, rowToTagTxt(p, Row cfTag priField))
      testCB(p, priField, None, ShowDead, fmtRows)(allSortsCB(2,
        asc  = "DD-2:pri=high  DD-4:pri=high  DD-3:pri=med  DD-4:pri=med",
        desc = "DD-3:pri=med  DD-4:pri=med  DD-2:pri=high  DD-4:pri=high"))
    }

    def testCustomTagField_sorted2(): Unit = {
      def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
      val p       = GReq() + t(2) + t(2, 3) + t(3) + t(11, 12, 22, 24, 26) ! PA
      val fmtRows = prefixWithPubid(p, rowToTagTxt(p, Row cfTag priField))
      testCB(p, priField, None, ShowDead, fmtRows)(allSortsCB(2,
        asc  = "DD-2:pri=high  DD-3:pri=high,pri=med  DD-4:pri=med",
        desc = "DD-3:pri=med  DD-4:pri=med  DD-2:pri=high  DD-3:pri=high"))
    }

    def testCustomTagField_unsorted(): Unit = {
      def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
      val p       = GReq() + t(2) + t(3) + t(2, 3, 4) + t(11, 12, 22, 24, 26) ! PA
      val fmtRows = rowToTagTxt(p, Row cfTag priField)
      testUnsorted(p, priField, None, ShowDead, fmtRows)(
        s"$z  pri=high  pri=med  pri=high,pri=med,pri=low  $z")
        // TODO s"$z  pri=high  pri=med  pri=high,pri=med  pri=high,pri=med  $z") + t(3, 2)
    }

    def testTitle(): Unit = {
      val p       = GReq() + GReq("AT") + GReq("and") + GReq("haha") + GReq("F") !! PA
      val pt      = pcache(p).pt
      val fmtRows = rowToStr(_.req |> pt.reqTitle, _.groupAndId |> pt.reqCodeGroupTitle, _.apif(_.isEmpty, _z))
      testCB(p, C.Title, None, ShowDead, fmtRows)(allSortsCB(1,
        asc  = "and  AT  F  haha",
        desc = "haha  F  AT  and"))
    }

    def testImpSrc(): Unit = {
      def t(_id: GenericReqId, ids: ReqId*) = GReq(id = _id, reqType = fr).impSrc(ids: _*)
      //      FR-1   FR-2      DD-1                            FR-3         FR-4      FR-5
      val p = t(1) + t(2, 1) + t(3, 1, 2).copy(reqType = dd) + t(4, 1, 3) + t(5, 3) + t(6, 5) ! PA
      testCB(p, C.ImplicationSrc, None, ShowDead, rowToSrcImpTxt(p))(allSortsCB(1,
        asc  = "DD-1>FR-3  DD-1>FR-4  FR-1>DD-1  FR-1>FR-2  FR-1>FR-3  FR-2>DD-1  FR-4>FR-5",
        desc = "FR-4>FR-5  FR-2,FR-1>DD-1  FR-1>FR-2  FR-1,DD-1>FR-3  DD-1>FR-4"))
    }

    def testImpTgt(): Unit = {
      def t(_id: GenericReqId, ids: ReqId*) = GReq(id = _id, reqType = fr).impTgt(ids: _*)
      //      FR-1   FR-2      DD-1                            FR-3         FR-4      FR-5
      val p = t(1) + t(2, 1) + t(3, 1, 2).copy(reqType = dd) + t(4, 1, 3) + t(5, 3) + t(6, 5) ! PA
      testCB(p, C.ImplicationTgt, None, ShowDead, rowToTgtImpTxt(p))(allSortsCB(1,
        asc  = "DD-1<FR-3  DD-1<FR-4  FR-1<DD-1  FR-1<FR-2  FR-1<FR-3  FR-2<DD-1  FR-4<FR-5",
        desc = "FR-4<FR-5  FR-2,FR-1<DD-1  FR-1<FR-2  FR-1,DD-1<FR-3  DD-1<FR-4"))
    }

    def testCustomImpField(): Unit = {
      // Expected MFs per row
      // MF-1 ⇐
      // MF-2 ⇐
      // MF-3 ⇐
      // MF-4 ⇐ 3
      // MF-5 ⇐ 3
      // BR-1 ⇐
      // BR-2 ⇐
      // FR-1 ⇐ 1
      // FR-2 ⇐ 1,2
      // FR-3 ⇐ 1,2
      // FR-4 ⇐ 3
      // FR-5 ⇐ 3
      // FR-6 ⇐ 3,4
      val p   = SampleImplicationGraph.project
      val fmt = rowToCustomImpTxt(p, mfField)
      testCB(p, mfField, None, ShowDead, fmt)(allSortsCB(2,
        asc  = """
                 |MF-1>FR-1
                 |MF-1>FR-2
                 |MF-1>FR-3
                 |MF-1>MF-1
                 |MF-2>FR-2
                 |MF-2>FR-3
                 |MF-2>MF-2
                 |MF-3>FR-4
                 |MF-3>FR-5
                 |MF-3>FR-6
                 |MF-3>MF-3
                 |MF-3>MF-4
                 |MF-3>MF-5
                 |MF-4>FR-6
                 |MF-4>MF-4
                 |MF-5>MF-5
               """.stripMargin.replace("\n", sep).trim,
        desc = """
                 |MF-5>MF-5
                 |MF-4>FR-6
                 |MF-4>MF-4
                 |MF-3>FR-4
                 |MF-3>FR-5
                 |MF-3>FR-6
                 |MF-3>MF-3
                 |MF-3>MF-4
                 |MF-3>MF-5
                 |MF-2>FR-2
                 |MF-2>FR-3
                 |MF-2>MF-2
                 |MF-1>FR-1
                 |MF-1>FR-2
                 |MF-1>FR-3
                 |MF-1>MF-1
               """.stripMargin.replace("\n", sep).trim))
    }

    def testReqType(): Unit = {
      def t(_reqTypeId: CustomReqTypeId) = GReq(reqType = _reqTypeId)
      val p = t(co) + t(co) + t(br) + t(br) + t(mf) + t(mf) + t(fr) + t(fr) !! PA
      val fmtRows = rowToPubid(p)
      testIB(p, C.ReqType, None, ShowDead, fmtRows)(allSortsIB(
        asc  = "BR-1  BR-2  CO-1  CO-2  FR-1  FR-2  MF-1  MF-2",
        desc = "MF-1  MF-2  FR-1  FR-2  CO-1  CO-2  BR-1  BR-2"))
    }

    def testCustomTextField(): Unit = {
      def t(n: String, r: String) = GReq(reqType = dd).cftextS(notesField, n).cftextS(reporterField, r)
      val p   = GReq() + t("HAHA", "zz") + t("", "f") + t("d", "") + t("Abc", "g") !! PA
      val pt  = pcache(p).pt
      val fmt = rowToCustomText(pt, notesField)
      testCB(p, notesField, None, ShowDead, fmt)(allSortsCB(2,
        asc  = "Abc  d  HAHA",
        desc = "HAHA  d  Abc"))
    }

    def testReqCodes(): Unit = {
      def req(codes: ReqCode.Value*) = GReq(codes = codes.toSet)
      def grp(code: ReqCode.Value)   = RCGroup(code)
      val p =
        GReq().times(2)            +
        req("a.b.c", "x.y.z")      +
        req("a")                   +
        req("a.boo", "x.z", "y.q") +
        grp("abc")                 +
        grp("a.b.d")               +
        req("abc.no")              !! PA
      val fmtRows = rowToAsToStr(_.exp.reqCodes, r => Vector1(r.reqCode))(PlainText.reqCode)
      testCB(p, C.Code, None, ShowDead, fmtRows)(allSortsCB(2,
        asc  = "a  a.b.c  a.b.d  a.boo  abc  abc.no  x.y.z  x.z,y.q",
        desc = "y.q,x.z  x.y.z  abc.no  a.boo  a.b.c  a")) // groups not displayed in DESC
    }

    def testApplicabilityOfCustomTextFields(): Unit = {
      // desc only applies to 2(MF) 6(SI) UC
      // notes applies to all except 4(BR)
      val p =
        GReq(reqType = mf).cftextS(descField, "MF.desc.ok" ).cftextS(notesField, "MF.note.ok") +
        GReq(reqType = co).cftextS(descField, "CO.desc.NO!").cftextS(notesField, "CO.note.ok") +
        GReq(reqType = br).cftextS(descField, "BR.desc.NO!").cftextS(notesField, "BR.note.NO!") !! PA
      val pt = pcache(p).pt
      val ap = Applicability(p)
      def fmt(c: CustomField.Text.Id) =
        ap(c).wrap(rowToCustomText(pt, c))(z)
      def expect(zcount: Int, suffix: String)(prefixes: String*) = {
        val es = prefixes.map(_ + suffix).sorted
        allSortsCB(zcount, es mkString sep, es.reverse mkString sep)
      }
      testCB(p, descField,  None, ShowDead, fmt(descField)) (expect(2, ".desc.ok")("MF"))
      testCB(p, notesField, None, ShowDead, fmt(notesField))(expect(1, ".note.ok")("CO", "MF"))
    }

    def testApplicabilityOfCustomTagFields(): Unit = {
      // Field 5 over 10(Status) not applicable to types 5(DD) 6(SI)
      val p   = GReq(reqType = mf).tag(wip, defer) + GReq(reqType = si).tag(wip, defer) !! PA
      val fmt = prefixWithPubid(p, rowToTagTxt(p, Row cfTag statusField))
      testCB(p, statusField, None, ShowDead, fmt)(allSortsCB(1, "MF-1:wip,defer", "MF-1:defer,wip"))
      // Order here ↗ looks wrong but is correctly determined by tags' position in the TagTree (thus configurable)
    }

    def testApplicabilityOfCustomImpFields(): Unit = {
      // Field 6 over implications of 2(MF) not applicable to types 6(SI)
      val p = GReq(id = 21, reqType = mf) + GReq(id = 22, reqType = mf) +
        GReq(id = 31, reqType = fr).impSrc(21,22) +
        GReq(id = 61, reqType = si).impSrc(21,22) ! PA
      val fmt = rowToCustomImpTxt(p, mfField)
      testCB(p, mfField, None, ShowDead, fmt)(allSortsCB(1,
        "MF-1>FR-1  MF-1>MF-1  MF-2>FR-1  MF-2>MF-2",
        "MF-2>FR-1  MF-2>MF-2  MF-1>FR-1  MF-1>MF-1"))
    }

    def testFilterDeadRows(): Unit = {
      def dead = GReq(live = Dead)
      def live = GReq()
      val p   = (live + dead + live + dead + live).defaultReqType(mf) ! PD
      val fmt = rowToPubid(p)
      testUnsorted(p, C.Pubid, None, ShowDead, fmt)("MF-1  MF-2  MF-3  MF-4  MF-5")
      testUnsorted(p, C.Pubid, None, HideDead, fmt)("MF-1  MF-3  MF-5")
    }

    def testFilterDeadImpsSrc(): Unit = {
      val p   = (GReq(id = 1) + GReq(id = 2, live = Dead) + GReq(id = 3).impSrc(1,2)).defaultReqType(br) ! PD
      val c   = C.ImplicationSrc
      val fmt = rowToSrcImpTxt(p)
      testUnsorted(p, c, None, ShowDead, fmt)(s"$z  $z  BR-1,BR-2>BR-3")
      testUnsorted(p, c, None, HideDead, fmt)(s"$z  BR-1>BR-3")
    }

    def testFilterDeadImpsTgt(): Unit = {
      val p = (GReq(id = 1) + GReq(id = 2, live = Dead) + GReq(id = 3).impTgt(1,2)).defaultReqType(br) ! PD
      val c   = C.ImplicationTgt
      val fmt = rowToTgtImpTxt(p)
      testUnsorted(p, c, None, ShowDead, fmt)(s"$z  $z  BR-1,BR-2<BR-3")
      testUnsorted(p, c, None, HideDead, fmt)(s"$z  BR-1<BR-3")
    }

    def testFilterDeadCustomImps(): Unit = {
      val p = (
        // MF-1ᵒ → MF-5ᵒ → FR-1
        // MF-2ˣ → MF-6ᵒ → FR-1 <-- difficult case - it should be displayed as its part of (a chain with ShowDead)
        // MF-3ᵒ → MF-7ˣ → FR-1 <-- important case - shouldn't hold for FR-1 even in ShowDead
        // MF-4ˣ → MF-8ˣ → FR-1
        GReq(reqType = mf, id = 1) +
        GReq(reqType = mf, id = 2, live = Dead) +
        GReq(reqType = mf, id = 3) +
        GReq(reqType = mf, id = 4, live = Dead) +
        GReq(reqType = mf, id = 5).impSrc(1) +
        GReq(reqType = mf, id = 6).impSrc(2) +
        GReq(reqType = mf, id = 7, live = Dead).impSrc(3) +
        GReq(reqType = mf, id = 8, live = Dead).impSrc(4) +
        GReq(reqType = fr, id = 91).impSrc(5, 6, 7, 8) +
        // MF-1ᵒ → CO-1ᵒ → FR-2
        // MF-2ˣ → CO-2ᵒ → FR-2 <-- difficult case - it should be displayed as its part of (a chain with ShowDead)
        // MF-3ᵒ → CO-3ˣ → FR-2 <-- important case - shouldn't hold for FR-2 even in ShowDead
        // MF-4ˣ → CO-4ˣ → FR-2
        GReq(reqType = co, id = 11).impSrc(1) +
        GReq(reqType = co, id = 12).impSrc(2) +
        GReq(reqType = co, id = 13, live = Dead).impSrc(3) +
        GReq(reqType = co, id = 14, live = Dead).impSrc(4) +
        GReq(reqType = fr, id = 92).impSrc(11, 12, 13, 14)
        ) ! PD
      val c = mfField
      val fmt = rowToCustomImpTxt(p, c)

      testUnsorted(p, c, None, ShowDead, fmt)(
        s"""
          |MF-1>CO-1
          |MF-2>CO-2
          |MF-3>CO-3
          |MF-4>CO-4
          |MF-1,MF-2,MF-5,MF-6,MF-7,MF-8>FR-1
          |MF-1,MF-2>FR-2
          |MF-1>MF-1
          |MF-2>MF-2
          |MF-3>MF-3
          |MF-4>MF-4
          |MF-1,MF-5>MF-5
          |MF-2,MF-6>MF-6
          |MF-3,MF-7>MF-7
          |MF-4,MF-8>MF-8
        """.stripMargin.replace("\n", sep).trim)

      testUnsorted(p, c, None, HideDead, fmt)(
        s"""
          |MF-1>CO-1
          |$z
          |MF-1,MF-5,MF-6>FR-1
          |MF-1>FR-2
          |MF-1>MF-1
          |MF-3>MF-3
          |MF-1,MF-5>MF-5
          |MF-6>MF-6
        """.stripMargin.replace("\n", sep).trim)
    }

    def testFilterDeadTags(): Unit = {
      val p       = GReq(reqType = fr).tag(v1x, v3x) ! PD
      val fmtRows = rowToTagTxt(p, Row.tags)
      testUnsorted(p, C.Tags, None, ShowDead, fmtRows)("v1.x,v3.x")
      testUnsorted(p, C.Tags, None, HideDead, fmtRows)("v1.x")
    }

    def testFilterDeadTagsInCustomTagField(): Unit = {
      val p        = GReq(reqType = fr).tag(wip, uat, v1x) ! PD
      val fmtRowsC = rowToTagTxt(p, Row cfTag statusField)
      val fmtRowsT = rowToTagTxt(p, Row.tags)
      testUnsorted(p, statusField, None, ShowDead, fmtRowsC)("wip,uat")
      testUnsorted(p, statusField, None, HideDead, fmtRowsC)("wip")
      testUnsorted(p, C.Tags, None, ShowDead, fmtRowsT)("v1.x")
      testUnsorted(p, C.Tags, None, HideDead, fmtRowsT)("v1.x")
    }

    def testFilterDeadCustomTagField() = {
      val p        = GReq(reqType = fr).tag(v09, v10, v2x) ! PD
      val fmtRowsC = rowToTagTxt(p, Row cfTag relField)
      val fmtRowsT = rowToTagTxt(p, Row.tags)
      // dead-customfield visible
      val both = NonEmptyVector[C](C.Tags, relField)
      testUnsorted(p, relField, None, ShowDead, fmtRowsC)("v0.9,v1.0")
      testUnsorted2(p, both, None, ShowDead, fmtRowsC)("v0.9,v1.0")
      testUnsorted2(p, both, None, ShowDead, fmtRowsT)("v2.x")
      // dead-customfield not visible
      testUnsorted(p, C.Tags, None, HideDead, fmtRowsT)("v1.0,v2.x")
      testUnsorted(p, C.Tags, None, ShowDead, fmtRowsT)("v0.9,v1.0,v2.x")
    }

    def testTags_inText(): Unit = {
      def t(direct: ApplicableTagId*)(inTitle: ApplicableTagId*)(inCustomText: ApplicableTagId*) =
        GReq(title = reqTitleTagRefs(inTitle))
          .cftextO(descField, customTextTagRefs(inCustomText))
          .cftext(reporterField, allLiveTags map Text.CustomTextField.TagRef) // dead column has no effect
          .tag(direct: _*)
      val p       = t()()() + t(v10)(v12)(v1x, v1x) + t(v2x)(v2x, v11)(v11) ! PA
      val fmtRows = rowToTagTxt(p, Row.tags)
      testUnsorted(p, C.Tags, None, HideDead, fmtRows)(s"$z  v1.0,v1.2,v1.x  v1.1,v2.x")
      // The Tags column is *not* expanded. Only custom tag columns are.
//      testCB(p, pt, C.Tags, None, HideDead, fmtRows)(allSortsCB(1,
//        asc  = "v1.0  v1.1  v1.2,v1.x  v2.x",
//        desc = "xxxxxxxxx"))
    }

    def testCustomTagField_inText(): Unit = {
      // TODO test tag transitivity: column tag ← mutual tag ← tag in text
      def t(direct: ApplicableTagId*)(inTitle: ApplicableTagId*)(inCustomText: ApplicableTagId*) =
        GReq(title = reqTitleTagRefs(inTitle))
          .cftextO(descField, customTextTagRefs(inCustomText))
          .cftext(reporterField, customTextTagRefs(allLiveTags)) // dead column has no effect
          .tag(direct: _*)
      val p       = t(wip)(wip, priHigh)(priLow, priLow) + t()()() + t(priMed)(priHigh, priMed)(priHigh, defer) ! PA
      val fmtRows = rowToTagTxt(p, Row cfTag priField)
      testUnsorted(p, priField, None, HideDead, fmtRows)(s"pri=high,pri=low  $z  pri=high,pri=med")
      testCB(p, priField, None, HideDead, fmtRows)(allSortsCB(1,
        asc  = "pri=high  pri=high,pri=med  pri=low",
        desc = "pri=low  pri=med  pri=high  pri=high"))
    }

    def testFilterDeadTagsInText(): Unit = {
      val p       = GReq(reqType = fr, title = reqTitleTagRefs(v09)).tag(v1x).cftext(descField, customTextTagRefs(v3x)) ! PD
      val fmtRows = rowToTagTxt(p, Row.tags)
      testUnsorted(p, C.Tags, None, ShowDead, fmtRows)("v0.9,v1.x,v3.x")
      testUnsorted(p, C.Tags, None, HideDead, fmtRows)("v1.x")
    }

    def testReqCodeTree(): Unit = {
      val src =
        """
          │a.a.1
          │a.a.2
          │a.aaa.1
          │a.aaa.2
          │a.c
          │a.c.d
          │aaa.a.1
          │aaa.a.2
          │aaa.aaa.1
          │aaa.aaa.2
          │aaa.c
          │aaa.c.d
          │b
          │b.a.1
          │b.a.2
          │b.aaa.1
          │b.aaa.2
          │b.c
          │b.c.d
          │bbb
          │bbb.a.1
          │bbb.a.2
          │bbb.aaa.1
          │bbb.aaa.2
          │bbb.c
          │bbb.c.d
          │refs1
          │refs1.code
          │refs1.code.short.parse
          │refs1.code.short.disp
          │refs1.code.change
          │refs1.code.change.update
          │refs1.code.change.warn
          │refs1.code.display
          │refs2
          │refs2.code.short.parse
          │refs2.code.short.disp
          │refs2.code.change
          │refs2.code.change.update
          │refs2.code.change.warn
          │refs2.code.display
        """.stripMargin('│').trim

      // Rule #1: Indent bar lines up with first char (az) of string above.
      // Rule #2: Child's first char (az) is 2 vertical spaces away from first char (az) of parent. (so gap=1)

      val exp =
        """
          │a.a.1
          │| |.2
          │|.aaa.1
          │| |  .2
          │|.c
          │| |.d
          │aaa.a.1
          │|   |.2
          │|  .aaa.1
          │|   |  .2
          │|  .c
          │|   |.d
          │b
          │|.a.1
          │| |.2
          │|.aaa.1
          │| |  .2
          │|.c
          │| |.d
          │bbb
          │|.a.1
          │| |.2
          │|.aaa.1
          │| |  .2
          │|.c
          │| |.d
          │refs1
          │|.code
          │| |.short.parse
          │| | |    .disp
          │| |.change
          │| | |.update
          │| | |.warn
          │| |.display
          │refs2
          │|.code.short.parse
          │| |    |    .disp
          │| |   .change
          │| |    |.update
          │| |    |.warn
          │| |   .display
        """.stripMargin('│').trim

      import ReqCode._

      val mkReqCode: String => Value = line => {
        val v = line.split("\\.").map(Node.applyFn).toVector
        NonEmptyVector(v.head, v.tail)
      }

      def formatTreeItems(ts: Vector[ReqCodeTreeItem]) =
        ts map PlainText.reqCodeTreeItem map (_.replace('│', '|')) mkString "\n"

      val srcCodes = src.split("\n").map(mkReqCode)

      val actual = Logic.mkReqCodeTree[Value, Vector, Vector, Vector[ReqCodeTreeItem]](srcCodes, Vector1, (_, ts) => ts)
        .flatten

      assertMultiline(formatTreeItems(actual), exp)
    }
  }

  // ===================================================================================================================

  // NOTE: The Tags column is *not* expanded. Only custom tag columns are.

  override def tests = TestSuite {
    'prop - gen.mustSatisfyE(_.all)//(implicitly[Settings].setSeed(0).setDebug.setSampleSize(20))
    'unit {
      import UnitTest._
      'sort {
        'reqCodes - testReqCodes()
        'reqType  - testReqType()
        'title    - testTitle()
        'impSrc   - testImpSrc()
        'impTgt   - testImpTgt()
        'impCust  - testCustomImpField()
        'custTxt  - testCustomTextField()
        'tags {
          'sorted1  - testTags_sorted1()
          'sorted2  - testTags_sorted2()
          'unsorted - testTags_unsorted()
          'inText   - testTags_inText()
        }
        'custTag {
          'sorted1  - testCustomTagField_sorted1()
          'sorted2  - testCustomTagField_sorted2()
          'unsorted - testCustomTagField_unsorted()
          'inText   - testCustomTagField_inText()
        }
      }
      'applicability {
        'custTxt - testApplicabilityOfCustomTextFields()
        'custTag - testApplicabilityOfCustomTagFields()
        'custImp - testApplicabilityOfCustomImpFields()
      }
      'filterDead {
        'rows       - testFilterDeadRows()
        'impSrc     - testFilterDeadImpsSrc()
        'impTgt     - testFilterDeadImpsTgt()
        'impCust    - testFilterDeadCustomImps()
        'tags       - testFilterDeadTags()
        'tagsInText - testFilterDeadTagsInText()
        'tagsCust   - testFilterDeadTagsInCustomTagField()
        'tagField   - testFilterDeadCustomTagField()
      }
      'reqCodeTree - testReqCodeTree()
    }
  }
}
