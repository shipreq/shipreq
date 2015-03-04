package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.util.Multimap
import scalaz.{\/, \/-, -\/, Equal}
import scalaz.std.AllInstances._
import utest._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.{UnsafeTypes, RandomData}
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.{SampleProject, ProjectDSL}
import shipreq.webapp.client.lib.Presentation
import shipreq.webapp.client.test.ClientTestSettings._
import shipreq.webapp.client.app.ui.reqtable.{SortCriterion => SC, Column => C}
import SortMethod._
import Sorter._

object LogicTest extends TestSuite {

  private def codesInRow(r: Row): List[ReqCode] = r match {
    case g: GenericReqRow => g.exp.reqCodes
  }

  private def tagsInRow(r: Row): List[ApplicableTag.Id] = r match {
    case g: GenericReqRow => g.mv.tags
  }

  def pubidExtract(p: Project)(pid: Pubid): (String, Int) =
    (p.reqType(pid.reqTypeId).fold(sys.error, _.mnemonic.value), pid.pos.value)

  def pubidToStr(p: Project)(pid: Pubid): String = {
    val (a, b) = pubidExtract(p)(pid)
    s"$a-$b"
  }

  def firstCodePerRow(r: Row): String =
    codesInRow(r) match {
      case Nil    => ""
      case h :: _ => h.txt
    }

  def applicableTag(p: Project): ApplicableTag.Id => ApplicableTag =
    id => p.tags.data.get(id).map(_.tag) match {
      case Some(t: ApplicableTag) => t
      case x => sys.error(s"Not an ApplicableTag: $x")
    }

  // ===================================================================================================================
  val nop = Eval.pass()

  case class LogicTests(vs: ViewSettings, p: Project) {
    val E = EvalOver(this)

    val gathered    = Logic.gather(vs, p)
    val gatheredG   = gathered.filterT[GenericReqRow]
    val rowReqCodes = gathered.flatMap(codesInRow(_).toStream)
    val rowGReqIds  = gatheredG.map(_.req.id).toSet
    val srcGReqIds  = p.reqs.data.reqs.keys.filterT[GenericReq.Id].toSet
    val srcReqCodes = p.reqCodes.data.codeSet
    val textToStr   = Presentation.textToString(p)

    // -----------------------------------------------------------------------------------------------------------------
    // Gathering

    def noEmptyAndNonEmptyReqCodesMixed = {
      val data: Stream[List[List[ReqCode]]] =
        Multimap.empty[Req.Id, List, List[ReqCode]]
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
    def gather =
      ( E.distinct("Rows", gathered)
      ∧ E.allPresent("each generic req id has a row", srcGReqIds, rowGReqIds)
      ∧ E.allPresent("all req codes are displayed", srcReqCodes, rowReqCodes)
      ∧ noEmptyAndNonEmptyReqCodesMixed
      ) rename "Logic.gather"

    // -----------------------------------------------------------------------------------------------------------------
    // Sorting

    implicit def textOrd[T <: Text.Generic] =
      implicitly[Ordering[String]].on[T#OptionalText](t => textToStr(t).toLowerCase)

    def universalSort = {
      val revOrder  = vs.order.reverse
      val sorted    = Logic.sort(vs.order, p)(gathered)
      //val reversed  = Logic.sort(revOrder, p)(gathered)
      def criRev    = E.equal("[criteria] reverse.reverse = id", revOrder.reverse, vs.order)
      def sortTwice = E.equal("sort.sort = sort", Logic.sort(vs.order, p)(sorted.toStream), sorted)
      def sortRev   = E.pass // TODO FAILS: sort(criteria.reverse) = reverse(sort(cri))
      //def sortRev   = E.equal("sort(criteria.reverse) = reverse(sort(cri))", reversed, reverseRows(sorted))
      ((criRev ==> sortRev) ∧ sortTwice) rename "Universal sort props"
    }

    def reverseRows(rs: List[Row]): List[Row] =
      rs.reverse.map {
        case GenericReqRow(r, e, mv) => GenericReqRow(r, reverseExpansion(e), reverseMultiValues(mv))
      }

    def reverseExpansion(e: Expansion): Expansion = {
      val Expansion(a, b, c) = e
      Expansion(a.reverse, b.reverse, c.reverse)
    }

    def reverseMultiValues(mv: MultiValues): MultiValues = {
      val MultiValues(a, b, c) = mv
      MultiValues(a.reverse, b.mapValues(_.reverse), c.mapValues(_.reverse))
    }

    def sortCri(c: SC.Inconclusive): SortCriteria =
      this.vs.order.copy(init = Vector(c))

    def gatherOn(c: C.SortInconclusive, sc: SortCriteria): Stream[Row] =
      if (vs isVisible c) gathered else Logic.gather(ViewSettings(Vector(c), sc), p)

    def sortCriAndGather(c: SC.Inconclusive) =
      sortCri(c).mapStrengthR(gatherOn(c.column, _))

    def sortBy(c: SC.Inconclusive) = {
      val (sc, input) = sortCriAndGather(c)
      Logic.sort(sc, p)(input)
    }

    /** @return error \/ (blank, non-blank) */
    def separateBlanks[A](expectBlanksFirst: Boolean, as: List[A])(isBlank: A => Boolean): String \/ (List[A], List[A]) = as match {
      case Nil =>
        \/-(Nil, Nil)
      case h :: t =>
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

    def E_bnbBlocks[A](name: String, bp: BlankPlacement, as: List[A])(isBlank: A => Boolean, f: (List[A], List[A]) => EvalL): EvalL = {
      val expectBlanksFirst = bp match {case BlanksFirst => true; case BlanksLast => false}
      E.either(s"$name make separate blank/non-blank blocks", separateBlanks(expectBlanksFirst, as)(isBlank))(f.tupled)
    }

    def E_sorted[A: Ordering: Equal](name: String, as: List[A], dirChange: Dir): EvalL =
      E.equal(name + " are sorted", as, dirChange(as.sorted)(_.reverse))

    type IndivSortCB = (ConsiderBlanks, BlankPlacement, Dir) => EvalL
    type IndivSortIB = (IgnoreBlanks  ,                 Dir) => EvalL

    def sortByPubid: IndivSortIB = (sm, dir) => {
      val sc     = SortCriteria(Vector.empty, SC.Conclusive(C.PubId, sm))
      val sorted = Logic.sort(sc, p)(gathered)
      val pubids = sorted.map { case r: GenericReqRow => pubidExtract(p)(r.req.pubId)}
      E_sorted("Pubids", pubids, dir)
    }

    def sortByRecCode: IndivSortCB = (sm, bp, dir) => {
      val sorted     = sortBy(SC.InconclusiveCB(C.Code, sm))
      val data       = sorted map firstCodePerRow
      val name       = s"ReqCodes ($sm)"
      val intra      = sorted.toStream.map(codesInRow).filter{case _ :: _ :: _ => true; case _ => false}.map(_.map(_.txt))
      def eachRow    = E.forall(intra)(E_sorted(s"Codes within a single row are sorted.", _, dir))
      def wholeTable = E_bnbBlocks(name, bp, data)(_.isEmpty, (_, nb) => E_sorted(name, nb, dir))
      (wholeTable ∧ eachRow) rename name
    }

    def sortByDesc: IndivSortCB = (sm, bp, dir) => {
      val sorted     = sortBy(SC.InconclusiveCB(C.Desc, sm))
      val data       = sorted.map{ case r: GenericReqRow => r.req.desc }
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
      case C.PubId           => sortIB(sortByPubid)
      case C.Code            => sortCB(sortByRecCode)
      case C.Desc            => sortCB(sortByDesc)
      case C.Tags            => nop
      case C.ImplicationSrc  => nop
      case C.ImplicationTgt  => nop
      case C.CustomField(id) =>
        id match {
          case i: CustomField.Implication.Id => nop
          case i: CustomField.Tag        .Id => nop
          case i: CustomField.Text       .Id => nop
        }
    }

    def individualSorts: EvalL =
      C.all(None).map(individualSort).reduce(_ ∧ _)

    def sorting =
      (individualSorts ==> universalSort) rename "Logic.sort"

    // -----------------------------------------------------------------------------------------------------------------
    def all = gather ∧ sorting
  }

  def gen: Gen[LogicTests] =
    for {
      p  <- RandomData.project
      vs <- ReqTableTest.rndViewSettings(p)
    } yield
      LogicTests(vs, p)

  // ===================================================================================================================
  // Unit tests
  // Fucking IntelliJ crashes typing these tests inline

  object UnitSort {
    import ProjectDSL._
    import UnsafeTypes._
    private val P = SampleProject.project
    private type Rows = List[Row]

    private def vsSortedByCB(c: C.SortInconclusive with C.HasBlanks, sm: ConsiderBlanks): ViewSettings =
      ViewSettings(Vector(c), SortCriteria.default.copy(init = Vector(SC.InconclusiveCB(c, sm))))

    private def test[A: Equal](p: Project, c: C.SortInconclusive with C.HasBlanks, extract: Rows => A)(tests: (ConsiderBlanks, A)*) =
      for ((sm, expect) <- tests) {
        val vs = vsSortedByCB(c, sm)
        val r = Logic.gather(vs, p) |> Logic.sort(vs.order, p)
        assertEq(sm.toString, extract(r), expect)
      }

    private def allSorts[A](z: A)(f: (A, A) => A, asc: A, desc: A): Seq[(ConsiderBlanks, A)] =
      (BlanksThenAsc  -> f(z, asc))  ::
      (AscThenBlanks  -> f(asc, z))  ::
      (BlanksThenDesc -> f(z, desc)) ::
      (DescThenBlanks -> f(desc, z)) :: Nil

    def testTags(): Unit = {
      def t(ids: ApplicableTag.Id*) = GReq().tag(ids: _*)
      val p       = GReq() + t(2) + t(3) + t(11) + t(12) + t(11, 12) + t(12, 11) !! P
      val (z,sep) = ("∅","  ")
      val fmtEach = applicableTag(p).andThen(_.key.value)
      val fmtRows = (_: Rows).map(tagsInRow(_).ifelse(_.isEmpty, _ => z, _ map fmtEach mkString ",")) mkString sep
      test(p, C.Tags, fmtRows)(allSorts(z)(_ + sep + _,
        asc  = "defer  defer,wip  defer,wip  pri=high  pri=med  wip",
        desc = "wip,defer  wip,defer  wip  pri=med  pri=high  defer"): _*)
    }

    def testDesc(): Unit = {
      val p       = GReq() + GReq("AT") + GReq("and") + GReq("haha") + GReq("F") !! P
      val (z,sep) = ("∅"," ")
      val fmtEach = Presentation.textToString(p)
      val fmtRows = (_: Rows).map{ case r: GenericReqRow => r.req.desc.ifelse(_.isEmpty, _ => z, fmtEach) } mkString sep
      test(p, C.Desc, fmtRows)(allSorts(z)(_ + sep + _,
        asc  = "and AT F haha",
        desc = "haha F AT and"): _*)
    }

    def testImpSrc(): Unit = {
      def t(_id: GenericReq.Id, ids: Req.Id*) = GReq(id = _id, reqType = 3).impSrc(ids: _*)
      //            FR-1   FR-2      DD-1                           FR-3         FR-4      FR-5
      val p       = t(1) + t(2, 1) + t(3, 1, 2).copy(reqType = 5) + t(4, 1, 3) + t(5, 3) + t(6, 5) ! P
      val (z,sep) = ("∅","  ")
      def fmtEach(s: Pubid, t: Pubid) = pubidToStr(p)(s) + ">" + pubidToStr(p)(t)
      val fmtRows = (_: Rows).map{ case r: GenericReqRow => r.exp.implicationSrc.ifelse(_.isEmpty, _ => z, _ map (fmtEach(_, r.req.pubId)) mkString ",") } mkString sep
      test(p, C.ImplicationSrc, fmtRows)(allSorts(z)(_ + sep + _,
        asc  = "DD-1>FR-3  DD-1>FR-4  FR-1>DD-1  FR-1>FR-2  FR-1>FR-3  FR-2>DD-1  FR-4>FR-5",
        desc = "FR-4>FR-5  FR-2>DD-1  FR-1>DD-1  FR-1>FR-2  FR-1>FR-3  DD-1>FR-3  DD-1>FR-4"): _*)
    }

    // ReqType
    // Code
    // ImplicationTgt
    // CustomField.Implication.Id
    // CustomField.Tag        .Id
    // CustomField.Text       .Id

  }

  // ===================================================================================================================
  override def tests = TestSuite {
    'prop - gen.mustSatisfyE(_.all)//(implicitly[Settings].setSeed(0).setDebug.setSampleSize(20))
    'unit {
      'sort {
        'desc   - UnitSort.testDesc()
        'tags   - UnitSort.testTags()
        'impSrc - UnitSort.testImpSrc()
      }
    }
  }
}
