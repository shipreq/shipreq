package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.{Lens, Optional}
import scalaz.Equal
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.savedview.{Column => C, SortCriterion => SC}
import shipreq.webapp.base.event.{CustomImpFieldGD, Event => E, GenericReqGD, UseCaseGD, UseCaseStepGD}
import shipreq.webapp.base.filter.{Filter, IntensionalReqSet}
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.sort.SortMethod._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{PlainText, Text, TextSearch}
import shipreq.webapp.base.util.ReqCodeTreeItem
import sourcecode.Line
import utest._

object LogicTestUtil {
  def codesInRow(r: Row): Vector[ReqCode.Value] =
  // Don't use optics here
    r match {
      case r: Row.ForReq       => r.exp.reqCodes.values
      case r: Row.ForCodeGroup => Vector1(r.reqCode)
    }

  def pubidExtract(p: Project)(pid: Pubid): (String, Int) =
    (p.config.reqTypes.need(pid.reqTypeId).mnemonic.value, pid.pos.value)

  def pubidToStr(p: Project)(pid: Pubid): String = {
    val (a, b) = pubidExtract(p)(pid)
    s"$a-$b"
  }

  def firstCodePerRow(r: Row): String = {
    val c = codesInRow(r)
    if (c.isEmpty) "" else PlainText.reqCode(c.head)
  }

  def applicableTag(p: Project): ApplicableTagId => ApplicableTag =
    id => p.config.tags.tree.get(id).map(_.tag) match {
      case Some(t: ApplicableTag) => t
      case x => sys.error(s"Not an ApplicableTag: $x")
    }

  def columnState(c: Column): NonEmptyVector[Column] =
    columnState(NonEmptyVector one c)

  def columnState(cs: NonEmptyVector[Column]): NonEmptyVector[Column] =
    cs
}

// ===================================================================================================================
// Unit tests
// Fucking IntelliJ crashes typing these tests inline

object LogicTest extends TestSuite {
  import ProjectDsl._
  import UnsafeTypes._
  import SampleProject7.Values._
  import shipreq.webapp.base.filter.Filter.{Valid => F}
  import shipreq.webapp.base.filter.FilterAst.Attr.{AnyIssue, AnyTag}
  import shipreq.webapp.base.filter.FilterAst.{FieldAttr, FieldCriteria, ImpCriteria}
  import shipreq.webapp.base.filter.IntensionalReqSet._
  import LogicTestUtil._

  private implicit def liftFieldAttr(a: FieldAttr): F.FieldCriteria =
    FieldCriteria.Attr(a)

  private      def P1  = SampleProject.project
  private      def P3  = SampleProject3.project
  private      def P4  = SampleProject4.project
  private      def P6  = SampleProject6.project
  private      def P7  = SampleProject7.project
  private lazy val PA  = TestOptics.customReqTypesLive.set(Live)(P1)
  private      val sep = "  "
  private      val z   = "∅"
  private      val _z  = (_: Any) => z
  private type Rows    = Vector[Row]
  private type Filter  = Option[F]

  private case class PCache(p: Project, pt: PlainText.ForProject.NoCtx, ts: TextSearch)
  private var _pcache: List[PCache] = Nil
  private def pcache(p: Project): PCache =
    _pcache.find(_.p eq p).getOrElse {
      val pt = PlainText.ForProject.noCtx(p)
      val c = PCache(p, pt, TextSearch(p, pt))
      _pcache ::= c
      c
    }

  implicit def autoSomeFilter(f: F): Filter = Some(f)
  private def testFilter(p: Project, f: Filter)(live: String, dead: String)(implicit l: Line): Unit = {
    val fmt = rowToPubid(p)
    val d = if (dead.isEmpty) live else sortPubidsInString(s"$live  $dead")
    testUnsorted(p, C.Pubid, f, HideDead, fmt)(live)
    testUnsorted(p, C.Pubid, f, ShowDead, fmt)(d)
  }

  private def gatherSortConsolidate(p: Project, v: View, pt: PlainText.ForProject.NoCtx, ts: TextSearch): Vector[Row] = {
    val fc                    = Filter.Valid.compiler(p, pt, ts, v.filterDead, applyFilterDeadToReqs = false)
    def r1: Array       [Row] = Logic.gather(p, v, fc)
    def r2: MutableArray[Row] = Logic.sorter(p, v, pt)(r1)
    val r3: Vector      [Row] = Logic.consolidateAdjacentDups(r2.iterator())

//    def renderReq(reqId: ReqId) = PlainText.pubidByReqId(reqId, p)
//    def renderTags(tagIds: Vector[ApplicableTagId]) = pt.tagList(tagIds, Live, !Mandatory, Valid.always)
//    def renderRow(r: Row.ForReq) = s"${renderReq(r.req.id)} : ${renderTags(r.exp.otherTags)} | ${renderTags(r.mv.allTags)}"
//    def renderRows(rs: TraversableOnce[Row]) = rs.toIterator.filterSubType[Row.ForReq].foreach(r => println(renderRow(r)))
//    println()
//    println("========================== r1 ==========================")
//    renderRows(r1)
//    println()
//    println("========================== r2 ==========================")
//    renderRows(r2.iterator)
//    println()
//    println("========================== r3 ==========================")
//    renderRows(r3)
//    println()

    r3
  }

  private def defaultOrder = View.default.order

  private def expansionResults[A]: Lens[Expansion[A], Vector[A]] =
    Lens[Expansion[A], Vector[A]](_.result)(_ => identity)

  private val otherTags: Optional[Row, Vector[ApplicableTagId]] =
    Row.expansion ^|-> Expansions.otherTags ^|-> expansionResults

  private val allTags: Optional[Row, Vector[ApplicableTagId]] =
    Row.expansion ^|-> Expansions.allTags ^|-> expansionResults

  private def testUnsorted[A: Equal](p: Project, c: C, f: Filter, fd: FilterDead, extract: Rows => A)(expect: A)(implicit l: Line): Unit =
    testUnsorted2(p, NonEmptyVector one c, f, fd, extract)(expect)

  private def testUnsorted2[A: Equal](p: Project, cs: NonEmptyVector[C], f: Filter, fd: FilterDead, extract: Rows => A)(expect: A)(implicit l: Line): Unit = {
    val v = View(columnState(cs), defaultOrder.copy(init = Vector.empty), fd, f, None)
    val pc = pcache(p)
    import pc.{pt, ts}
    val r = gatherSortConsolidate(p, v, pt, ts)
    def filterText = f.map(", filter = \"" + Filter.Valid.toText(p.config, _) + "\"")
    assertEq(s"testUnsorted2($fd${filterText.getOrElse("")})", extract(r), expect)
  }

  private def viewSortedByCB(c: C.SortInconclusiveHasBlanks, sm: ConsiderBlanks, fd: FilterDead, f: Filter): View =
    View(columnState(c), defaultOrder.copy(init = Vector(SC.InconclusiveCB(c, sm))), fd, f, None)

  private def testCB[A: Equal](p: Project, c: C.SortInconclusiveHasBlanks, f: Filter, fd: FilterDead, extract: Rows => A)
                              (tests: Seq[(ConsiderBlanks, A)])(implicit l: Line) = {
    val pc = pcache(p)
    import pc.{pt, ts}
    for ((sm, expect) <- tests) {
      val v = viewSortedByCB(c, sm, fd, f)
      val r = gatherSortConsolidate(p, v, pt, ts)
      assertEq(sm.toString, extract(r), expect)
    }
  }

  private def allSortsCBA[A](z: A, zcount: Int)(f: (A, A) => A, asc: A, desc: A): Seq[(ConsiderBlanks, A)] = {
    if (zcount < 1) fail("zcount must be ≥ 1")
    val zz: A = if (zcount > 1) Iterator.fill(zcount)(z).reduce(f) else z
    (BlanksThenAsc  -> f(zz, asc))  ::
    (AscThenBlanks  -> f(asc, zz))  ::
    (BlanksThenDesc -> f(zz, desc)) ::
    (DescThenBlanks -> f(desc, zz)) :: Nil
  }

  /** @param zcount Number of rows that are empty at the target column */
  private def allSortsCB(zcount: Int, asc: String, desc: String): Seq[(ConsiderBlanks, String)] =
    allSortsCBA(z, zcount)(_ + sep + _, asc, desc)

  private def viewSortedByIB(c: C.SortInconclusiveNoBlanks, sm: IgnoreBlanks, fd: FilterDead, f: Filter): View =
    View(columnState(c), defaultOrder.copy(init = Vector(SC.InconclusiveIB(c, sm))), fd, f, None)

  private def testIB[A: Equal](p: Project, c: C.SortInconclusiveNoBlanks, f: Filter, fd: FilterDead, extract: Rows => A)(tests: Seq[(IgnoreBlanks, A)]) = {
    val pc = pcache(p)
    import pc.{pt, ts}
    for ((sm, expect) <- tests) {
      val v = viewSortedByIB(c, sm, fd, f)
      val r = gatherSortConsolidate(p, v, pt, ts)
      assertEq(sm.toString, extract(r), expect)
    }
  }

  private def allSortsIB[A](asc: A, desc: A): Seq[(IgnoreBlanks, A)] =
    (Asc  -> asc) :: (Desc -> desc) :: Nil

  private def rowToStr(f: Row.ForReq => String, g: Row.ForCodeGroup => String): Row => String =
    rowToStr(f, g, identity)

  private def rowToStr(f: Row.ForReq => String, g: Row.ForCodeGroup => String, h: String => String): Row => String = {
    case r: Row.ForReq          => h(f(r))
    case r: Row.ForCodeGroup => h(g(r))
  }

  private def rowToAsToStr[A](f: Row.ForReq => Vector[A])(h: A => String): Row => String =
    rowToAsToStr(f, _ => Vector.empty)(h)

  private def rowToAsToStr[A](f: Row.ForReq => Vector[A], g: Row.ForCodeGroup => Vector[A])(h: A => String): Row => String = {
    val i = (_: Vector[A]).ifelse(_.isEmpty, _z, _ map h mkString ",")
    rowToStr(i compose f, i compose g)
  }

  private def rowToAsToStr2[A](f: Row.ForReq => Vector[A])(g: Row.ForReq => A => String) =
    rowToStr(r => f(r).ifelse(_.isEmpty, _z, _ map g(r) mkString ","), _z)

  private def rowToCustomText(pt: PlainText.ForProject.NoCtx, id: CustomField.Text.Id): Row => String = {
    val f = pt.customTextFieldOption(id)
    rowToStr(r => f(r.req) getOrElse z, _z)
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
    rowToImpTxt(p, Row.implications(Backwards), ">")

  private def rowToTgtImpTxt(p: Project): Row => String =
    rowToImpTxt(p, Row.implications(Forwards), "<")

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
    rowToStrAp2(rowToPubid(p), f)((a, b) => if (b ==* z) z else a + ":" + b)

  private def prefixWithPubidNoZ(p: Project, f: Row => String): Row => String =
    rowToStrAp2(rowToPubid(p), f)((a, b) => if (b ==* z) a else a + ":" + b)

  private val rowToReqCodes: Row => String =
    rowToAsToStr(_.exp.reqCodes.values, r => Vector1(r.reqCode))(PlainText.reqCode)

  private val pubidSep = " +".r.pattern
  private val pubidFmt = "^([A-Z]+)-(\\d+)$".r
  private def sortPubidsInString(s: String): String =
    MutableArray(
      pubidSep.split(s)
        .iterator
        .filterNot(_.isEmpty)
        .map { case pubidFmt(m, n) => (m, n.toInt) }
    )
      .sort
      .iterator()
      .map(t => t._1 + "-" + t._2.toString)
      .mkString(sep)

  implicit private def rowFnToRowsFn(f: Row => String): Rows => String =
    _ map f mkString sep

  implicit private def customFieldToColumn(id: CustomFieldId) =
    C.CustomField(id)

  // -----------------------------------------------------------------------------------------------------------------

  val fieldSetL = Project.fields

  def modCustomFields(f: EndoFn[IMap[CustomFieldId, CustomField]]): EndoFn[Project] =
    fieldSetL.modify { fs =>
      val cf = f(fs.customFields)
      FieldSet(cf, StaticField.mandatory.whole.toVector ++ cf.values.iterator.map(_.fieldId))
    }

  def clearCustomFields =
    modCustomFields(_ replaceUnderlying Map.empty)

  def testOtherTags_sorted1(): Unit = {
    def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
    val p       = GReq().times(2) + t(priHigh) + t(priMed) + t(wip) + t(defer) + t(wip, defer) + t(defer, wip) !! PA
    val p2      = clearCustomFields(p)
    val fmtRows = rowToTagTxt(p, Row.otherTags)

    // Order: defer pri=high  pri=med wip
    testCB(p2, C.OtherTags, None, ShowDead, fmtRows)(allSortsCB(2,
      asc  = "defer  defer  defer  pri=high  pri=med  wip  wip  wip",
      desc = "wip  wip  wip  pri=med  pri=high  defer  defer  defer"))
  }

  def testOtherTags_sorted2(): Unit = {
    def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
    val p       = GReq() + t(priHigh) + t(wip) + t(defer) + t(wip, defer, priHigh) + t(priMed, defer, wip) !! PA
    val p2      = modCustomFields(_.filterK(_ == priField))(p)
    val fmtRows = rowToTagTxt(p, Row.otherTags)

    // Order: defer pri=high  pri=med wip
    testCB(p2, C.OtherTags, None, ShowDead, fmtRows)(allSortsCB(2,
      asc  = "defer  defer  defer  wip  wip  wip",
      desc = "wip  wip  wip  defer  defer  defer"))
  }

  /** When tags aren't being sorted by SortCriteria they should be sorted by some default. */
  def testOtherTags_unsorted(): Unit = {
    def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
    val p       = t(wip, defer, priHigh, priMed, priLow) ! PA
    val p2      = clearCustomFields(p)
    val fmtRows = rowToTagTxt(p, otherTags)
    testUnsorted(p2, C.OtherTags, None, ShowDead, fmtRows)("defer,pri=high,pri=low,pri=med,wip")
  }

  def testCustomTagField_sorted1(): Unit = {
    def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
    val p       = GReq() + t(priHigh) + t(priMed) + t(priHigh, priMed) + t(wip, defer, v10, v12, v3x) ! PA
    val fmtRows = prefixWithPubid(p, rowToTagTxt(p, Row cfTag priField))
    testCB(p, priField, None, ShowDead, fmtRows)(allSortsCB(2,
      asc  = "DD-2:pri=high  DD-4:pri=high  DD-3:pri=med  DD-4:pri=med",
      desc = "DD-3:pri=med  DD-4:pri=med  DD-2:pri=high  DD-4:pri=high"))
  }

  def testCustomTagField_sorted2(): Unit = {
    def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
    val p       = GReq() + t(priHigh) + t(priHigh, priMed) + t(priMed) + t(wip, defer, v10, v12, v3x) ! PA
    val fmtRows = prefixWithPubid(p, rowToTagTxt(p, Row cfTag priField))
    testCB(p, priField, None, ShowDead, fmtRows)(allSortsCB(2,
      asc  = "DD-2:pri=high  DD-3:pri=high,pri=med  DD-4:pri=med",
      desc = "DD-3:pri=med  DD-4:pri=med  DD-2:pri=high  DD-3:pri=high"))
  }

  def testCustomTagField_unsorted(): Unit = {
    def t(ids: ApplicableTagId*) = GReq().tag(ids: _*)
    val p       = GReq() + t(priHigh) + t(priMed) + t(priHigh, priMed, priLow) + t(wip, defer, v10, v12, v3x) ! PA
    val fmtRows = rowToTagTxt(p, Row cfTag priField)
    testUnsorted(p, priField, None, ShowDead, fmtRows)(
      s"$z  pri=high  pri=med  pri=high,pri=med,pri=low  $z")
      // TODO s"$z  pri=high  pri=med  pri=high,pri=med  pri=high,pri=med  $z") + t(3, 2)
  }

  def testTitle(): Unit = {
    val p       = GReq() + GReq("AT") + GReq("and") + GReq("haha") + GReq("F") !! PA
    val pt      = pcache(p).pt
    val fmtRows = rowToStr(_.req |> pt.reqTitle, _.group |> pt.codeGroupTitle, _.apif(_.isEmpty, _z))
    testCB(p, C.Title, None, ShowDead, fmtRows)(allSortsCB(1,
      asc  = "and  AT  F  haha",
      desc = "haha  F  AT  and"))
  }

  def testImpSrc(): Unit = {
    def t(_id: GenericReqId, ids: ReqId*) = GReq(id = _id, reqType = fr).impSrc(ids: _*)
    //      FR-1   FR-2      DD-1                            FR-3         FR-4      FR-5
    val p = t(1) + t(2, 1) + t(3, 1, 2).copy(reqType = dd) + t(4, 1, 3) + t(5, 3) + t(6, 5) ! PA
    testCB(p, C.Implications(Backwards), None, ShowDead, rowToSrcImpTxt(p))(allSortsCB(1,
      asc  = "DD-1>FR-3  DD-1>FR-4  FR-1>DD-1  FR-1>FR-2  FR-1>FR-3  FR-2>DD-1  FR-4>FR-5",
      desc = "FR-4>FR-5  FR-2,FR-1>DD-1  FR-1>FR-2  FR-1,DD-1>FR-3  DD-1>FR-4"))
  }

  def testImpTgt(): Unit = {
    def t(_id: GenericReqId, ids: ReqId*) = GReq(id = _id, reqType = fr).impTgt(ids: _*)
    //      FR-1   FR-2      DD-1                            FR-3         FR-4      FR-5
    val p = t(1) + t(2, 1) + t(3, 1, 2).copy(reqType = dd) + t(4, 1, 3) + t(5, 3) + t(6, 5) ! PA
    testCB(p, C.Implications(Forwards), None, ShowDead, rowToTgtImpTxt(p))(allSortsCB(1,
      asc  = "DD-1<FR-3  DD-1<FR-4  FR-1<DD-1  FR-1<FR-2  FR-1<FR-3  FR-2<DD-1  FR-4<FR-5",
      desc = "FR-4<FR-5  FR-2,FR-1<DD-1  FR-1<FR-2  FR-1,DD-1<FR-3  DD-1<FR-4"))
  }

  def testCustomImpField(): Unit = {
    val p   = SampleImplicationGraph.project
    val fmt = rowToCustomImpTxt(p, mfField)
    /*
    1
    FR1 - 1,2
    FR2 - 1,2
    FR3 - 1,2
    MF1 - 1

    2
    BR1 - 2,3,4,5
    FR1 - 1,2
    FR2 - 1,2
    FR3 - 1,2
    MF2 - 2

    3
    BR1 - 2,3,4,5
    BR2 - 3,4,5
    FR4 - 3,5
    FR5 - 3,5
    FR6 - 3,4
    MF3 - 3,4,5
    MF4 - 3,4
    MF5 - 3,5

    4
    BR1 - 2,3,4,5
    BR2 - 3,4,5
    FR6 - 3,4
    MF3 - 3,4,5
    MF4 - 3,4

    5
    BR1 - 2,3,4,5
    BR2 - 3,4,5
    FR4 - 3,5
    FR5 - 3,5
    MF3 - 3,4,5
    MF5 - 3,5
    */
    testCB(p, mfField, None, ShowDead, fmt)(allSortsCB(
      zcount = 1,
      asc  = """
               |MF-1>FR-1
               |MF-1>FR-2
               |MF-1>FR-3
               |MF-1>MF-1
               |MF-2>BR-1
               |MF-2>FR-1
               |MF-2>FR-2
               |MF-2>FR-3
               |MF-2>MF-2
               |MF-3>BR-1
               |MF-3>BR-2
               |MF-3>FR-4
               |MF-3>FR-5
               |MF-3>FR-6
               |MF-3>MF-3
               |MF-3>MF-4
               |MF-3>MF-5
               |MF-4>BR-1
               |MF-4>BR-2
               |MF-4>FR-6
               |MF-4>MF-3
               |MF-4>MF-4
               |MF-5>BR-1
               |MF-5>BR-2
               |MF-5>FR-4
               |MF-5>FR-5
               |MF-5>MF-3
               |MF-5>MF-5
             """.stripMargin.replace("\n", sep).trim,
      desc = """
               |MF-5>BR-1
               |MF-5>BR-2
               |MF-5>FR-4
               |MF-5>FR-5
               |MF-5>MF-3
               |MF-5>MF-5
               |MF-4>BR-1
               |MF-4>BR-2
               |MF-4>FR-6
               |MF-4>MF-3
               |MF-4>MF-4
               |MF-3>BR-1
               |MF-3>BR-2
               |MF-3>FR-4
               |MF-3>FR-5
               |MF-3>FR-6
               |MF-3>MF-3
               |MF-3>MF-4
               |MF-3>MF-5
               |MF-2>BR-1
               |MF-2>FR-1
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
    testCB(p, C.Code, None, ShowDead, rowToReqCodes)(allSortsCB(2,
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
    val ap = Row.applicability(p.config.applicability)
    def fmt(c: CustomField.Text.Id)(r: Row): String =
      ap(r, c) match {
        case Applicable    => rowToCustomText(pt, c)(r)
        case NotApplicable => z
      }
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
    val p   = (live + dead + live + dead + live).defaultReqType(mf) ! P1
    val fmt = rowToPubid(p)
    testUnsorted(p, C.Pubid, None, ShowDead, fmt)("MF-1  MF-2  MF-3  MF-4  MF-5")
    testUnsorted(p, C.Pubid, None, HideDead, fmt)("MF-1  MF-3  MF-5")
  }

  def testFilterDeadImpsSrc(): Unit = {
    val p   = (GReq(id = 1) + GReq(id = 2, live = Dead) + GReq(id = 3).impSrc(1,2)).defaultReqType(br) ! P1
    val c   = C.Implications(Backwards)
    val fmt = rowToSrcImpTxt(p)
    testUnsorted(p, c, None, ShowDead, fmt)(s"$z  $z  BR-1,BR-2>BR-3")
    testUnsorted(p, c, None, HideDead, fmt)(s"$z  BR-1>BR-3")
  }

  def testFilterDeadImpsTgt(): Unit = {
    val p = (GReq(id = 1) + GReq(id = 2, live = Dead) + GReq(id = 3).impTgt(1,2)).defaultReqType(br) ! P1
    val c   = C.Implications(Forwards)
    val fmt = rowToTgtImpTxt(p)
    testUnsorted(p, c, None, ShowDead, fmt)(s"$z  $z  BR-1,BR-2<BR-3")
    testUnsorted(p, c, None, HideDead, fmt)(s"$z  BR-1<BR-3")
  }

  def testFilterDeadCustomImps(): Unit = {
    // One of the most important aspects of this test is that when a req in the req chain is Dead,
    // 1. it's included because it's immediately relevant
    // 2. it's not transitive because it's Dead and those links no longer hold

    /*
    digraph G {
      rankdir=LR
      MF1 [label="MF-1ᵒ"]
      MF2 [label="MF-2ˣ"]
      MF3 [label="MF-3ᵒ"]
      MF4 [label="MF-4ˣ"]
      MF5 [label="MF-5ᵒ"]
      MF6 [label="MF-6ᵒ"]
      MF7 [label="MF-7ˣ"]
      MF8 [label="MF-8ˣ"]
      CO1 [label="CO-1ᵒ"]
      CO2 [label="CO-2ᵒ"]
      CO3 [label="CO-3ˣ"]
      CO4 [label="CO-4ˣ"]
      FR1 [label="FR-1ᵒ"]
      FR2 [label="FR-2ᵒ"]
      MF1 -> MF5 -> FR1
      MF2 -> MF6 -> FR1
      MF3 -> MF7 -> FR1
      MF4 -> MF8 -> FR1
      MF1 -> CO1 -> FR2
      MF2 -> CO2 -> FR2
      MF3 -> CO3 -> FR2
      MF4 -> CO4 -> FR2
    }
     */
    val p = (
      // MF-1ᵒ → MF-5ᵒ → FR-1
      // MF-2ˣ → MF-6ᵒ → FR-1 <-- difficult case - it should be displayed as its part of [a chain with ShowDead]
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
      // MF-2ˣ → CO-2ᵒ → FR-2 <-- difficult case - it should be displayed as its part of [a chain with ShowDead]
      // MF-3ᵒ → CO-3ˣ → FR-2 <-- important case - shouldn't hold for FR-2 even in ShowDead
      // MF-4ˣ → CO-4ˣ → FR-2
      GReq(reqType = co, id = 11).impSrc(1) +
      GReq(reqType = co, id = 12).impSrc(2) +
      GReq(reqType = co, id = 13, live = Dead).impSrc(3) +
      GReq(reqType = co, id = 14, live = Dead).impSrc(4) +
      GReq(reqType = fr, id = 92).impSrc(11, 12, 13, 14)
      ) ! P1
    val c = mfField
    val fmt = rowToCustomImpTxt(p, c)

    testUnsorted(p, c, None, ShowDead, fmt)(
      s"""
        |MF-1,MF-2>CO-1
        |MF-1,MF-2>CO-2
        |MF-1,MF-2,MF-3>CO-3
        |MF-1,MF-2,MF-4>CO-4
        |MF-1,MF-2,MF-5,MF-6,MF-7,MF-8>FR-1
        |MF-1,MF-2>FR-2
        |MF-1,MF-5>MF-1
        |MF-2,MF-6>MF-2
        |MF-3,MF-7>MF-3
        |MF-4,MF-8>MF-4
        |MF-1,MF-5>MF-5
        |MF-2,MF-6>MF-6
        |MF-3,MF-7>MF-7
        |MF-4,MF-8>MF-8
      """.stripMargin.replace("\n", sep).trim)

    testUnsorted(p, c, None, HideDead, fmt)(
      s"""
        |MF-1>CO-1
        |MF-1>CO-2
        |MF-1,MF-5,MF-6>FR-1
        |MF-1>FR-2
        |MF-1,MF-5>MF-1
        |MF-3>MF-3
        |MF-1,MF-5>MF-5
        |MF-6>MF-6
      """.stripMargin.replace("\n", sep).trim)
  }

  def testFilterDeadTags(): Unit = {
    val p       = GReq(reqType = fr).tag(v1x, v3x) ! P1
    val fmtRows = rowToTagTxt(p, otherTags)
    testUnsorted(p, C.OtherTags, None, ShowDead, fmtRows)("v1.x,v3.x")
    testUnsorted(p, C.OtherTags, None, HideDead, fmtRows)("v1.x")
  }

  def testFilterDeadTagsInCustomTagField(): Unit = {
    val p        = GReq(reqType = fr).tag(wip, uat, v1x) ! P1
    val fmtRowsC = rowToTagTxt(p, Row cfTag statusField)
    val fmtRowsT = rowToTagTxt(p, otherTags)
    testUnsorted(p, statusField, None, ShowDead, fmtRowsC)("wip,uat")
    testUnsorted(p, statusField, None, HideDead, fmtRowsC)("wip")
    testUnsorted(p, C.OtherTags, None, ShowDead, fmtRowsT)("v1.x")
    testUnsorted(p, C.OtherTags, None, HideDead, fmtRowsT)("v1.x")
  }

  def testFilterDeadCustomTagField() = {
    val p        = GReq(reqType = fr).tag(v09, v10, v2x) ! P1
    val fmtRowsC = rowToTagTxt(p, Row cfTag relField)
    val fmtRowsT = rowToTagTxt(p, otherTags)
    // dead-customfield visible
    val both = NonEmptyVector[C](C.OtherTags, relField)
    testUnsorted(p, relField, None, ShowDead, fmtRowsC)("v0.9,v1.0")
    testUnsorted2(p, both, None, ShowDead, fmtRowsC)("v0.9,v1.0")
    testUnsorted2(p, both, None, ShowDead, fmtRowsT)("v2.x")
    // dead-customfield not visible
    testUnsorted(p, C.OtherTags, None, HideDead, fmtRowsT)("v1.0,v2.x")
    testUnsorted(p, C.OtherTags, None, ShowDead, fmtRowsT)("v0.9,v1.0,v2.x")
  }

  /** See `Requirements/analysis-deletion.ods`. */
  object DeadTags {
    private val liveCF = notesField
    private val deadCF = reporterField

    // TagPool, LiveText, LiveText, DeadText
    private def batch(reqLive: Live, tag: ApplicableTagId) =
      GReq(live = reqLive).tag(tag) +
      GReq(live = reqLive, title = reqTitleTagRefs(tag).whole) +
      GReq(live = reqLive).cftext(liveCF, customTextTagRefs(tag)) +
      GReq(live = reqLive).cftext(deadCF, customTextTagRefs(tag))

    private val liveTag = defer
    private val deadTag = uat

    private val p0 =
      ( batch(Live, liveTag) // DD-[ 1, 4]
      + batch(Live, deadTag) // DD-[ 5, 8]
      + batch(Dead, liveTag) // DD-[ 9,12]
      + batch(Dead, deadTag) // DD-[13,16]
      ).defaultReqType(dd) ! PA

    private val p = applyEventsSuccessfully(p0,
      E.FieldCustomDelete(priField),
      E.FieldCustomDelete(statusField))

    private val Z = ""
    private val L = ":defer"
    private val D = ":uat"

    private val fmtRowsNoFilter = prefixWithPubidNoZ(p, rowToTagTxt(p, otherTags))
    private val fmtRowsHasFilter = rowToPubid(p)

    private def test(fd: FilterDead)(tags: String*)(implicit l: Line): Unit = {
      val withIds = tags.zipWithIndex.map(_.map2(_ + 1))

      // No filter
      val expect = (for ((t,i) <- withIds) yield s"DD-$i$t") mkString sep
      testUnsorted(p, C.OtherTags, None, fd, fmtRowsNoFilter)(expect)

      // With filters
      def testWithFilters(tag: ApplicableTagId, tagStr: String): Unit = {
        val ids = withIds.filter(_._1 == tagStr).map(_._2)
        val expect = ids.map("DD-" + _) mkString sep
        testUnsorted(p, C.OtherTags, F.tag(tag), fd, fmtRowsHasFilter)(expect)
      }
      testWithFilters(liveTag, L)
      testWithFilters(deadTag, D)
    }

    def testHideDead(): Unit =
      test(HideDead)(
        L, L, L, Z, // live req, live tag
        Z, D, D, Z) // live req, dead tag - Ds here cos they're in live text = not auto-removable = issues = show

    def testShowDead(): Unit =
      test(ShowDead)(
        L, L, L, L, // live req, live tag
        D, D, D, D, // live req, dead tag
        L, L, L, L, // dead req, live tag
        D, D, D, D) // dead req, dead tag
  }

  /** See `Requirements/analysis-deletion.ods`. */
  object DeadIssues {
    private val liveCF = notesField
    private val deadCF = reporterField

    // LiveText, LiveText, DeadText
    private def batch(reqLive: Live, issue: CustomIssueTypeId) =
      GReq(live = reqLive, title = Text.GenericReqTitle(Text.GenericReqTitle.Issue(issue, ∅))) +
      GReq(live = reqLive).cftext(liveCF, Text.CustomTextField nonEmpty Text.CustomTextField.Issue(issue, ∅)) +
      GReq(live = reqLive).cftext(deadCF, Text.CustomTextField nonEmpty Text.CustomTextField.Issue(issue, ∅))

    private val liveIssue = CustomIssueTypeId(2)
    private val deadIssue = CustomIssueTypeId(3)

    private val p =
      ( batch(Live, liveIssue) // DD-[ 1, 3]
      + batch(Live, deadIssue) // DD-[ 4, 6]
      + batch(Dead, liveIssue) // DD-[ 7, 9]
      + batch(Dead, deadIssue) // DD-[10,12]
      ).defaultReqType(dd) ! PA

    private val fmtRows = rowToPubid(p)

    private def Z = "Z" // Doesn't have issue, filter by issue removes these
    private def L = "L" // Has live issue,     filter by live issue keeps these
    private def D = "D" // Has dead issue,     filter by dead issue keeps these

    private def test(fd: FilterDead)(issues: String*): Unit = {
      val withIds = issues.zipWithIndex.map(_.map2(_ + 1))

      // No filter
      val expect = withIds.map("DD-" + _._2) mkString sep
      testUnsorted(p, C.OtherTags, None, fd, fmtRows)(expect)

      // With filters
      def testWithFilters(issue: CustomIssueTypeId, issueStr: String): Unit = {
        val ids = withIds.filter(_._1 == issueStr).map(_._2)
        val expect = ids.map("DD-" + _) mkString sep
        testUnsorted(p, C.OtherTags, F.issue(issue), fd, fmtRows)(expect)
      }
      testWithFilters(liveIssue, L)
      testWithFilters(deadIssue, D)
    }

    def testHideDead(): Unit =
      test(HideDead)(
        L, L, Z, // live req, live issue
        D, D, Z) // live req, dead issue - Ds here cos they're in live text = not auto-removable = issues = show

    def testShowDead(): Unit =
      test(ShowDead)(
        L, L, L, // live req, live issue
        D, D, D, // live req, dead issue
        L, L, L, // dead req, live issue
        D, D, D) // dead req, dead issue
  }

  def testOtherTags_inText(): Unit = {
    def t(direct: ApplicableTagId*)(inTitle: ApplicableTagId*)(inCustomText: ApplicableTagId*) =
      GReq(title = reqTitleTagRefs.optional(inTitle))
        .cftextO(descField, customTextTagRefs.optional(inCustomText))
        .cftext(reporterField, NonEmptyArraySeq.fromNEV(allLiveTags).map(Text.CustomTextField.TagRef)) // dead column has no effect
        .tag(direct: _*)
    val p       = t()()() + t(v10)(v12)(v1x, v1x) + t(v2x)(v2x, v11)(v11) ! PA
    val fmtRows = rowToTagTxt(p, otherTags)
    testUnsorted(p, C.OtherTags, None, HideDead, fmtRows)(s"$z  v1.0,v1.2,v1.x  v1.1,v2.x")
    // The Tags column is *not* expanded. Only custom tag columns are.
//      testCB(p, pt, C.Tags, None, HideDead, fmtRows)(allSortsCB(1,
//        asc  = "v1.0  v1.1  v1.2,v1.x  v2.x",
//        desc = "xxxxxxxxx"))
  }

  def testCustomTagField_inText(): Unit = {
    // TODO test tag transitivity: column tag ← mutual tag ← tag in text
    def t(direct: ApplicableTagId*)(inTitle: ApplicableTagId*)(inCustomText: ApplicableTagId*) =
      GReq(title = reqTitleTagRefs.optional(inTitle))
        .cftextO(descField, customTextTagRefs.optional(inCustomText))
        .cftext(reporterField, customTextTagRefs(allLiveTags)) // dead column has no effect
        .tag(direct: _*)
    val p       = t(wip)(wip, priHigh)(priLow, priLow) + t()()() + t(priMed)(priHigh, priMed)(priHigh, defer) ! PA
    val fmtRows = rowToTagTxt(p, Row cfTag priField)
    testUnsorted(p, priField, None, HideDead, fmtRows)(s"pri=high,pri=low  $z  pri=high,pri=med")
    testCB(p, priField, None, HideDead, fmtRows)(allSortsCB(1,
      asc  = "pri=high  pri=high,pri=med  pri=low",
      desc = "pri=low  pri=med  pri=high  pri=high"))
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

  def testFilterText(): Unit = {
    testFilter(P3, F.text("req"))("MF-12  MF-13  MF-22  MF-23  MF-24", dead = "CO-2")
    testFilter(P3, F.text("l r"))("MF-12  MF-22  MF-23  MF-27", dead = "CO-2")
    testFilter(P3, F.text(" is "))("FR-1  FR-2", "")
    testFilter(P3, F.text("github.com"))("FR-1", "")
  }

  def testFilterTextPattern(): Unit = {
    val r = """.*\W[A-Z]{3}\W.*"""
    testFilter(P3, F.regex(r))("FR-2  MF-3  MF-21", "") // FR-2 because of …#TBD …
  }

  def testFilterAnyIssue(): Unit = {
    testFilter(P3, F.presence(AnyIssue))("FR-1  FR-2", "")
    testFilter(P4, F.presence(AnyIssue))("FR-1  FR-2  UC-1", "")
    testFilter(P6, F.presence(AnyIssue))("FR-1  FR-2  UC-1  UC-2", "")
  }

  def testFilterAnyTag(): Unit = {
    testFilter(P3, F.not(F.presence(AnyTag)))("FR-1  FR-2", dead = "CO-2")
  }

  def testFilterTag(): Unit = {
    testFilter(P3, F.tag(wip))("MF-5  MF-6  MF-7  MF-12  MF-13  MF-22", "")
    testFilter(P3, F.tag(v10))("MF-1  MF-2  MF-7", dead = "CO-1")
  }

  def testFilterCustomIssue(): Unit = {
    testFilter(P3, F.issue(1))("FR-1", "")
    testFilter(P3, F.issue(2))("FR-2", "")
  }

  def testFilterReqType(): Unit = {
    testFilter(P3, F.reqType(fr))("FR-1  FR-2", "")
    testFilter(P3, F.reqType(co))("", dead = "CO-1  CO-2")
  }

  def testFilterReqTypeExLive(): Unit = {
    // Change FR-1 to BR-1
    val p = applyEventsSuccessfully(P3, E.GenericReqTypeSet(frs(1), br))
    testFilter(p, F.reqType(fr))("FR-2", "BR-1")
    testFilter(p, F.reqType(br))("BR-1", "")
  }

  def testFilterReqTypeExDead(): Unit = {
    // Change FR-1 to CO-3
    val p = applyEventsSuccessfully(P3,
      E.GenericReqTypeSet(frs(1), co),
      E.CustomReqTypeDeleteSoft(co),
    )
    testFilter(p, F.reqType(fr))("FR-2", "CO-3")
    testFilter(p, F.reqType(co))("", "CO-1  CO-2  CO-3")
  }

  def testFilterImplies(): Unit = {
    import SampleImplicationGraph._
    val justFR2 = ImpCriteria.Reqs(F.reqSet(SomeOfType(fr, NonEmptySet(2))))
    testFilter(project, F.impliesAnyOf(justFR2))("BR-1  FR-1  FR-2  MF-1  MF-2", "")
    //                                               reflexivity ↑
    val fr5and6 = ImpCriteria.Reqs(F.reqSet(SomeOfType(fr, NonEmptySet(5, 6))))
    testFilter(project, F.impliesAnyOf(fr5and6))("BR-1  BR-2  FR-4  FR-5  FR-6  MF-3  MF-4", "")
    //                                                            ↗ reflexivity ↖
  }

  def testFilterImpliedBy(): Unit = {
    import SampleImplicationGraph._
    val justMF2 = ImpCriteria.Reqs(F.reqSet(SomeOfType(mf, NonEmptySet(2))))
    testFilter(project, F.impliedByAnyOf(justMF2))("FR-2  FR-3  MF-2", "")
    //                                                 reflexivity ↑
    val mf1and2 = ImpCriteria.Reqs(F.reqSet(SomeOfType(mf, NonEmptySet(1, 2))))
    testFilter(project, F.impliedByAnyOf(mf1and2))("FR-1  FR-2  FR-3  MF-1  MF-2", "")
    //                                                                ↑ reflexivity ↖
  }

  def testFilterImpliedByQuery(): Unit = {
    import SampleImplicationGraph._
    val justMF2 = ImpCriteria.Query(F.reqs(NonEmptyVector(IntensionalReqSet.SomeOfType(mf, NonEmptySet(2)))))
    testFilter(project, F.impliedByAnyOf(justMF2))("FR-2  FR-3  MF-2", "")
    //                                                 reflexivity ↑
    val mf1and2 = ImpCriteria.Query(F.reqs(NonEmptyVector(IntensionalReqSet.SomeOfType(mf, NonEmptySet(1, 2)))))
    testFilter(project, F.impliedByAnyOf(mf1and2))("FR-1  FR-2  FR-3  MF-1  MF-2", "")
    //                                                                ↑ reflexivity ↖
  }

  def testFilterImplyNothing(): Unit = {
    import SampleImplicationGraph._
    val e = ImpCriteria.Reqs(F.reqSet(SomeOfType(mf, NonEmptySet(9999999))))
    testFilter(project, F.impliedByAnyOf(e))("", "")
    testFilter(project, F.impliesAnyOf  (e))("", "")
  }

  def testFilterImpFieldNA(): Unit = {
    testFilter(P7, F.fieldProp(\/-(mfField), FieldAttr.NotApplicable))("", "SI-1  SI-2")
  }

  def testFilterImpFieldBlank(): Unit = {
    testFilter(P7, F.fieldProp(\/-(mfField), FieldAttr.Blank))("BR-1  BR-2  BR-3  UC-1  UC-2", "")
  }

  def testFilterImpFieldNotBlank(): Unit = {
    val f = F.fieldProp(\/-(mfField), FieldAttr.NotBlank)

    testFilter(P7, f)(
      "FR-1  FR-2  MF-1  MF-2  MF-3  MF-4  MF-5  MF-6  MF-7  MF-8  MF-9  MF-10  MF-11  MF-12  MF-13  MF-14  MF-15  MF-16  MF-17  MF-18  MF-20  MF-21  MF-22  MF-23  MF-24  MF-25  MF-26  MF-27",
      "CO-1  CO-2  MF-19  MF-28")

    val p = applyEventsSuccessfully(P7,
      E.FieldCustomImpUpdate(mfField, CustomImpFieldGD.ValueForFieldReqTypeRules(FieldReqTypeRules.optional.notApplicable(mf))))
    testFilter(p, f)("FR-1  FR-2", "CO-1  CO-2")
  }

  def testFilterImpFieldPos(): Unit = {
    // Filter by field:MF=2
    import SampleImplicationGraph2._
    testFilter(project, F.fieldProp(\/-(mfField), FieldCriteria.ReqTypePosSet(NonEmptySet(2))))(
      "FB-1  IV-1  IV-2  MF-2  MF-3  UC-2",
      "UC-1")
  }

  def testFilterImpFieldQuery(): Unit = {
    // Filter by field:MF=(MF2)
    import SampleImplicationGraph2._
    val subQuery = F.reqs(NonEmptyVector(IntensionalReqSet.SomeOfType(mf, NonEmptySet(2))))
    testFilter(project, F.fieldProp(\/-(mfField), FieldCriteria.Query(subQuery)))(
      "FB-1  IV-1  IV-2  MF-2  MF-3  UC-2",
      "UC-1")
  }

  def testFilterTagFieldNA(): Unit = {
    testFilter(P7, F.fieldProp(\/-(priField), FieldAttr.NotApplicable))("", "CO-1  CO-2")

    testFilter(P7, F.fieldProp(\/-(verField), FieldAttr.NotApplicable))(
      "MF-1  MF-2  MF-3  MF-4  MF-5  MF-6  MF-7  MF-8  MF-9  MF-10  MF-11  MF-12  MF-13  MF-14  MF-15  MF-16  MF-17  MF-18  MF-20  MF-21  MF-22  MF-23  MF-24  MF-25  MF-26  MF-27",
      "MF-19  MF-28")
  }

  def testFilterTagFieldBlank(): Unit = {
    testFilter(P7, F.fieldProp(\/-(verField), FieldAttr.Blank))("BR-1  BR-2  BR-3  FR-1  FR-2  UC-2", "CO-2  SI-1  SI-2")
  }

  def testFilterTagFieldDefault(): Unit = {
    testFilter(P7, F.fieldProp(\/-(verField), FieldAttr.DefaultInUse))("", "")
    testFilter(P7, F.fieldProp(\/-(priField), FieldAttr.DefaultInUse))("BR-1  BR-2  BR-3", "")

    // Even with HideDead, when an empty tag field cell gets a dead default, it's still shown to users.
    // Otherwise, they'd see a blank cell where they'd be expecting a default tag, or a <blank>.
    testFilter(P7, F.fieldProp(\/-(statusField), FieldAttr.DefaultInUse))(
      "BR-1  BR-2  FR-1  FR-2  MF-1  MF-2  MF-4  MF-8  MF-9  MF-10  MF-11  MF-14  MF-15  MF-16  MF-17  MF-18  MF-20  MF-21  MF-23  MF-24  MF-25  MF-26  MF-27",
      "CO-1  CO-2  MF-19  MF-28  SI-1  SI-2")
  }

  def testFilterTextFieldNA(): Unit = {
    testFilter(P7, F.fieldProp(\/-(bizJustField), FieldAttr.NotApplicable))("", "CO-1  CO-2  SI-1 SI-2")
    testFilter(P7, F.fieldProp(\/-(componentField), FieldAttr.NotApplicable))(
      "BR-1  BR-2  BR-3  MF-1  MF-2  MF-3  MF-4  MF-5  MF-6  MF-7  MF-8  MF-9  MF-10  MF-11  MF-12  MF-13  MF-14  MF-15  MF-16  MF-17  MF-18  MF-20  MF-21  MF-22  MF-23  MF-24  MF-25  MF-26  MF-27  UC-1  UC-2",
      "MF-19  MF-28")
  }

  def testFilterTextFieldBlank(): Unit = {
    testFilter(P7, F.fieldProp(\/-(bizJustField), FieldAttr.Blank))(
      "BR-1  BR-2  BR-3  FR-1  FR-2  MF-1  MF-2  MF-3  MF-5  MF-6  MF-7  MF-8  MF-9  MF-10  MF-11  MF-12  MF-13  MF-14  MF-15  MF-16  MF-17  MF-18  MF-20  MF-21  MF-22  MF-23  MF-24  MF-25  MF-26  MF-27  UC-1  UC-2",
      "MF-19  MF-28")
  }

  def testFilterByTagsIncludesDefaults(): Unit = {
    testFilter(P7, F.tag(priMed))(
      "BR-1  BR-2  BR-3  MF-2  MF-4  MF-6  MF-8  MF-14  MF-15  MF-17  MF-20  MF-21  MF-22  MF-23  MF-24  MF-25",
      "MF-28")
  }

  def testFilterIgnoreNATags(): Unit = {
    // BR-2 should be missing; that's the point
    testFilter(P7, F.tag(prod))("MF-3  UC-1", "")
  }

  def testFilterTitleBlank(): Unit = {
    val p = applyEventsSuccessfully(P7,
      E.GenericReqTitleSet(mfs(15), ∅),
      E.ContentRestore(Set(cos(1)), ∅),
      E.GenericReqTitleSet(cos(1), ∅),
      E.ReqsDelete(NonEmptySet.one(cos(1)), ∅, ∅),
    )
    val f = F.fieldProp(-\/(SpecialBuiltInField.Title), FieldAttr.Blank)
    testFilter(p, f)("MF-15", "CO-1")
  }

  def testFilterTitleNotBlank(): Unit = {
    val p = applyEventsSuccessfully(P7,
      E.ContentRestore(Set(cos(1), cos(2)), ∅),
      E.GenericReqTitleSet(cos(2), ∅),
      E.GenericReqCreate(cos(9), co, GenericReqGD.values(GenericReqGD.ValueForTitle("qwe"))),
      E.ReqsDelete(NonEmptySet.one(cos(1)), ∅, ∅),
    )
    val f = F.allOf(F.reqType(co), F.fieldProp(-\/(SpecialBuiltInField.Title), FieldAttr.NotBlank))
    testFilter(p, f)("CO-3", "CO-1")
  }

  def testFilterOtherTags(): Unit = {
    val f = F.not(F.fieldProp(\/-(StaticField.OtherTags), FieldAttr.Blank))
    testFilter(P7, f)("BR-2", "")
  }

  def testFilterAllTags(): Unit = {
    val f = F.fieldProp(\/-(StaticField.AllTags), FieldAttr.Blank)
    testFilter(P3, f)("FR-1  FR-2", "CO-2")
  }

  def testFilterNCAC(): Unit = {
    val uc3 = 300.UC
    val p = applyEventsSuccessfully(P7,
      E.UseCaseCreate(uc3, 3000, UseCaseGD.emptyValues),
      E.UseCaseStepCreate(3001, uc3, StaticField.NormalAltStepTree, "0".ploc),
      E.UseCaseStepUpdate(3001, UseCaseStepGD.ValueForTitle("ah")),
      E.UseCaseStepDelete(3001),
    )
    val f = F.allOf(F.reqType(StaticReqType.UseCase), F.not(F.fieldProp(\/-(StaticField.NormalAltStepTree), FieldAttr.Blank)))
    testFilter(p, f)("UC-1", "UC-3")
  }

  def testFilterEC(): Unit = {
    val uc3 = 300.UC
    val uc4 = 301.UC
    val p = applyEventsSuccessfully(P7,
      E.UseCaseCreate(uc3, 3000, UseCaseGD.emptyValues),
      E.UseCaseStepCreate(3003, uc3, StaticField.ExceptionStepTree, ∅),
      E.UseCaseStepUpdate(3003, UseCaseStepGD.ValueForTitle("ah")),
      E.UseCaseStepDelete(3003),
      E.UseCaseCreate(uc4, 4000, UseCaseGD.emptyValues),
      E.UseCaseStepCreate(4001, uc4, StaticField.ExceptionStepTree, ∅),
    )
    val f = F.allOf(F.reqType(StaticReqType.UseCase), F.not(F.fieldProp(\/-(StaticField.ExceptionStepTree), FieldAttr.Blank)))
    testFilter(p, f)("UC-1  UC-4", "UC-3")
  }

  def testFilterAll(): Unit = {
    testFilter(P3, F.allOf(F.tag(wip), F.tag(v10)))("MF-7", "")
    testFilter(P3, F.allOf(F.tag(wip), F.text("req")))("MF-12  MF-13  MF-22", "")
  }

  def testFilterAny(): Unit = {
    testFilter(P3, F.anyOf(F.reqType(co), F.reqType(fr)))("FR-1  FR-2", dead = "CO-1  CO-2")
  }

  def testFilterNot(): Unit = {
    testFilter(P3, F.not(F.reqType(mf)))("FR-1  FR-2", dead = "CO-1  CO-2")
  }

  private class RCGFilterTester {
    var expect = UnivEq.emptySet[String]

    val #### = true  // Expect: visible
    val ____ = false // Expect: hidden

    val filterHit = "pass" // Filter will match
    val no        = "no"   // Filter won't match

    def grp(expectVisible: Boolean, live: Live, title: String, code: String) = {
      if (expectVisible) expect += code
      if (live is Live) RCGroup(code, title = title) else DeadReqCode(code, title = title)
    }

    def req(expectVisible: Boolean, live: Live, title: String, code: String) = {
      if (expectVisible) expect += code
      GReq(codes = Set(code), title = title, live = live)
    }

    def test(p: Project, fd: FilterDead): Unit = {
      val expectStr = expect.toVector.sorted.mkString(sep)
      testCB(p, C.Code, F.text(filterHit), fd, rowToReqCodes)(Seq(BlanksThenAsc -> expectStr))
    }

    def common = (
      grp(####, Live, no       , "a"          ) + // 1 immediate visible child req
      req(####, Live, filterHit, "a.1"        ) +
      req(____, Live, no       , "a.2"        ) +

      grp(____, Live, no       , "b"          ) + // 0 children visible
      req(____, Live, no       , "b.1"        ) +
      req(____, Live, no       , "b.2"        ) +

      grp(####, Live, no       , "c"          ) + // child group visible
      grp(####, Live, no       , "c.x"        ) + // 1 non-immediate visible child
      req(####, Live, filterHit, "c.x.a.1"    ) +

      grp(____, Live, no       , "d"          ) + // child group invisible
      grp(____, Live, no       , "d.x"        ) + // non-immediate child invisible
      req(____, Live, no       , "d.x.a.1"    ) +

      grp(####, Live, no       , "e"          ) + // 1/2 child groups visible
      grp(____, Live, no       , "e.z.x"      ) +
      req(____, Live, no       , "e.z.x.a.1"  ) +
      grp(####, Live, no       , "e.z.y"      ) +
      req(####, Live, filterHit, "e.z.y.a.1"  ) +

      grp(____, Live, no       , "lone"       ) +
      grp(____, Live, no       , "lone.1"     ) +

      grp(____, Dead, no       , "lone_dead"  ) +
      grp(____, Dead, no       , "lone_dead.1") )
  }

  /**
   * When a filter is active, only RCGs with visible children should be shown.
   */
  def testCodeGroupWhenFilteredAndHideDead(): Unit = {
    val t = new RCGFilterTester
    import t._
    val p = (
      grp(____, Dead, filterHit, "dflf") + req(####, Live, filterHit, "dflf.1") +
      grp(____, Dead, filterHit, "dfln") + req(____, Live, no       , "dfln.1") +
      grp(____, Dead, filterHit, "dfdf") + req(____, Dead, filterHit, "dfdf.1") +
      grp(____, Dead, filterHit, "dfdn") + req(____, Dead, no       , "dfdn.1") +
      grp(____, Dead, no       , "dnlf") + req(####, Live, filterHit, "dnlf.1") +
      grp(____, Dead, no       , "dnln") + req(____, Live, no       , "dnln.1") +
      grp(____, Dead, no       , "dndf") + req(____, Dead, filterHit, "dndf.1") +
      grp(____, Dead, no       , "dndn") + req(____, Dead, no       , "dndn.1") +
      grp(####, Live, filterHit, "lflf") + req(####, Live, filterHit, "lflf.1") +
      grp(####, Live, filterHit, "lfln") + req(____, Live, no       , "lfln.1") +
      grp(####, Live, filterHit, "lfdf") + req(____, Dead, filterHit, "lfdf.1") +
      grp(####, Live, filterHit, "lfdn") + req(____, Dead, no       , "lfdn.1") +
      grp(####, Live, no       , "lnlf") + req(####, Live, filterHit, "lnlf.1") +
      grp(____, Live, no       , "lnln") + req(____, Live, no       , "lnln.1") +
      grp(____, Live, no       , "lndf") + req(____, Dead, filterHit, "lndf.1") +
      grp(____, Live, no       , "lndn") + req(____, Dead, no       , "lndn.1") +
      common) !! PA
    test(p, HideDead)
  }

  /**
   * When a filter is active, only RCGs with visible children should be shown.
   */
  def testCodeGroupWhenFilteredAndShowDead(): Unit = {
    val t = new RCGFilterTester
    import t._
    val p = (
      grp(####, Dead, filterHit, "dflf") + req(####, Live, filterHit, "dflf.1") +
      grp(####, Dead, filterHit, "dfln") + req(____, Live, no       , "dfln.1") +
      grp(####, Dead, filterHit, "dfdf") + req(####, Dead, filterHit, "dfdf.1") +
      grp(####, Dead, filterHit, "dfdn") + req(____, Dead, no       , "dfdn.1") +
      grp(####, Dead, no       , "dnlf") + req(####, Live, filterHit, "dnlf.1") +
      grp(____, Dead, no       , "dnln") + req(____, Live, no       , "dnln.1") +
      grp(####, Dead, no       , "dndf") + req(####, Dead, filterHit, "dndf.1") +
      grp(____, Dead, no       , "dndn") + req(____, Dead, no       , "dndn.1") +
      grp(####, Live, filterHit, "lflf") + req(####, Live, filterHit, "lflf.1") +
      grp(####, Live, filterHit, "lfln") + req(____, Live, no       , "lfln.1") +
      grp(####, Live, filterHit, "lfdf") + req(####, Dead, filterHit, "lfdf.1") +
      grp(####, Live, filterHit, "lfdn") + req(____, Dead, no       , "lfdn.1") +
      grp(####, Live, no       , "lnlf") + req(####, Live, filterHit, "lnlf.1") +
      grp(____, Live, no       , "lnln") + req(____, Live, no       , "lnln.1") +
      grp(####, Live, no       , "lndf") + req(####, Dead, filterHit, "lndf.1") +
      grp(____, Live, no       , "lndn") + req(____, Dead, no       , "lndn.1") +
      common) !! PA
    test(p, ShowDead)
  }

  def testFilterHasIssueOn1(): Unit = {
    import IssueCategory._
    testFilter(P6, F.hasIssue(On, BadData    ))("FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(On, MissingData))("FR-1  FR-2  UC-1  UC-2", "")
    testFilter(P6, F.hasIssue(On, Futility   ))("", "")
    testFilter(P6, F.hasIssue(On, UserDefined))("FR-1  FR-2", "")
  }

  def testFilterHasIssueOn2(): Unit = {
    import IssueCategory._
    testFilter(P6, F.hasIssue(On, BadData, BadData    ))("FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(On, BadData, MissingData))("FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(On, BadData, Futility   ))("", "")
    testFilter(P6, F.hasIssue(On, BadData, UserDefined))("FR-2", "")
  }

  def testFilterHasIssueOff1(): Unit = {
    import IssueCategory._
    testFilter(P6, F.hasIssue(Off, BadData    ))("FR-1  FR-2  UC-1  UC-2", "")
    testFilter(P6, F.hasIssue(Off, MissingData))("FR-1  FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(Off, Futility   ))("FR-1  FR-2  UC-1  UC-2", "")
    testFilter(P6, F.hasIssue(Off, UserDefined))("FR-1  FR-2  UC-1  UC-2", "")
  }

  def testFilterHasIssueOff2(): Unit = {
    import IssueCategory._
    testFilter(P6, F.hasIssue(Off, MissingData, BadData    ))("FR-1  FR-2", "")
    testFilter(P6, F.hasIssue(Off, MissingData, MissingData))("FR-1  FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(Off, MissingData, Futility   ))("FR-1  FR-2  UC-1", "")
    testFilter(P6, F.hasIssue(Off, MissingData, UserDefined))("FR-2  UC-1", "")
  }

  def testOtherTags_expansion(): Unit = {
    def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
    // DD    1        2           3            4           5                    6 (∅)    7                8
    val p1 = GReq() + t(priLow) + t(priHigh) + t(priMed) + t(priLow, priHigh) + t(wip) + t(priMed, v10) + t(v10, priLow) ! P1
    val p  = applyEventSuccessfully(p1, E.FieldCustomDelete(priField))
    val fmtRows = prefixWithPubid(p, rowToTagTxt(p, otherTags))

    // Order: pri=high pri=low pri=med v10
    testCB(p, C.OtherTags, None, ShowDead, fmtRows)(allSortsCB(2,
      asc  = "DD-3:pri=high  DD-5:pri=high,pri=low  DD-2:pri=low  DD-5:pri=low,pri=high  DD-8:pri=low,v1.0  DD-4:pri=med  DD-7:pri=med,v1.0  DD-8:v1.0,pri=low",
      desc = "DD-7:v1.0,pri=med  DD-8:v1.0,pri=low  DD-4:pri=med  DD-7:pri=med,v1.0  DD-2:pri=low  DD-5:pri=low,pri=high  DD-8:pri=low,v1.0  DD-3:pri=high  DD-5:pri=high,pri=low"))
  }

  def testAllTags_expansion(): Unit = {
    def t(ids: ApplicableTagId*) = GReq(reqType = dd).tag(ids: _*)
    // DD   1        2           3            4           5                    6        7                8
    val p = GReq() + t(priLow) + t(priHigh) + t(priMed) + t(priLow, priHigh) + t(wip) + t(priMed, v10) + t(v10, priLow) ! P1
    val fmtRows = prefixWithPubid(p, rowToTagTxt(p, allTags))

    // Order: pri=high pri=low pri=med v10 wip
    testCB(p, C.AllTags, None, ShowDead, fmtRows)(allSortsCB(1,
      asc  = "DD-3:pri=high  DD-5:pri=high,pri=low  DD-2:pri=low  DD-5:pri=low,pri=high  DD-8:pri=low,v1.0  DD-4:pri=med  DD-7:pri=med,v1.0  DD-8:v1.0,pri=low  DD-6:wip",
      desc = "DD-6:wip  DD-7:v1.0,pri=med  DD-8:v1.0,pri=low  DD-4:pri=med  DD-7:pri=med,v1.0  DD-2:pri=low  DD-5:pri=low,pri=high  DD-8:pri=low,v1.0  DD-3:pri=high  DD-5:pri=high,pri=low"))
  }

  // ===================================================================================================================

  override def tests = Tests {
    "sort" - {
      "reqCodes" - testReqCodes()
      "reqType"  - testReqType()
      "title"    - testTitle()
      "impSrc"   - testImpSrc()
      "impTgt"   - testImpTgt()
      "impCust"  - testCustomImpField()
      "custTxt"  - testCustomTextField()
      "otherTags" - {
        "sorted1"   - testOtherTags_sorted1()
        "sorted2"   - testOtherTags_sorted2()
        "unsorted"  - testOtherTags_unsorted()
        "inText"    - testOtherTags_inText()
        "expansion" - testOtherTags_expansion()
      }
      "allTags" - {
        "expansion" - testAllTags_expansion()
      }
      "custTag" - {
        "sorted1"  - testCustomTagField_sorted1()
        "sorted2"  - testCustomTagField_sorted2()
        "unsorted" - testCustomTagField_unsorted()
        "inText"   - testCustomTagField_inText()
      }
    }
    "applicability" - {
      "custTxt" - testApplicabilityOfCustomTextFields()
      "custTag" - testApplicabilityOfCustomTagFields()
      "custImp" - testApplicabilityOfCustomImpFields()
    }
    "filterDead" - {
      "rows"       - testFilterDeadRows()
      "impSrc"     - testFilterDeadImpsSrc()
      "impTgt"     - testFilterDeadImpsTgt()
      "impCust"    - testFilterDeadCustomImps()
      "tags"       - testFilterDeadTags()
      "tagsCust"   - testFilterDeadTagsInCustomTagField()
      "tagField"   - testFilterDeadCustomTagField()
    }
    "deadData" - {
      // These comprehensively test all combinations of dead data
      "tags" - {
        "hideDead" - DeadTags.testHideDead()
        "showDead" - DeadTags.testShowDead()
      }
      "issues" - {
        "hideDead" - DeadIssues.testHideDead()
        "showDead" - DeadIssues.testShowDead()
      }
    }
    "filter" - {
      "text"                 - testFilterText()
      "textPattern"          - testFilterTextPattern()
      "anyIssue"             - testFilterAnyIssue()
      "anyTag"               - testFilterAnyTag()
      "hasIssueOn1"          - testFilterHasIssueOn1()
      "hasIssueOn2"          - testFilterHasIssueOn2()
      "hasIssueOff1"         - testFilterHasIssueOff1()
      "hasIssueOff2"         - testFilterHasIssueOff2()
      "tag"                  - testFilterTag()
      "customIssue"          - testFilterCustomIssue()
      "reqType"              - testFilterReqType()
      "reqTypeExLive"        - testFilterReqTypeExLive()
      "reqTypeExDead"        - testFilterReqTypeExDead()
      "impliesAnyOf"         - testFilterImplies()
      "impliedByAnyOf"       - testFilterImpliedBy()
      "impliedByQuery"       - testFilterImpliedByQuery()
      "implyNothing"         - testFilterImplyNothing()
      "impFieldNA"           - testFilterImpFieldNA()
      "impFieldBlank"        - testFilterImpFieldBlank()
      "impFieldNotBlank"     - testFilterImpFieldNotBlank()
      "impFieldPos"          - testFilterImpFieldPos()
      "impFieldQuery"        - testFilterImpFieldQuery()
      "textFieldNA"          - testFilterTextFieldNA()
      "textFieldBlank"       - testFilterTextFieldBlank()
      "tagFieldNA"           - testFilterTagFieldNA()
      "tagFieldBlank"        - testFilterTagFieldBlank()
      "tagFieldDefault"      - testFilterTagFieldDefault()
      "ignoreNATags"         - testFilterIgnoreNATags()
      "tagsIncludesDefaults" - testFilterByTagsIncludesDefaults()
      "titleBlank"           - testFilterTitleBlank()
      "titleNotBlank"        - testFilterTitleNotBlank()
      "otherTags"            - testFilterOtherTags()
      "allTags"              - testFilterAllTags()
      "ncac"                 - testFilterNCAC()
      "ec"                   - testFilterEC()
      "allOf"                - testFilterAll()
      "anyOf"                - testFilterAny()
      "not"                  - testFilterNot()
    }
    "codeGroupsWithFilter" - {
      "hideDead" - testCodeGroupWhenFilteredAndHideDead()
      "showDead" - testCodeGroupWhenFilteredAndShowDead()
    }
    "reqCodeTree" - testReqCodeTree()
  }
}
