package shipreq.webapp.client.app.ui.reqtable

import shipreq.webapp.base.text.PlainText

import scala.collection.immutable.SortedSet
import utest._
import shipreq.base.util.{UnivEq, IMap, Util}
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import BaseTestUtil._
import Deletion.{Props1, RcgRow, ReqRow}
import UnsafeTypes._

// Because utest.TestSuite slows down Intellij ridiculously
object DeletionTestData {

  /*
  case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])

  case class RcgRow(group    : LiveReqCodeGroup,
                    codeStr  : String,
                    subStatic: SortedSet[String],
                    subReqs  : List[(ReqId, String)],
                    subGrps  : List[(ReqCodeId, String)])

  case class Props1(project      : Project,
                    reqRows      : Vector[ReqRow],
                    deletableRCGs: Vector[RcgRow],
                    initialState : State)

  def initProps1(project             : Project,
                 directlySelectedReqs: Traversable[Req],
                 directlySelectedRcgs: Set[ReqCodeId]): Props1 = {

  case class State(selectedReqIds: Set[ReqId], selectedRCGs: Selection[ReqCodeId])
   */

  import ProjectDsl._
  import SampleProject.Values._

  private var _selectedReqIds = Vector.empty[ReqId]
  private var _selectedRCGs = Set.empty[ReqCodeId]

  private var _expectInitialReqs = Set.empty[ReqId]
  private var _expectInitialRCGs = Set.empty[ReqCodeId]

  private var _expectUnselectedReqs = Set.empty[ReqId]
  private var _expectDeletableRCGs = IMap.empty[ReqCodeId, RcgRow](_.id)
//  private var _expectDeletableRcgSubs = UnivEq.emptySetMultimap[ReqCodeId, Int]

  private implicit class GReqExt(private val x: GReq) {
    private val id = x.id.getOrElse(sys error s"$x needs an id.")
    def select = {
      _selectedReqIds :+= id
      auto
    }
    /** Expect to be selected automatically */
    def auto = {
      _expectInitialReqs += id
      x
    }
    def visible = {
      _expectUnselectedReqs += id
      x
    }
  }

  private val pairStr = "^(\\d+):(.*)$".r
  private def split(s: String) = s.trim.split(" *, *").toIterator.filterNot(_.isEmpty)
  private def pairs[A](s: String, f: Int => A) = split(s).map{ case pairStr(a,b) => (f(a.toInt), b)}.toSet


  private implicit class RCGroupExt(private val x: RCGroup) {
    private val id = x.id.getOrElse(sys error s"$x needs an id.")

    def select = {
      _selectedRCGs += id
      auto
    }

    /** Expect to be selected automatically */
    def auto = {
      _expectInitialRCGs += id
      visible
    }

    def visible = {
      _expectDeletableRCGs += RcgRow(LiveReqCodeGroup(id, x.title), ReqCode.valueToStr(x.code, '.'), ∅, ∅)
      x
    }

    def subs(req: String, rcg: String) = {
      _expectDeletableRCGs = _expectDeletableRCGs.modOrPut(id, row =>
        row.copy(
          subReqs = pairs(req, GenericReqId),
          subGrps = pairs(rcg, ReqCodeId))
      , ???
      )
//      _expectDeletableRcgSubs = _expectDeletableRcgSubs.addvs(id, ids.toSet)
      x
    }
  }

  private def ___req(id: GenericReqId, codes: ReqCode.Value*) =
    GReq(id = id, codes = codes.toSet)

  private def rcg___(id: ReqCodeId, code: ReqCode.Value) =
    RCGroup(id = id, code = code)

  private val p =
    ( ___req(100).select          // Simple reqs - no relations
    + ___req(101)
    + rcg___(900, "a").select     // Simple groups - no relations
    + rcg___(901, "b")

    + rcg___(910, "c"      ).auto   .subs("110:c.a.1", "912:c.a.1.g, 911:c.a")    // Select because only child group being (indirectly) deleted
    + rcg___(911, "c.a"    ).auto   .subs("110:c.a.1", "912:c.a.1.g")             // Select because all children being (directly) deleted
    + ___req(110, "c.a.1"  ).select
    + rcg___(912, "c.a.1.g").auto      // Select because its sub-selection with no children

    + rcg___(920, "cn1")                 // Negative test of above (live sibling)
    + rcg___(921, "cn1.a")               // Nope: cos of live req at .2
    + ___req(120, "cn1.a.1").select
    + rcg___(922, "cn1.a.1.g1").auto
    + ___req(121, "cn1.a.2")
    + rcg___(923, "cn1.a.2.g")           // Nope: cos of live parent

    + rcg___(930, "cn2")                 // Negative test of above (live child)
    + rcg___(931, "cn2.a")               // Nope: cos of live req at .1.g.s
    + ___req(130, "cn2.a.1").select
    + rcg___(932, "cn2.a.1.g")           // Nope: cos of live req at .s
    + ___req(131, "cn2.a.1.g.s")
    + rcg___(933, "cn2.a.1.g.s.g")       // Nope: cos of live parent

    + rcg___(940, "d"      ).auto .subs("140:d.a.1, 141:d.a.1.b.g.1, 142:d.a.1.b.g.2", "941:d.a.1.b.g, 942:d.a.1.b.g.2.x")
    + ___req(140, "d.a.1"  ).select
    + rcg___(941, "d.a.1.b.g").auto .subs("141:d.a.1.b.g.1, 142:d.a.1.b.g.2", "942:d.a.1.b.g.2.x")
    + ___req(141, "d.a.1.b.g.1"  ).select
    + ___req(142, "d.a.1.b.g.2"  ).select
    + rcg___(942, "d.a.1.b.g.2.x").auto

    + rcg___(950, "dn1"            )      // Negative test of above
    + ___req(150, "dn1.a.1"        ).select
    + rcg___(951, "dn1.a.1.b.g"    )
    + ___req(151, "dn1.a.1.b.g.1"  )
    + ___req(152, "dn1.a.1.b.g.2"  ).select
    + rcg___(952, "dn1.a.1.b.g.2.x").auto

    // Implications

    + ___req(300).select
    + ___req(301).impSrc(300).auto         // All implying reqs marked for deletion
    + ___req(302).impSrc(301).auto         // All implying reqs marked for deletion

    + ___req(310).select
    + ___req(311)
    + ___req(312).impSrc(310, 311).visible       // Visible (due to 310) but not selected (due to 311)
    + ___req(313).impSrc(312)     .visible       // Visible (due to 312) but not selected (due to 311)

    + ___req(320)
    + ___req(321).impSrc(320).select
    + ___req(322).impSrc(321).auto         // All implying reqs marked for deletion
    + ___req(323).impSrc(321, 320).visible // Visible (due to 321) but not selected (due to 320)

    // Implications & ReqCodes

    + ___req(500).select
    + ___req(501, "both0.1").impSrc(500).auto                         // Sole implied marked for deletion
    + rcg___(700, "both0").auto .subs("501:both0.1", "701:both0.1.x")   // Sole child (501) marked for deletion
    + rcg___(701, "both0.1.x").auto                                   // Sole parent (501) marked for deletion

    + ___req(510).select
    + ___req(511)
    + ___req(512, "both1.1").impSrc(510,511).visible                      // 1 live imp, 1 deleting imp
    + rcg___(710, "both1").visible .subs("512:both1.1", "711:both1.1.x")  // Sole child visible but not initially selected
  //+ rcg___(711, "both1.1.x").visible                                    // IGNORE: Sole parent visible but not initially selected
    + rcg___(711, "both1.1.x").auto                                       // It has no live children on screen init. Just easier.

    ).defaultReqType(fr) ! SampleProject.project

  private val selectedReqs: Vector[Req] =
    _selectedReqIds.map(p.reqs.req)

println("_"*120)
  lazy val result = Deletion.initProps1(p, selectedReqs, _selectedRCGs)
  lazy val expectInitialReqs = _expectInitialReqs
  lazy val expectInitialRCGs = _expectInitialRCGs
  lazy val expectUnselectedReqs = _expectUnselectedReqs

  lazy val expectDeletableRCGs =
    _expectDeletableRCGs.values.toVector.sortBy(_.codeStr)
//  lazy val expectDeletableRCGs = {
//    _expectDeletableRCGs.values.iterator.map { r =>
//      val extra = _expectDeletableRcgSubs(r.id)
//      if (extra.isEmpty)
//        r
//      else {
//        val (a,b) = extra.partition(_ < 600)
////        a.iterator.map(GenericReqId).flatMap(i => p.reqCodes.activeReqCodesByReqId(i).toIterator.map(code =>
////          PlainText.reqCode(code))
//        b.map(p.reqCodes.)
//        r
//      }
//    }
//  }

  def fmtReqRows(p: Props1): String =
    Util.quickSB { sb =>
      p.reqRows.foreach { r =>
        if (sb.nonEmpty)
          sb append '\n'
        for (_ <- 1 to r.indent)
          sb append ". "
        sb append r.req.id.value
        if (r.impliedBy.nonEmpty) {
          sb append " <-"
          for (i <- r.impliedBy) {
            sb append ' '
            sb append i.id.value
          }
        }
      }
    }
}

object DeletionTest extends TestSuite {
  import DeletionTestData._

  implicit val rcgRowEquality = UnivEq.derive[RcgRow]

  val pairStr = "^(\\d+):(.*)$".r

  override def tests = TestSuite {

    'reqRows {

      val manual =
      """
        |300
        |. 301 <- 300
        |. . 302 <- 301
        |310
        |. 312 <- 310 311
        |. . 313 <- 312
        |321 <- 320
        |. 322 <- 321
        |. 323 <- 320 321
        |500
        |. 501 <- 500
        |510
        |. 512 <- 510 511
      """.stripMargin.trim

      val parseManual = "^[. ]*?(\\d+)(?: .*)?$".r

      val codesInManual = manual.split("\n").map { case parseManual(i) => i.toInt }
      val autoCodes = (expectUnselectedReqs | expectInitialReqs).map(_.value) -- codesInManual
      val autoText = autoCodes.toVector.sorted.mkString("\n")
      val expect = autoText + "\n" + manual

      val expectInManual = autoCodes.filter(_ >= 300)
      if (expectInManual.nonEmpty)
        fail("The following codes are expected to be specified manually: " + expectInManual)

      assertEq(fmtReqRows(result), expect)
    }

    'deletableRCGs {
      /*
      val expectations = Vector.newBuilder[RcgRow]
      def expect(id: ReqCodeId, code: String, subReqs: String = "", subGrps: String = ""): Unit = {
        def split(s: String) = s.trim.split(" *, *").toIterator.filterNot(_.isEmpty)
        def pairs[A](s: String, f: Int => A) = split(s).map{ case pairStr(a,b) => (f(a.toInt), b)}.toList
        val sr = pairs(subReqs, GenericReqId.apply)
        val sg = pairs(subGrps, ReqCodeId.apply)
        val e = RcgRow(LiveReqCodeGroup(id, Vector.empty), code, sr, sg)
        expectations += e
      }

      expect(900, "a")

      expect(910, "c",   subReqs = "110:c.a.1", subGrps = "911:c.a")
      expect(911, "c.a", subReqs = "110:c.a.1")

//      expect(920, "cn1",   subStatic = "cn1.a.2", subReqs = "120:cn1.a.1", subGrps = "921:cn1.a")
//      expect(921, "cn1.a", subStatic = "cn1.a.2", subReqs = "120:cn1.a.1")
      expect(922, "cn1.a.1.g")

//      expect(930, "cn2",   subStatic = "cn2.a.1.s", subReqs = "130:cn2.a.1", subGrps = "931:cn2.a")
//      expect(931, "cn2.a", subStatic = "cn2.a.1.s", subReqs = "130:cn2.a.1")

      val e = expectations.result()
      */
      val e = expectDeletableRCGs
      val a = result.deletableRCGs
      assertSet("Deletable RCGs", a.toSet, e.toSet)
      assertEq("Deletable RCG count", a.length, e.length)
      for ((ar,er) <- a zip e)
        assertEq("Deletable RCG", ar, er)
    }

    'initalReqs - assertSet("Initially selected reqs", result.initialState.selectedReqIds, expectInitialReqs)
    'initalRCGs - assertSet("Initially selected RCGs", result.initialState.selectedRCGs.selected, expectInitialRCGs)
  }
}
