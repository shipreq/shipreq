package shipreq.webapp.client.project.feature.deletion

import nyaya.test.DefaultSettings
import nyaya.test.PropTest._
import utest._
import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{IMap, Util}
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import DeletionRestorationLogic.{Data, GroupRow}
import UnsafeTypes._
import WebappTestUtil._

object DeletionLogicTestData {
  import ProjectDsl._
  import ProjectDslInternals.{ToState, Composite}
  import SampleProject.Values._

  private var _selectedReqIds = Set.empty[ReqId]
  private var _selectedRCGs = Set.empty[ReqCodeGroupId]

  private var _expectInitialReqs = Set.empty[ReqId]
  private var _expectInitialRCGs = Set.empty[ReqCodeGroupId]

  private var _expectUnselectedReqs = Set.empty[ReqId]
  private var _expectDeletableRCGs = IMap.empty[ReqCodeGroupId, GroupRow](_.id)

  private implicit class GReqExt(private val x: GReq) {
    private val id = x.id.getOrElse(sys error s"$x needs an id.")
    def select = {
      _selectedReqIds += id
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

    /** Req will be ignored and not appear in the Deletion screen */
    def no = x
  }

  private val pairStr = "^(\\d+):(.*)$".r
  private def split(s: String) = s.trim.split(" *, *").iterator.filterNot(_.isEmpty)
  private def pairs[A](s: String, f: Int => A) = split(s).map{ case pairStr(a,b) => (f(a.toInt), b)}.toSet

  private implicit class RCGroupExt(private val x: RCGroup) {
    private val id = x.id.getOrElse(sys error s"$x needs an id.")
    private def codeStr = ReqCode.Value.toStr(x.code, '.')

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
      _expectDeletableRCGs += GroupRow(LiveCodeGroup(id, x.title), codeStr, ∅, ∅)
      x
    }

    def subs(req: String, rcg: String) = {
      // No point showing the same prefix in all subcodes
      val fixCodeR = ("^" + codeStr.replace(".", "\\.")).r
      val fixCode: EndoFn[String] = fixCodeR.replaceFirstIn(_, "")

      _expectDeletableRCGs = _expectDeletableRCGs.modOrPut(id, row =>
        row.copy(
          subReqs = pairs(req, GenericReqId).map(_ map2 fixCode),
          subGroups = pairs(rcg, ReqCodeGroupId).map(_ map2 fixCode))
        , sys error s"$id not found")
      x
    }

    /** Group will be ignored and not appear in the Deletion screen */
    def no = x
  }

  private val dead = ReqCode.Node("dead")

  private def ___req(id: GenericReqId, codes: ReqCode.Value*)(f: EndoFn[GReq]): Composite = {
    val a = f(GReq(id = id, codes = codes.toSet))
    val d1 = a.copy(live = Dead, id = GenericReqId(id.value + 10000), codes = a.codes.map(_ :+ dead))
    val d2 = a.copy(live = Dead, id = GenericReqId(id.value + 20000), codes = a.codes.map(dead +: _))
    a + d1 + d2
  }

  private def rcg___(id: ReqCodeGroupId, code: ReqCode.Value)(f: EndoFn[RCGroup]): Composite = {
    val a = f(RCGroup(id = id, code = code))
    val d1 = DeadReqCode(code :+ dead, id = ReqCodeGroupId(id.value + 10000))
    val d2 = DeadReqCode(dead +: code, id = ReqCodeGroupId(id.value + 20000))
    a + d1 + d2
  }

  // =================================================================================================================
  private val p =
    ( ___req(100)(_.select)          // Simple reqs - no relations
    + ___req(101)(_.no)
    + rcg___(900, "a")(_.select)     // Simple groups - no relations
    + rcg___(901, "b")(_.no)

    // =================================================================================================================
    // ReqCodes

    + rcg___(910, "c"      )(_.auto.subs("110:c.a.1", "912:c.a.1.g, 911:c.a")) // Select because only child group being (indirectly) deleted
    + rcg___(911, "c.a"    )(_.auto.subs("110:c.a.1", "912:c.a.1.g")         ) // Select because all children being (directly) deleted
    + ___req(110, "c.a.1"  )(_.select                                        )
    + rcg___(912, "c.a.1.g")(_.auto                                          ) // Select because its sub-selection with no children

    + rcg___(920, "cn1"       )(_.no    ) // Negative test of above (live sibling)
    + rcg___(921, "cn1.a"     )(_.no    ) // Nope: cos of live req at .2
    + ___req(120, "cn1.a.1"   )(_.select)
    + rcg___(922, "cn1.a.1.g1")(_.auto  )
    + ___req(121, "cn1.a.2"   )(_.no    )
    + rcg___(923, "cn1.a.2.g" )(_.no    ) // Nope: cos of live parent

    + rcg___(930, "cn2"          )(_.no    ) // Negative test of above (live child)
    + rcg___(931, "cn2.a"        )(_.no    ) // Nope: cos of live req at .1.g.s
    + ___req(130, "cn2.a.1"      )(_.select)
    + rcg___(932, "cn2.a.1.g"    )(_.no    ) // Nope: cos of live req at .s
    + ___req(131, "cn2.a.1.g.s"  )(_.no    )
    + rcg___(933, "cn2.a.1.g.s.g")(_.no    ) // Nope: cos of live parent

    + rcg___(940, "d"            )(_.auto.subs("140:d.a.1, 141:d.a.1.b.g.1, 142:d.a.1.b.g.2", "941:d.a.1.b.g, 942:d.a.1.b.g.2.x"))
    + ___req(140, "d.a.1"        )(_.select                                                                                      )
    + rcg___(941, "d.a.1.b.g"    )(_.auto.subs("141:d.a.1.b.g.1, 142:d.a.1.b.g.2", "942:d.a.1.b.g.2.x")                          )
    + ___req(141, "d.a.1.b.g.1"  )(_.select                                                                                      )
    + ___req(142, "d.a.1.b.g.2"  )(_.select                                                                                      )
    + rcg___(942, "d.a.1.b.g.2.x")(_.auto                                                                                        )

    + rcg___(950, "dn1"            )(_.no    ) // Negative test of above
    + ___req(150, "dn1.a.1"        )(_.select)
    + rcg___(951, "dn1.a.1.b.g"    )(_.no    )
    + ___req(151, "dn1.a.1.b.g.1"  )(_.no    )
    + ___req(152, "dn1.a.1.b.g.2"  )(_.select)
    + rcg___(952, "dn1.a.1.b.g.2.x")(_.auto  )

    // =================================================================================================================
    // Implications

    + ___req(300)(_            .select)
    + ___req(301)(_.impSrc(300).auto  ) // All implying reqs marked for deletion
    + ___req(302)(_.impSrc(301).auto  ) // All implying reqs marked for deletion

    + ___req(310)(_                 .select )
    + ___req(311)(_                 .no     )
    + ___req(312)(_.impSrc(310, 311).visible) // Visible (due to 310) but not selected (due to 311)
    + ___req(313)(_.impSrc(312)     .visible) // Visible (due to 312) but not selected (due to 311)

    + ___req(320)(_                 .no     )
    + ___req(321)(_.impSrc(320)     .select )
    + ___req(322)(_.impSrc(321)     .auto   ) // All implying reqs marked for deletion
    + ___req(323)(_.impSrc(321, 320).visible) // Visible (due to 321) but not selected (due to 320)

    // =================================================================================================================
    // Implications & ReqCodes

    + ___req(500             )(_            .select                                   )
    + ___req(501, "both0.1"  )(_.impSrc(500).auto                                     ) // Sole implied marked for deletion
    + rcg___(700, "both0"    )(_            .auto.subs("501:both0.1", "701:both0.1.x")) // Sole child (501) marked for deletion
    + rcg___(701, "both0.1.x")(_            .auto                                     ) // Sole parent (501) marked for deletion

    + ___req(510             )(_                .select                                      )
    + ___req(511             )(_                .no                                          )
    + ___req(512, "both1.1"  )(_.impSrc(510,511).visible                                     ) // 1 live imp, 1 deleting imp
    + rcg___(710, "both1"    )(_                .visible.subs("512:both1.1", "711:both1.1.x")) // Sole child visible but not initially selected
  //+ rcg___(711, "both1.1.x")(_                .visible                                     ) // IGNORE: Sole parent visible but not initially selected
    + rcg___(711, "both1.1.x")(_                .auto                                        ) // It has no live children on screen init. Just easier.

    ).defaultReqType(fr) ! SampleProject.project
  // =================================================================================================================

  // Don't use implications on ReqIds < 300; their expectations are automatically determined
  // For implications use 300+ and declare expected results here manually
  val expectedDeletableReqs_manual =
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

  lazy val result               = DeletionRestorationLogic.forReqsAndCodeGroups__TEST_ONLY(p, NonEmptySet force _selectedReqIds, _selectedRCGs)
  lazy val expectInitialReqs    = _expectInitialReqs
  lazy val expectInitialRCGs    = _expectInitialRCGs
  lazy val expectUnselectedReqs = _expectUnselectedReqs
  lazy val expectDeletableRCGs  = _expectDeletableRCGs.values.toVector.sortBy(_.codeStr)

  def fmtReqRows(p: Data): String =
    Util.quickSB { sb =>
      p.actionableReqs.foreach { r =>
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

object DeletionLogicTest extends TestSuite {
  import DeletionLogicTestData._

  implicit val rcgRowEquality = UnivEq.derive[GroupRow]

  override def tests = Tests {

    "deletableReqs" - {
      val manual        = expectedDeletableReqs_manual
      val parseManual   = "^[. ]*?(\\d+)(?: .*)?$".r
      val codesInManual = manual.split("\n").map { case parseManual(i) => i.toInt }
      val autoCodes     = (expectUnselectedReqs | expectInitialReqs).map(_.value) -- codesInManual
      val autoText      = autoCodes.toVector.sorted.mkString("\n")
      val expect        = autoText + "\n" + manual

      val expectInManual = autoCodes.filter(_ >= 300)
      if (expectInManual.nonEmpty)
        fail("The following codes are expected to be specified manually: " + expectInManual)

      assertEq(fmtReqRows(result), expect)
    }

    "deletableGroups" - {
      val e = expectDeletableRCGs
      val a = result.actionableGroups
      assertSet("Deletable groups", a.toSet, e.toSet)
      assertEq("Deletable group count", a.length, e.length)
      for ((ar,er) <- a zip e)
        assertEq("Deletable group", ar, er)
    }

    "initialReqs"   - assertSet("Initial reqs"  , result.initialReqs, expectInitialReqs)
    "initialGroups" - assertSet("Initial groups", result.initialGroups, expectInitialRCGs)

    "props" - {
      val g = DeletionProps.RandomData(Delete).genProps
      g.mustSatisfyE(_.allProps)(DefaultSettings.propSettings.setSampleSize(2))
      // scala.util.Try(g.bugHunt(10009, 8)(Prop.eval(_.allProps))(DefaultSettings.propSettings.setDebug)); ()
    }
  }
}
