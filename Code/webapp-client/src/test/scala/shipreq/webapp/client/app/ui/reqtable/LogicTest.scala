package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.util.Multimap
import scalaz.std.set.setOrder
import scalaz.std.stream.streamInstance
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.client.test.ClientTestSettings._

object LogicTest extends TestSuite {

//  private sealed trait ExpType[A] {
//    def visible(vs: ViewSettings): Boolean
//    def extract(e: Expansion): List[A]
//  }
//  private case object ExpCodes extends ExpType[ReqCode] {
//    override def visible(vs: ViewSettings) = vs isVisible Column.Code
//    override def extract(e: Expansion) = e.reqCodes
//  }
//
//  private val exptypes = List(ExpCodes)

  private def codes(r: Row): List[ReqCode] = r match {
    case GenericReqRow(_, e)   => e.reqCodes
    case ReqCodeGroupRow(_, c) => c :: Nil
  }

  // sorting properties
  // ==================
  // reverse viewsettings.order = reverse results

  // TODO Nyaya: Here and in ReqsTest, Eval.distinctC not available?
  // TODO Nyaya: Eval needs the allPresent (etc) functions (and update README)

  private val distinctRows =
    Prop.distinctC[Stream, Row]("Rows")

  // Codes can be duplicated. Imagine sorting by MF > Code
  // private val distinctCodes = Prop.distinctC[Stream, ReqCode]("ReqCodes")

  private val allGenericReqsPresent =
    Prop.allPresent[LogicTests]("each generic req id has a row")(_.srcGReqIds, _.rowGReqIds)

  private val allRecCodesPresent =
    Prop.allPresent[LogicTests]("all req codes are displayed")(_.srcReqCodes, _.rowReqCodes)

  private val noEmptyAndNonEmptyMixed =
    Prop.atom[List[List[Any]]]("Either all empty or all non-empty", l => {
      val a = l.exists(_.isEmpty)
      val b = l.exists(_.nonEmpty)
      if (a && b)
        Some(s"Empty found with non-empty: $l")
      else
        None
    }).forallF[Stream]

  case class LogicTests(vs: ViewSettings, p: Project) {
    val gathered    = Logic.gather(vs, p)
    val gatheredG   = gathered.filterT[GenericReqRow]
    def rowReqCodes = gathered.flatMap(codes(_).toStream)
    def rowGReqIds  = gatheredG.map(_.req.id).toSet
    def srcGReqIds  = p.reqs.data.reqs.keys.filterT[GenericReq.Id].toSet
    def srcReqCodes = p.reqCodes.data.codeSet

    def noEmptyAndNonEmptyReqCodesMixed =
      noEmptyAndNonEmptyMixed(
        Multimap.empty[Req.Id, List, List[ReqCode]]
          .addPairs(gatheredG.map(r => (r.req.id, r.exp.reqCodes)): _*)
          .m.values.toStream
      )

    def reqCodes = "Req Codes" rename_: (
      allRecCodesPresent             (this).liftL ∧
      noEmptyAndNonEmptyReqCodesMixed      .liftL )

    // TODO doesn't check expanded implications
    // expansions per expandable A
    //   - list A = req.A
    //   - if req.A then no rows without As

    def all = "Logic.gather" rename_: (
      distinctRows         (gathered).liftL ∧
      allGenericReqsPresent(this)    .liftL ∧
      reqCodes)
  }

  def gen: Gen[LogicTests] =
    for {
      p  <- RandomData.project
      vs <- ReqTableTest.rndViewSettings(p)
    } yield
      LogicTests(vs, p)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all) //(implicitly[Settings].setSeed(0).setDebug.setSampleSize(10))
  }
}
