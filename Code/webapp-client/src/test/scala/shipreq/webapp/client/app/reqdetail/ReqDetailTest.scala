package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.test._
import shipreq.base.util.univEqOps
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.SampleProject3._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ProjectSpaTestDsl
import shipreq.webapp.client.test._
import teststate.Exports._
import utest._
import DomZipper.Implicits._
import ReactTestUtils.Simulate

object ReqDetailTest extends TestSuite {
  import ReqDetailTestDsl._

  PrepareEnv()

  //action: *.Action = *.emptyAction
  def runTest(ep: ExternalPubid, error: Boolean)(test: *.TestContent): Unit = {
    val tc = test.addInvariants(invariants)

    import ProjectSpaTestDsl._

    ProjectSpaTestDsl.runTest(
      setPageToReqDetail(ep, if (error) None else Some(ep))
        >> liftReqDetailTests(tc).asAction(s"Req Detail (${PlainText.pubid(ep)})")
    )
  }

  def testError(ep: ExternalPubid, error: String): Unit =
    runTest(ep, true)(*.emptyTest addInvariants checkErrorReason(error))

  def test(ep: ExternalPubid)(test: *.TestContent = *.emptyTest): Unit = {
    runTest(ep, false)(test)
  }

  // yeah i'm being lazy
  def testLifeRowInnerText(expect: String) =
    *.focus("Life row").value(_.obs.generic.lifeRow.innerText).assert.equal(expect)

  override def tests = TestSuite {

    'badReqType - testError("QL-1", "Type QL not found.")
    'badReq     - testError("FR-9", "FR-9 not found.")

    'gr - test("FR-1")(*.emptyTest addInvariants testLifeRowInnerText("Alive.Kill"))

    'uc - test("UC-1")(Test(
      addTailStepEC
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1").before)
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1", "1.E.1").after)
      >> delStep("1.1")
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1").after)
      >> shiftStepLeft("1.0.3")
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.1", "1.E.1").after)
      >> shiftStepRight("1.1")
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1").after)
      >> addStep("1.E.1")
        .addCheck(allSteps.assert.equal("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1", "1.E.1.a").after)
      ))


    // TODO emptyTest addInvariants = abuse

    'deadExplicitly - test("MF-19")(*.emptyTest addInvariants testLifeRowInnerText("Dead.Resurrect"))

    'deadImplicitly - test("SI-1")(*.emptyTest addInvariants testLifeRowInnerText("Dead."))

    'deadImplicitlyAndExplicitly - test("SI-2")(*.emptyTest addInvariants testLifeRowInnerText("Dead."))

//    val u = ReqDetail.Props("EMMEFF", 5, project).component
//    val m = ReactTestUtils.renderIntoDocument(u)
//    ReqDetailObs(DomZipper(m))

  }
}
