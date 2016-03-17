package shipreq.webapp.client.app.reqdetail

import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ProjectSpaMain.Page
import shipreq.webapp.client.app.ProjectSpaTestDsl
import shipreq.webapp.client.data.ShowDead
import shipreq.webapp.client.test._
import TestState._
import utest._

object ReqDetailTest extends TestSuite {
  import ReqDetailTestDsl._

  PrepareEnv()

  //action: *.Action = *.emptyAction
  def runTest(ep: ExternalPubid, error: Boolean)(tc: *.TestContent): Unit = {

    import ProjectSpaTestDsl._

    ProjectSpaTestDsl.runTest(
      liftReqDetailTests(tc).asAction(s"Req Detail (${PlainText.pubid(ep)})"),
      page = Page.ReqDetail(ep),
      rd = State(ep, if (error) Mode.Error else Mode.Details))
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

    'deadFields - test("UC-1")(Test(
      filterDeadToggle
        .addCheck(visibleFields.assert.existenceOf("Reporter")(_.obs.generic.filterDead :: ShowDead).beforeAndAfter)
        .times(3)
    ))

    'inapplicableFields - {
      def check(expectVisible: Boolean) =
        visibleFields.assert(expectVisible).contains("Description")
      def t(pubid: ExternalPubid, expectVisible: Boolean) =
        test(pubid)(*.emptyTest addInvariants check(expectVisible))
      'mf1 - t("MF-1", true)
      'fr1 - t("FR-1", false)
    }

    'deleteRestore - test("UC-1")(Test(
      changeLife.updateState(stateMode set Mode.Delete).addCheck(life.assert.equal(Live).before)
      >> deleteDelete                                  .addCheck(life.assert.equal(Dead).after)
      >> changeLife                                    .addCheck(life.assert.equal(Live).after)
    ))


  }
}
