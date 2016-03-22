package shipreq.webapp.client.app.reqdetail

import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ProjectSpaMain.Page
import shipreq.webapp.client.app.ProjectSpaTestDsl
import shipreq.webapp.client.data.{HideDead, ShowDead}
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
    *.focus("Life row").value(_.obs.generic.lifeRow.innerText).assert(expect)

  val reporterFieldExistence =
    visibleFields.assert.existenceOf("Reporter")(_.obs.generic.filterDead :: ShowDead)

  override def tests = TestSuite {

    'badReqType - testError("QL-1", "Type QL not found.")
    'badReq     - testError("FR-9", "FR-9 not found.")

    'gr - test("FR-1")(*.emptyTest addInvariants testLifeRowInnerText("Alive.Kill"))

    'uc - test("UC-1")(Test(
      allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1")
        +> addTailStepEC
        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1", "1.E.1")
        >> delStep("1.1")
        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1")
        >> shiftStepLeft("1.0.3")
        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.1", "1.E.1")
        >> shiftStepRight("1.1")
        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1")
        >> addStep("1.E.1")
        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1", "1.E.1.a")
    ))

    // TODO emptyTest addInvariants = abuse

    'deadExplicitly - test("MF-19")(*.emptyTest addInvariants testLifeRowInnerText("Dead.Resurrect"))

    'deadImplicitly - test("SI-1")(*.emptyTest addInvariants testLifeRowInnerText("Dead."))

    'deadImplicitlyAndExplicitly - test("SI-2")(*.emptyTest addInvariants testLifeRowInnerText("Dead."))

    'deadFields - test("UC-1")(Test(
      filterDeadToggle
        .addCheck(reporterFieldExistence.beforeAndAfter)
        .times(3)
    ))

    'inapplicableFields - {
      def check(expectVisible: Boolean) =
        visibleFields.assertB(expectVisible).contains("Description")
      def t(pubid: ExternalPubid, expectVisible: Boolean) =
        test(pubid)(*.emptyTest addInvariants check(expectVisible))
      'mf1 - t("MF-1", true)
      'fr1 - t("FR-1", false)
    }

    'deleteRestore - test("UC-1")(Test(
      changeLife.updateState(stateMode set Mode.Delete) <+ life.assert(Live)
      >> deleteDelete                                   +> life.assert(Dead)
      >> changeLife                                     +> life.assert(Live)
    ))

    'editors - test("UC-1")(Test(
      doubleClickTitle                     +> editorCount.assert.beforeAndAfter(0, 1) <+ filterDead.assert(HideDead)
      >> doubleClickFieldValue("Notes")    +> editorCount.assert(2)
      >> showDead                          +> editorCount.assert(2)
      >> doubleClickFieldValue("Reporter") +> editorCount.assert(2) // dead field
      >> hideDead                          +> editorCount.assert(2)
    , reporterFieldExistence))

  }
}
