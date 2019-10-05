package shipreq.webapp.client.project.app.reqdetail

import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.UiText
import shipreq.webapp.base.test.{SampleProject5, SampleProject6}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.root.Routes.Page
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.test._

object ReqDetailTest extends TestSuite {
  import ReqDetailTestDsl._

  PrepareEnv()

  private def defaultProject = SampleProject5.project

  private def runTest(ep: ExternalPubid, error: Boolean, project: Project = defaultProject)(p: *.Plan): Unit = {

    import ProjectSpaTestDsl._

    ProjectSpaTestDsl.runTest(
      liftReqDetailTests(p).asAction(s"Req Detail (${PlainText.pubid(ep)})"),
      page = Page.ReqDetail(ep),
      project = project,
      rd = State(ep, if (error) Mode.Error else Mode.Details))
  }

  private def testError(ep: ExternalPubid, error: String): Unit =
    runTest(ep, true)(Plan invariants checkErrorReason(error))

  private def test(ep: ExternalPubid, project: Project = defaultProject)(test: *.Plan): Unit =
    runTest(ep, false, project)(test)

  // yeah i'm being lazy
  private def testLifeRowInnerText(expect: String) =
    *.focus("Life row").value(_.obs.generic.lifeRow.innerText).assert(expect)

  private val reporterFieldExistence =
    visibleFields.assert.existenceOf("Reporter")(_.obs.generic.filterDead is ShowDead)

  private val liveCanDelete  = UiText.Life.live + "." + UiText.Life.delete
  private val deadCanRestore = UiText.Life.dead + "." + UiText.Life.restore
  private val deadNoRestore  = UiText.Life.dead + "."

  override def tests = Tests {

    'badReqType - testError("QL-1", "QL-1 doesn't exist")
    'badReq     - testError("FR-9", "FR-9 doesn't exist")

    'gr - test("FR-1")(Plan invariants testLifeRowInnerText(liveCanDelete))

    'uc {

      'tree - test("UC-1")(Plan.action( allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1")
          +> addTailStepEC           +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1", "1.E.1")
          >> delStep("1.1")          +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1")
          >> shiftStepLeft("1.0.3")  +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.1", "1.E.1")
          >> shiftStepRight("1.1")   +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1")
          >> addStep("1.E.1")        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.E.1", "1.E.1.a")
          >> filterDeadToggle        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.X.0", "1.X.0.1", "1.E.1", "1.E.1.a")
          >> filterDeadToggle
          // These two test shiftRightV, not just shiftRight
          >> delStep("1.0.2")        +> allSteps.assert("1.0", "1.0.1", "1.0.2", "1.E.1", "1.E.1.a")
          >> shiftStepRight("1.0.2") +> allSteps.assert("1.0", "1.0.1", "1.0.1.a", "1.E.1", "1.E.1.a")
          // Test restore
          >> filterDeadToggle        +> allSteps.assert("1.0", "1.0.1", "1.0.1.a", "1.0.X.1", "1.X.0", "1.X.0.1", "1.E.1", "1.E.1.a")
          >> restoreStep("1.X.0")    +> allSteps.assert("1.0", "1.0.1", "1.0.1.a", "1.0.X.1", "1.1", "1.1.1", "1.E.1", "1.E.1.a")
      ))

      'text - test("UC-1")(Plan.action(
             stepText("1.1").assert("Have no food")
          +> stepText("1.0.2").assert("Put in mouth←1.1.1")
          +> stepText("1.1.1").assert("Steal food→1.0.2")

          +> editStepText("1.1", "No food? --> 1.0.2 <-- 1.1.1 ")
          +> stepText("1.1").assert("No food?←1.1.1→1.0.2")
          +> stepText("1.0.2").assert("Put in mouth←1.1, 1.1.1")
          +> stepText("1.1.1").assert("Steal food→1.0.2, 1.1")

          >> editStepText("1.1", "No food? <--1.0.2-->1.1.1")
          +> stepText("1.1").assert("No food?←1.0.2→1.1.1")
          +> stepText("1.0.2").assert("Put in mouth←1.1.1→1.1")
          +> stepText("1.1.1").assert("Steal food←1.1→1.0.2")

          >> editStepText("1.1.1", "-->.0.3 .1")
          +> stepText("1.1").assert("No food?←1.0.2, 1.1.1")
          +> stepText("1.0.2").assert("Put in mouth→1.1")
          +> stepText("1.1.1").assert("→1.0.3, 1.1")
          +> stepText("1.0.3").assert("Still hungry?←1.1.1→1.0.1")

          >> editStepText("1.1.1", "uc ref to step [.0.3]  <---- ")
          +> stepText("1.1").assert("No food?←1.0.2")
          +> stepText("1.0.2").assert("Put in mouth→1.1")
          +> stepText("1.1.1").assert("uc ref to step [1.0.3]")
          +> stepText("1.0.3").assert("Still hungry?→1.0.1")
      ))

      'dead - test("UC-1")(Plan.action(
        changeLife.updateState(stateMode set Mode.Delete) >> deleteScreenDelete
          +> life.assert(Dead)
          +> tailStepAC.test("doesn't exist")(_.isEmpty)
          +> tailStepEC.test("doesn't exist")(_.isEmpty)
      ))

      'deadFlow - test("UC-2", SampleProject6.project)(Plan.action(
           addStep("2.0")
        >> addStep("2.0.1")
        >> addStep("2.0.2")
        >> addStep("2.0.3")
        >> addStep("2.0.4")
        >> addStep("2.0.5")
        +> allSteps.assert("2.0", "2.0.1", "2.0.2", "2.0.3", "2.0.4", "2.0.5", "2.0.6")

        >> editStepText("2.0.3", "cat <-- .0.1 .0.2 --> .0.4 .0.5")
        >> delStep("2.0.4")
        >> delStep("2.0.2")
        +> allSteps.assert("2.0", "2.0.1", "2.0.2", "2.0.3", "2.0.4")
        +> stepText("2.0.2").assert("cat←2.0.1→2.0.3")

        >> filterDeadToggle // ShowDead
        +> allSteps.assert("2.0", "2.0.1", "2.0.X.1", "2.0.2", "2.0.X.2", "2.0.3", "2.0.4")
        +> stepText("2.0.2").assert("cat←2.0.1, 2.0.X.1→2.0.3, 2.0.X.2")

        >> filterDeadToggle // HideDead
        +> allSteps.assert("2.0", "2.0.1", "2.0.2", "2.0.3", "2.0.4")
        +> stepText("2.0.2").assert("cat←2.0.1→2.0.3")
        >> editStepText("2.0.2", "cat <-- 2.0.1 --> 2.0.3", "dog --> .0.4")
        +> stepText("2.0.2").assert("dog→2.0.4")

        >> filterDeadToggle // ShowDead
        +> allSteps.assert("2.0", "2.0.1", "2.0.X.1", "2.0.2", "2.0.X.2", "2.0.3", "2.0.4")
        +> stepText("2.0.2").assert("dog←2.0.X.1→2.0.4, 2.0.X.2")

        >> restoreStep("2.0.X.2")
        >> restoreStep("2.0.X.1")
        +> allSteps.assert("2.0", "2.0.1", "2.0.2", "2.0.3", "2.0.4", "2.0.5", "2.0.6")
        +> stepText("2.0.3").assert("dog←2.0.2→2.0.4, 2.0.6")
      ))
    }

    'deadExplicitly - test("MF-19")(Plan invariants testLifeRowInnerText(deadCanRestore))

    'deadImplicitly - test("SI-1")(Plan invariants testLifeRowInnerText(deadNoRestore))

    'deadImplicitlyAndExplicitly - test("SI-2")(Plan invariants testLifeRowInnerText(deadNoRestore))

    'deadFields - test("UC-1")(Plan.action(
      filterDeadToggle
        .addCheck(reporterFieldExistence.beforeAndAfter)
        .times(3)
    ))

    'inapplicableFields - {
      def check(expectVisible: Boolean) =
        visibleFields.assertB(expectVisible).contains("Description")
      def t(pubid: ExternalPubid, expectVisible: Boolean) =
        test(pubid)(Plan invariants check(expectVisible))
      'mf1 - t("MF-1", true)
      'fr1 - t("FR-1", false)
    }

    'deleteRestore - test("UC-1")(Plan.action(
      changeLife.updateState(stateMode set Mode.Delete)     <+ life.assert(Live)
      >> deleteScreenDelete                                 +> life.assert(Dead)
      >> changeLife.updateState(stateMode set Mode.Restore)
      >> restoreScreenRestore                               +> life.assert(Live)
    ))

    'editors - test("UC-1")(Plan(
      doubleClickTitle                     +> editorCount.assert.beforeAndAfter(0, 1) <+ filterDead.assert(HideDead)
      >> doubleClickFieldValue("Notes")    +> editorCount.assert(2)
      >> showDead                          +> editorCount.assert(2)
      >> doubleClickFieldValue("Reporter") +> editorCount.assert(2) // dead field
      >> hideDead                          +> editorCount.assert(2)
    , reporterFieldExistence))

  }
}
