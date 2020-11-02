package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.test.SimEvent.{Keyboard => KB}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test.UnsafeTypes.autoExtPubid
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test._
import utest._

object ReqDetailTest extends TestSuite {
  import SampleDerivativeTags3.step3.{project => DT3_3}
  import SampleDerivativeTags4.{project => DT4}
  import ReqDetailTestDsl._
  import WebappTestUtil._
  import global.press

  PrepareEnv()

  private def defaultProject = SampleProject5.project

  private def runTest(ep        : ExternalPubid,
                      error     : Boolean,
                      project   : Project = defaultProject,
                      assertPass: Boolean = true)
                     (p: *.Plan): Report[String] = {
    import ProjectSpaTestDsl._

    val p2 = p.addInvariants(unsavedChangesInvariant)

    ProjectSpaTestDsl.runTestReturnReport(
      action     = liftReqDetailTests(p2).asAction(s"Req Detail (${PlainText.pubid(ep)})"),
      page       = Page.ReqDetail(ep),
      project    = project,
      rd         = State(ep, if (error) Mode.Error else Mode.Details),
      assertPass = assertPass,
    )
  }

  private def testError(ep: ExternalPubid, error: String): Unit =
    runTest(ep, true)(Plan invariants checkErrorReason(error))

  private def test(ep: ExternalPubid, project: Project = defaultProject)(test: *.Plan): Unit =
    runTest(ep, false, project)(test)

  // yeah i'm being lazy
  private def testLifeRowInnerText(expect: String) =
    *.focus("Life row").value(_.obs.generic.lifeRow.text).assert(expect)

  private val reporterFieldExistence =
    visibleFields.assert.existenceOf("Reporter")(_.obs.generic.filterDead is ShowDead)

  private val liveCanDelete  = UiText.Life.live + "." + UiText.Life.delete
  private val deadCanRestore = UiText.Life.dead + "." + UiText.Life.restore
  private val deadNoRestore  = UiText.Life.dead + "."

  // webapp-client-project/testOnly -- shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetailTest.uc.propTest
  protected def useCaseStepEditorPropTest(reps         : Int,
                                          sets         : Int,
                                          stopWhenFound: Boolean): Unit = {

    val plan = Plan.action(randomUseCaseStepAction.times(reps))

    val stepAdd        = "^Add (.+)".r
    val stepDelete     = "^Delete (.+)".r
    val stepShiftLeft  = "^ShiftLeft (.+)".r
    val stepShiftRight = "^ShiftRight (.+)".r
    val editorOpen     = "^Double-click (.+) text".r
    val editorCommit   = "^Commit (.+?) text.*".r
    val editorAbort    = "^Abort (.+?) text.*".r
    val editorSetText  = "^Set (.+?) text to ⟪?(.*?)⟫?$".r
    val stepAddTailAC  = "Add AC tail step"
    val stepAddTailEC  = "Add EC tail step"

    var failures = Vector.empty[String]

    try {

      for (i <- 1 to sets) {
        println(s"Pass $i of $sets ...")
        val report = runTest("UC-1", false, SampleProject6.project, assertPass = false)(plan)

        if (report.failed) {

          val code =
            report
              .format()
              .removeAnsiEscapeCodes
              .split('\n')
              .iterator
              .filter(_.matches("^    [^ ] .+"))
              .filterNot(_.contains("] Random "))
              .filterNot(_.contains("unsavedChanges ≤ editors"))
              .map(_.drop(6))
              .map {
                case stepAdd       (s   ) => s"""addStep("$s")"""
                case stepDelete    (s   ) => s"""delStep("$s")"""
                case stepShiftLeft (s   ) => s"""shiftStepLeft("$s")"""
                case stepShiftRight(s   ) => s"""shiftStepRight("$s")"""
                case editorOpen    (s   ) => s"""openEditor("$s")"""
                case editorCommit  (s   ) => s"""commitStepTextEdit("$s")"""
                case editorAbort   (s   ) => s"""abortStepTextEdit("$s")"""
                case editorSetText (s, v) => s"""setStepTextEditValue("$s", "$v")"""
                case `stepAddTailAC`      => addTailStepAC
                case `stepAddTailEC`      => addTailStepEC
                case other                => s"??? // $other"
              }
              .mkString(" >>\n")

          val reason = report.failureReason.fold("Reason unknown")(_.failure.takeWhile(_ != '\n'))

          val block =
            s"""// $reason
               |'bugX - test("UC-1", SampleProject6.project)(Plan.action(
               |${code.indent(2)}
               |))
               |""".stripMargin.trim.indent("      ")

          failures :+= block

          if (stopWhenFound)
            assertTestState(report)
          else
            println(s"  FAILED AFTER ${predefWrapString(code).count(_ == '\n') + 1} ACTIONS\n$block")
        }
      }

    } finally {
      if (failures.nonEmpty) {
        val sep = "=" * 80
        println(failures.mkString(sep + "\n", "\n\n", "\n\n" + sep))
      }
    }
  }

  override def tests = Tests {

    "badReqType" - testError("QL-1", "QL-1 doesn't exist")
    "badReq"     - testError("FR-9", "FR-9 doesn't exist")

    "gr" - test("FR-1")(Plan invariants testLifeRowInnerText(liveCanDelete))

    "uc" - {

      "tree" - test("UC-1")(Plan.action(allSteps.assert("1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1")
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

      "text" - test("UC-1")(Plan.action(
             stepText("1.1").assert("Have no food")
          +> stepText("1.0.2").assert("Put in mouth←1.1.1")
          +> stepText("1.1.1").assert("Steal food→1.0.2")

          +> editStepText("1.1", "No food? --> 1.0.2 <-- 1.1.1 ")
          +> stepText("1.1").assert("No food?←1.1.1→1.0.2")
          +> stepText("1.0.2").assert("Put in mouth←1.1, 1.1.1")
          +> stepText("1.1.1").assert("Steal food→1.0.2, 1.1")

          >> editStepText("1.1", "No food? <--102-->1.1.1")
          +> stepText("1.1").assert("No food?←1.0.2→1.1.1")
          +> stepText("1.0.2").assert("Put in mouth←1.1.1→1.1")
          +> stepText("1.1.1").assert("Steal food←1.1→1.0.2")

          >> editStepText("1.1.1", "-->3 1.1")
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

      "flowComma" - test("UC-1")(Plan.action(
        editStepText("1.1", "omg --> 1.0.2,1.0.1")
          +> stepText("1.1").assert("omg→1.0.1, 1.0.2")
      ))

      "dead" - test("UC-1")(Plan.action(
        clickDeleteOrRestore.updateState(stateMode set Mode.Delete) >> deleteScreenDelete
          +> life.assert(Dead)
          +> tailStepAC.test("doesn't exist")(_.isEmpty)
          +> tailStepEC.test("doesn't exist")(_.isEmpty)
      ))

      "deadFlow" - test("UC-2", SampleProject6.project)(Plan.action(
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

      // webapp-client-project/testOnly -- shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetailTest.uc.propTest
//      'propTest - useCaseStepEditorPropTest(
//                    reps          = 100,
//                    sets          = 3,
//                    stopWhenFound = true)

      "bugs" - {

        // Turned out the problem was in UnsavedChanges.derive
        "useCaseStepIdNotFound" - test("UC-1", SampleProject6.project)(Plan.action(
                                      // 1.0  1.0.1  1.0.2  1.0.2.a  1.0.3                                                          1.1  1.1.1  1.E.1
          addStep("1.0")           >> // 1.0  1.0.1  1.0.2           1.0.3    1.0.3.a  1.0.4                                        1.1  1.1.1  1.E.1
          shiftStepLeft("1.0.3.a") >> // 1.0  1.0.1  1.0.2           1.0.3             1.0.4                        1.0.5           1.1  1.1.1  1.E.1
          addStep("1.0")           >> // 1.0  1.0.1  1.0.2           1.0.3             1.0.4                        1.0.5  1.0.6    1.1  1.1.1  1.E.1
          openEditor("1.0.2")      >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4                        1.0.5  1.0.6    1.1  1.1.1  1.E.1
          shiftStepRight("1.0.6")  >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4                        1.0.5  1.0.5.a  1.1  1.1.1  1.E.1
          shiftStepRight("1.0.5")  >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4    1.0.4.a  1.0.4.a.i                  1.1  1.1.1  1.E.1
          delStep("1.0.2")            // Delete 1.0.2 -- java.util.NoSuchElementException: key not found: UseCaseStepId(201)
        ))


        "extraUnsavedChanges" - test("UC-1", SampleProject6.project)(Plan.action(
                                                                 // 1.0  1.0.1  1.0.2  1.0.2.a  1.0.3  1.1  1.1.1  1.E.1
          shiftStepRight("1.1")   +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.2  1.0.2.a  1.0.3  1.0.4  1.0.4.a  1.E.1
          delStep("1.0.2")        +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.2  1.0.3  1.0.3.a  1.E.1
          delStep("1.0.3")        +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.2  1.E.1
          shiftStepRight("1.0.2") +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.1.a  1.E.1
          openEditor("1.0.1.a")   +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.1.a* 1.E.1
          openEditor("1.E.1")     +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.1.a* 1.E.1*
          delStep("1.E.1")        +> unsavedChanges.assert(0) >> // 1.0  1.0.1  1.0.1.a*
          delStep("1.0.1")        +> unsavedChanges.assert(0)    // unsavedChanges (1) must be ≤ editorCount (0)
        ))

      }
    }

    "deadExplicitly" - test("MF-19")(Plan invariants testLifeRowInnerText(deadCanRestore))

    "deadImplicitly" - test("SI-1")(Plan invariants testLifeRowInnerText(deadNoRestore))

    "deadImplicitlyAndExplicitly" - test("SI-2")(Plan invariants testLifeRowInnerText(deadNoRestore))

    "deadFields" - test("UC-1")(Plan.action(
      filterDeadToggle
        .addCheck(reporterFieldExistence.beforeAndAfter)
        .times(3)
    ))

    "inapplicableFields" - {
      def check(expectVisible: Boolean) =
        visibleFields.assertB(expectVisible).contains("Description")
      def t(pubid: ExternalPubid, expectVisible: Boolean) =
        test(pubid)(Plan invariants check(expectVisible))
      "mf1" - t("MF-1", true)
      "fr1" - t("FR-1", false)
    }

    "deleteRestore" - test("UC-1")(Plan.action(
      deleteReq >> restoreReq
    ))

    "editors" - test("UC-1")(Plan(
      title.doubleClick                +> editorCount.assert.beforeAndAfter(0, 1) <+ filterDead.assert(HideDead)
      >> field("Notes").doubleClick    +> editorCount.assert(2)
      >> showDead                      +> editorCount.assert(2)
      >> field("Reporter").doubleClick +> editorCount.assert(2) // dead field
      >> hideDead                      +> editorCount.assert(2)
    , reporterFieldExistence))

    "unsavedDeadChanges" - test("UC-1")(Plan.action(
      filterDead.assert(HideDead)
        +> editorCount.assert(0)
        +> unsavedChanges.assert(0)

        +> title.doubleClick
        +> editorCount.assert.increaseBy(1)
        +> unsavedChanges.assert.noChange

        >> field("Notes").doubleClick
        +> editorCount.assert.increaseBy(1)
        +> unsavedChanges.assert.noChange

        >> title.setEditorValue("xxxxxxxxxxx")
        +> editorCount.assert.noChange
        +> unsavedChanges.assert.increaseBy(1)

        >> field("Notes").setEditorValue("zzzzzzzzzzzzzzzzz")
        +> editorCount.assert.noChange
        +> unsavedChanges.assert.increaseBy(1)

        >> clickDeleteOrRestore.updateState(stateMode set Mode.Delete)
        >> deleteScreenDelete
        +> life.assert(Dead)
        +> editorCount.assert(0)
        +> unsavedChanges.assert(0)

        >> clickDeleteOrRestore.updateState(stateMode set Mode.Restore)
        >> restoreScreenRestore
        +> life.assert(Live)
        +> editorCount.assert(2)
        +> unsavedChanges.assert(2)
    ))

    "fieldRules" - {
      // +--------------------------------------+---------------+
      // | TEST                                 | IN            |
      // +--------------------------------------+---------------+
      // | perReq > otherwise                   | FR1, BR1, CO1 |
      // | na                                   | FR1, BR1, CO1 |
      // | opt          - no content            | FR1, BR1, CO1 |
      // | opt          - w/ content            | FR1           |
      // | man          - no content            | FR1, BR1      |
      // | man          - w/ content            | FR1           |
      // | def:tag:ok   - no content            | BR1           |
      // | def:tag:ok   - w/ content            | BR1           |
      // | def:tag:ok   - editor                | BR1           |
      // | def:tag:dead - no content - HideDead | FR1, BR1, CO1 |
      // | def:tag:dead - no content - ShowDead | FR1, BR1, CO1 |
      // | def:tag:dead - w/ content - HideDead | BR1           |
      // | def:tag:dead - w/ content - ShowDead | BR1           |
      // | def:tag:dead - editor     - HideDead | BR1           |
      // | def:tag:dead - editor     - ShowDead | BR1           |
      // | def:tag:bad  - no content - HideDead | FR1, BR1      |
      // | def:tag:bad  - no content - ShowDead | FR1, BR1      |
      // | def:tag:bad  - w/ content - HideDead | CO1           |
      // | def:tag:bad  - w/ content - ShowDead | CO1           |
      // | def:tag:bad  - editor     - HideDead | BR1           |
      // | def:tag:bad  - editor     - ShowDead | BR1           |
      // | dead fields                          | SI1           |
      // +--------------------------------------+---------------+

      // FR
      //   - bizJust      : pr : opt
      //   - alternatives : ow : na
      //   - component    : pr : opt
      //   - priority     : pr : man
      //   - released     : ow : man
      //   - status       : pr : def:deadTag
      "fr1" - test("FR-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Business Justification",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Business Justification").text.assert("") // perReq > otherwise, opt - no content
          +> field("Component").text.assert("") // opt - no content
          +> field("Released").text.assert("blank") // man - no content
          +> field("Priority").text.assert("blank") // man - no content
          +> field("Status").text.assert("") // def:tag:dead - no content - HideDead
          +> field("Version").text.assert("") // def:tag:bad - no content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Business Justification",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Business Justification").text.assert("") // perReq > otherwise, opt - no content
          +> field("Component").text.assert("") // opt - no content
          +> field("Priority").text.assert("blank") // man - no content
          +> field("Released").text.assert("blank") // man - no content
          +> field("Status").text.assert("uat2") // def:tag:dead - no content - ShowDead
          +> field("Version").text.assert("") // def:tag:bad - no content - ShowDead

          >> field("Component").changeToAndBack("" -> "X", "" -> "X") // opt - w/ content
          >> field("Priority").changeToAndBack("" -> "pri=low", "blank" -> "pri=low") // man - w/ content
      ))

      // BR
      //   - ow : man
      //   - ow : na
      //   - ow : na
      //   - pr : def
      //   - ow : opt
      //   - pr : def:dead -> ow:opt
      "br1" - test("BR-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Business Justification",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Business Justification").text.assert("blank") // man - no content
          +> field("Priority").text.assert("pri=med") // def:tag:ok - no content
          +> field("Released").text.assert("blank") // man - no content
          +> field("Status").text.assert("") // perReq > otherwise, def:tag:dead - no content - HideDead
          +> field("Version").text.assert("") // def:tag:bad - no content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Implications",
          "Major Feature",
          "Priority",
          "Reporter",
          "Released",
          "Status",
          "Business Justification",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Business Justification").text.assert("blank") // man - no content
          +> field("Priority").text.assert("pri=med") // def:tag:ok - no content
          +> field("Released").text.assert("blank") // man - no content
          +> field("Status").text.assert("uat") // perReq > otherwise, def:tag:dead - no content - ShowDead
          +> field("Version").text.assert("") // def:tag:bad - no content - ShowDead

          >> field("Priority").changeToAndBack("" -> "pri=low", "pri=med" -> "pri=low") // def:tag:ok - w/ content
          >> field("Status").changeToAndBack("" -> "wip", "uat" -> "wip") // def:tag:dead - w/ content - ShowDead
          >> field("Version").changeToAndBack("" -> "v1.0", "" -> "v1.0") // def:tag:bad - w/ content - ShowDead

          >> filterDeadToggle
          +> filterDead.assert(HideDead)
          >> field("Status").changeToAndBack("" -> "wip", "" -> "wip") // def:tag:dead - w/ content - HideDead
          >> field("Version").changeToAndBack("" -> "v1.0", "" -> "v1.0") // def:tag:bad - w/ content - HideDead
      ))

      // CO
      //   - bizJust      : pr : na
      //   - alternatives : ow : na
      //   - component    : pr : opt
      //   - priority     : pr : na
      //   - released     : pr : def:badTag
      //   - status       : pr : def:deadTag
      "co1" - test("CO-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Component").text.assert("") // perReq > otherwise, opt - no content
          +> field("Released").text.assert("v1.0") // def:tag:bad - w/ content - ShowDead
          +> field("Status").text.assert("uat") // def:tag:dead - no content - ShowDead
          +> tagFieldDesc("Version").assert("v1.0 v3.x-") // def:tag:bad - w/ content - ShowDead

          >> restoreReq
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Component").text.assert("") // opt - no content
          +> field("Released").text.assert("v1.0") // def:tag:bad - w/ content - HideDead
          +> field("Status").text.assert("") // def:tag:dead - no content - HideDead
          +> field("Version").text.assert("v1.0") // def:tag:bad - w/ content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Component").text.assert("") // perReq > otherwise, opt - no content
          +> field("Released").text.assert("v1.0") // def:tag:bad - w/ content - ShowDead
          +> tagFieldDesc("Status").assert("uat?-") // def:tag:dead - no content - ShowDead
          +> tagFieldDesc("Version").assert("v1.0 v3.x-") // def:tag:bad - w/ content - ShowDead
      ))

      "si1" - test("SI-1", SampleProject7.project)(Plan.action(
        *.emptyAction
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Implications",
          "Description",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Alternatives",
          "Component",
          "Version",
          StaticField.OtherTags.name,
          StaticField.AllTags.name)
          +> field("Component").text.assert("")
          +> field("Priority").text.assert("")
          +> field("Released").text.assert("") // dead req with mandatory blank
          +> field("Status").text.assert("uat3") // dead req with dead default
          +> field("Version").text.assert("") // dead req with bad live default
      ))
    }

    "staticTagFields" - {
      import SampleProject7.Values._
      import UnsafeTypes._
      import StaticField._

      val project =
        applyEventsSuccessfully(
          SampleProject7.project,
          Event.ReqTagsPatch(brs(2), nesd()(priLow, priHigh)), // live & dead assigned to live field
          Event.ReqTagsPatch(brs(2), nesd()(wip, defer)), // live & dead assigned to dead field
          Event.TagDelete(defer),
          Event.TagDelete(priHigh),
          Event.FieldCustomDelete(statusField),
        )

      // Note 1: BR-2 already has
      //           - N/A tag (#prod)
      //           - live & dead assigned to no field (#misc1 #misc2)
      // Note 2: N/A tags are shown when ShowDead
      test("BR-2", project)(Plan.action(
        *.emptyAction
          +> filterDead.assert(HideDead)
          +> visibleFields.assert.not.contains("Status")
          +> field("Priority").text.assert("pri=low")
          +> tagFieldDesc(OtherTags.name).assert("misc1 wip")
          +> tagFieldDesc(AllTags.name).assert("misc1 pri=low wip")

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> tagFieldDesc("Priority").assert("pri=high- pri=low")
          +> tagFieldDesc("Status").assert("wip defer- prod")
          +> tagFieldDesc(OtherTags.name).assert("misc1 misc2-")
          +> tagFieldDesc(AllTags.name).assert("defer- misc1 misc2- pri=high- pri=low prod wip")
      ))
    }

    // Test that preview state is forgotten when the editor closes.
    // This is subtly important because once a user makes a manual change (ie. show/hide/down/right), there's no way to
    // restore the state to auto. Closing the editor gives users a way. Without this, if a user manually closed a
    // preview in ReqDetail then when to ReqTable and opened up the same editor again, they would never get the nice
    // show-minimally behaviour that is the default in ReqTable where space is limited.
    "previewReset" - {
      val f = field("Description")

      def testPreviewReset(closeEditor: *.Actions) = test("MF-1")(Plan.action(
        f.doubleClick
        >> f.setEditorValue("pwoeir") +> f.previewButtonsStr.assert("-hd-")
        >> f.clickPreviewDown         +> f.previewButtonsStr.assert("-h-r")
        >> closeEditor                +> f.previewButtonsStr.assert("----")
        >> f.doubleClick              +> f.previewButtonsStr.assert("-hd-") // this is the point
        >> f.setEditorValue("qwe")    +> f.previewButtonsStr.assert("-hd-")
        >> f.clickPreviewHide         +> f.previewButtonsStr.assert("s---")
        >> closeEditor                +> f.previewButtonsStr.assert("----")
        >> f.doubleClick              +> f.previewButtonsStr.assert("-hd-") // this is the point
      ))

      "onCommit" - testPreviewReset(f.commit)
      "onAbort" - testPreviewReset(f.abort)
    }

    "fullscreenEditor" - {
      val f = field("Description")

      def assert(editors: Int, preview: String, isFS: Boolean, canFS: Boolean, spin: Boolean) = {
        val p = if (preview == "none") "----" else preview
        (
          editorCount.assert(editors)
          & f.hasPreview.assert(p.startsWith("-h"))
          & f.previewButtonsStr.assert(p)
          & f.isFullscreen.assert(isFS)
          & global.isBrowserFullscreen.assert(isFS)
          & f.hasEnabledFullscreenButton.assert(canFS)
          & f.isSpinning.assert(spin)
        )
      }

      "saveChange" - test("MF-1")(Plan.action(
        global.disableAutoResponse  +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
        >> f.doubleClick            +> assert(editors = 1, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> f.clickPreviewHide       +> assert(editors = 1, preview = "s---", isFS = n, canFS = y, spin = n)
        >> f.toggleFullscreen       +> assert(editors = 1, preview = "s---", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewShow       +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.toggleFullscreen       +> assert(editors = 1, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> f.clickPreviewDown       +> assert(editors = 1, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> f.clickPreviewHide       +> assert(editors = 1, preview = "s---", isFS = n, canFS = y, spin = n)
        >> f.clickPreviewShow       +> assert(editors = 1, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> f.toggleFullscreen       +> assert(editors = 1, preview = "-h-r", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewHide       +> assert(editors = 1, preview = "s---", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewShow       +> assert(editors = 1, preview = "-h-r", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewRight      +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewHide       +> assert(editors = 1, preview = "s---", isFS = y, canFS = y, spin = n)
        >> f.clickPreviewShow       +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.setEditorValue("zxc")  +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.commit                 +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = y)
        >> global.autoRespondToLast +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
      ))

      "saveNoOp" - test("MF-1")(Plan.action(
        global.disableAutoResponse +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
        >> f.doubleClick           +> assert(editors = 1, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> f.toggleFullscreen      +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.commit                +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
      ))

      "cancel" - test("MF-1")(Plan.action(
        global.disableAutoResponse +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
        >> f.doubleClick           +> assert(editors = 1, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> f.toggleFullscreen      +> assert(editors = 1, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> f.abort                 +> assert(editors = 0, preview = "none", isFS = n, canFS = n, spin = n)
      ))
    }

    "titleFocus" - test("MF-1")(Plan.action(
      title.focusCell     +> titleCellHasFocus +> title.editorValue.assert.empty
        >> press(KB.d)    +> title.editorValue.assert(Some("d"))
        >> title.abort    +> titleCellHasFocus
        >> press(KB.Down) +> assertTableCellFocused(0)
        >> press(KB.Down) +> assertTableCellFocused(1)
        >> press(KB.Up)   +> assertTableCellFocused(0)
        >> press(KB.Up)   +> titleCellHasFocus
        >> press(KB.Up)   +> assertTableCellFocused(-1)
        >> press(KB.Up)   +> assertTableCellFocused(-2)
        >> press(KB.Down) +> assertTableCellFocused(-1)
        >> press(KB.Down) +> titleCellHasFocus
    ))

    "keysInEditor" - {
      val f = field("Notes")
      val editorHasFocus = global.assertEditorHasFocus(f)
      test("UC-1")(Plan.action(
        f.focusCell
          >> press(KB.d)        +> editorHasFocus +> f.editorValue.assert(Some("d"))
          >> press(KB.Left)     +> editorHasFocus
          >> press(KB.Right)    +> editorHasFocus
          >> press(KB.Up)       +> editorHasFocus
          >> press(KB.Down)     +> editorHasFocus
          >> press(KB.Home)     +> editorHasFocus
          >> press(KB.End)      +> editorHasFocus
          >> press(KB.PageDown) +> editorHasFocus
          >> press(KB.PageUp)   +> editorHasFocus
      ))
    }

    "tabInEditor" - {
      val f = field("Notes")
      val cellHasFocus = global.assertCellHasFocus(f)
      val editorHasFocus = global.assertEditorHasFocus(f)
      test("UC-1")(Plan.action(
        f.focusCell              +> cellHasFocus
          >> press(KB.d)         +> editorHasFocus +> f.editorValue.assert(Some("d"))
          >> press(KB.Tab.shift) +> cellHasFocus
          >> press(KB.Tab)       +> editorHasFocus
          >> press(KB.Tab)       +> cellHasFocus
          >> press(KB.Tab.shift) +> editorHasFocus
      ))
    }

    "refsWithTitle" - test("MF-1")(Plan.action(
      title.set("[mf2] [mf2:]")
      +> title.text.assert("[MF-2] [MF-2: Anonymous Share]")
      >> title.doubleClick
      +> title.editorValue.assert.contains("[MF-2] [MF-2:]")
    ))

    "derivativeTags" - {

      "dt3-3" - {
        "mf1" - test("MF-1", project = DT3_3)(Plan.action(
          *.emptyAction +> tagFieldDescs(
            "Status"   -> "readyForDev+",
            "Version"  -> "v1+ v2",
            "All Tags" -> "readyForDev+ v1+ v2")
        ))

        "fr1" - test("FR-1", project = DT3_3)(Plan.action(
          *.emptyAction +> tagFieldDescs(
            "Status"   -> "readyForDev?",
            "Version"  -> "v1",
            "All Tags" -> "readyForDev? v1")
        ))
      }

      "dt4" - {
        "d3" - test("D-3", project = DT4)(Plan.action(
          *.emptyAction +> tagFieldDescs(
            "W"        -> "w1?",
            "Y"        -> "y1+ y2+",
            "All Tags" -> "w1? y1+ y2+")
          >> filterDeadToggle +> tagFieldDescs(
            "W"        -> "w1?",
            "Y"        -> "y1+ y2+ y5?-",
            "X"        -> "x1?-",
            "All Tags" -> "w1? x1?- y1+ y2+ y5?-")
        ))

        "other" - {
          import SampleDerivativeTags4.Values._
          val p = applyEventsSuccessfully(DT4,
            Event.TagRestore(z4),
            TestEvent.applicableTagUpdate(z4, parents = Vector.empty),
            Event.TagDelete(z4),
          )
          test("A-12", p)(Plan.action(
            *.emptyAction +> tagFieldDescs(
              "W"          -> "w1?",
              "Other Tags" -> "",
              "All Tags"   -> "w1? z1?")
          >> filterDeadToggle +> tagFieldDescs(
              "W"          -> "w1?",
              "Other Tags" -> "z4-",
              "All Tags"   -> "w1? x1?- z1? z4-")
          ))
        }
      }

    }

    "textSurrounds" - {
      val f = field("Notes")

      def subtest(prefix: String, mid: String, suffix: String) = {
        val a = prefix.length
        val b = a + mid.length

        (
          f.setEditorValue(s"$prefix$mid$suffix")
            >> f.focusEditor

            // new wrap
            >> f.setEditorSelectionRange(a, b)
            >> press(KB.BracketLeft)
            +> f.editorValue.assert(Some(s"$prefix[$mid]$suffix"))

            // unwrap cos of inside
            >> f.setEditorSelectionRange(a + 1, b + 1)
            >> press(KB.BracketLeft)
            +> f.editorValue.assert(Some(s"$prefix$mid$suffix"))

            // new wrap
            >> f.setEditorSelectionRange(a, b)
            >> press(KB.Slash)
            +> f.editorValue.assert(Some(s"$prefix//$mid//$suffix"))

            // unwrap cos of outside
            >> f.setEditorSelectionRange(a + 2, b +2)
            >> press(KB.Slash)
            +> f.editorValue.assert(Some(s"$prefix$mid$suffix"))
        ).group(s"Test with '$prefix|$mid|$suffix'")
      }

      test("UC-1")(Plan.action(
        f.doubleClick +> f.editing.assert(true)
          >> subtest("abc", "wow", "omg")
          >> subtest("", "x", "")
      ))
    }

  }
}
