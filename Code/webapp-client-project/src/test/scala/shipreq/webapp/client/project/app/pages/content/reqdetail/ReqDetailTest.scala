package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.microlibs.stdlib_ext.StdlibExt._
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes.autoExtPubid
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.UiText
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.test._

object ReqDetailTest extends TestSuite {
  import ReqDetailTestDsl._

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
    *.focus("Life row").value(_.obs.generic.lifeRow.innerText).assert(expect)

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
              .toIterator
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
            println(s"  FAILED AFTER ${wrapString(code).count(_ == '\n') + 1} ACTIONS\n$block")
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

      'flowComma - test("UC-1")(Plan.action(
        editStepText("1.1", "omg --> 1.0.2,1.0.1")
          +> stepText("1.1").assert("omg→1.0.1, 1.0.2")
      ))

      'dead - test("UC-1")(Plan.action(
        clickDeleteOrRestore.updateState(stateMode set Mode.Delete) >> deleteScreenDelete
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

      // webapp-client-project/testOnly -- shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetailTest.uc.propTest
//      'propTest - useCaseStepEditorPropTest(
//                    reps          = 100,
//                    sets          = 3,
//                    stopWhenFound = true)

      'bugs {

        // Turned out the problem was in UnsavedChanges.derive
        'useCaseStepIdNotFound - test("UC-1", SampleProject6.project)(Plan.action(
                                      // 1.0  1.0.1  1.0.2  1.0.2.a  1.0.3                                                          1.1  1.1.1  1.E.1
          addStep("1.0")           >> // 1.0  1.0.1  1.0.2           1.0.3    1.0.3.a  1.0.4                                        1.1  1.1.1  1.E.1
          shiftStepLeft("1.0.3.a") >> // 1.0  1.0.1  1.0.2           1.0.3             1.0.4                        1.0.5           1.1  1.1.1  1.E.1
          addStep("1.0")           >> // 1.0  1.0.1  1.0.2           1.0.3             1.0.4                        1.0.5  1.0.6    1.1  1.1.1  1.E.1
          openEditor("1.0.2")      >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4                        1.0.5  1.0.6    1.1  1.1.1  1.E.1
          shiftStepRight("1.0.6")  >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4                        1.0.5  1.0.5.a  1.1  1.1.1  1.E.1
          shiftStepRight("1.0.5")  >> // 1.0  1.0.1  1.0.2*          1.0.3             1.0.4    1.0.4.a  1.0.4.a.i                  1.1  1.1.1  1.E.1
          delStep("1.0.2")            // Delete 1.0.2 -- java.util.NoSuchElementException: key not found: UseCaseStepId(201)
        ))


        'extraUnsavedChanges - test("UC-1", SampleProject6.project)(Plan.action(
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
      deleteReq >> restoreReq
    ))

    'editors - test("UC-1")(Plan(
      doubleClickTitle                     +> editorCount.assert.beforeAndAfter(0, 1) <+ filterDead.assert(HideDead)
      >> doubleClickFieldValue("Notes")    +> editorCount.assert(2)
      >> showDead                          +> editorCount.assert(2)
      >> doubleClickFieldValue("Reporter") +> editorCount.assert(2) // dead field
      >> hideDead                          +> editorCount.assert(2)
    , reporterFieldExistence))

    'unsavedDeadChanges - test("UC-1")(Plan.action(
      filterDead.assert(HideDead)
        +> editorCount.assert(0)
        +> unsavedChanges.assert(0)

        +> doubleClickTitle
        +> editorCount.assert.increaseBy(1)
        +> unsavedChanges.assert.noChange

        >> doubleClickFieldValue("Notes")
        +> editorCount.assert.increaseBy(1)
        +> unsavedChanges.assert.noChange

        >> setTitleEditValue("xxxxxxxxxxx")
        +> editorCount.assert.noChange
        +> unsavedChanges.assert.increaseBy(1)

        >> setFieldEditValue("Notes", "zzzzzzzzzzzzzzzzz")
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

    'fieldRules {
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
      'fr1 - test("FR-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Business Justification",
          "Component",
          "Version")
          +> fieldText("Business Justification").assert("") // perReq > otherwise, opt - no content
          +> fieldText("Component").assert("") // opt - no content
          +> fieldText("Released").assert("blank") // man - no content
          +> fieldText("Priority").assert("blank") // man - no content
          +> fieldText("Status").assert("") // def:tag:dead - no content - HideDead
          +> fieldText("Version").assert("") // def:tag:bad - no content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Business Justification",
          "Component",
          "Version")
          +> fieldText("Business Justification").assert("") // perReq > otherwise, opt - no content
          +> fieldText("Component").assert("") // opt - no content
          +> fieldText("Priority").assert("blank") // man - no content
          +> fieldText("Released").assert("blank") // man - no content
          +> fieldText("Status").assert("uat2") // def:tag:dead - no content - ShowDead
          +> fieldText("Version").assert("") // def:tag:bad - no content - ShowDead

          >> changeFieldAndBack("Component", "" -> "X", "" -> "X") // opt - w/ content
          >> changeFieldAndBack("Priority", "" -> "pri=low", "blank" -> "pri=low") // man - w/ content
      ))

      // BR
      //   - ow : man
      //   - ow : na
      //   - ow : na
      //   - pr : def
      //   - ow : opt
      //   - pr : def:dead -> ow:opt
      'br1 - test("BR-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Priority",
          "Released",
          "Status",
          "Business Justification",
          "Version")
          +> fieldText("Business Justification").assert("blank") // man - no content
          +> fieldText("Priority").assert("pri=med") // def:tag:ok - no content
          +> fieldText("Released").assert("blank") // man - no content
          +> fieldText("Status").assert("") // perReq > otherwise, def:tag:dead - no content - HideDead
          +> fieldText("Version").assert("") // def:tag:bad - no content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Priority",
          "Reporter",
          "Released",
          "Status",
          "Business Justification",
          "Version")
          +> fieldText("Business Justification").assert("blank") // man - no content
          +> fieldText("Priority").assert("pri=med") // def:tag:ok - no content
          +> fieldText("Released").assert("blank") // man - no content
          +> fieldText("Status").assert("uat") // perReq > otherwise, def:tag:dead - no content - ShowDead
          +> fieldText("Version").assert("") // def:tag:bad - no content - ShowDead

          >> changeFieldAndBack("Priority", "" -> "pri=low", "pri=med" -> "pri=low") // def:tag:ok - w/ content
          >> changeFieldAndBack("Status", "" -> "wip", "uat" -> "wip") // def:tag:dead - w/ content - ShowDead
          >> changeFieldAndBack("Version", "" -> "v1.0", "" -> "v1.0") // def:tag:bad - w/ content - ShowDead

          >> filterDeadToggle
          +> filterDead.assert(HideDead)
          >> changeFieldAndBack("Status", "" -> "wip", "" -> "wip") // def:tag:dead - w/ content - HideDead
          >> changeFieldAndBack("Version", "" -> "v1.0", "" -> "v1.0") // def:tag:bad - w/ content - HideDead
      ))

      // CO
      //   - bizJust      : pr : na
      //   - alternatives : ow : na
      //   - component    : pr : opt
      //   - priority     : pr : na
      //   - released     : pr : def:badTag
      //   - status       : pr : def:deadTag
      'co1 - test("CO-1", SampleProject7.project)(Plan.action(

        *.emptyAction
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version")
          +> fieldText("Component").assert("") // perReq > otherwise, opt - no content
          +> fieldText("Released").assert("v1.0") // def:tag:bad - w/ content - ShowDead
          +> fieldText("Status").assert("uat") // def:tag:dead - no content - ShowDead
          +> fieldText("Version").assert("v1.0 v3.x") // def:tag:bad - w/ content - ShowDead

          >> restoreReq
          +> filterDead.assert(HideDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version")
          +> fieldText("Component").assert("") // opt - no content
          +> fieldText("Released").assert("v1.0") // def:tag:bad - w/ content - HideDead
          +> fieldText("Status").assert("") // def:tag:dead - no content - HideDead
          +> fieldText("Version").assert("v1.0") // def:tag:bad - w/ content - HideDead

          >> filterDeadToggle
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Tags",
          "Implications",
          "Major Feature",
          "Released",
          "Status",
          "Notes",
          "Component",
          "Version")
          +> fieldText("Component").assert("") // perReq > otherwise, opt - no content
          +> fieldText("Released").assert("v1.0") // def:tag:bad - w/ content - ShowDead
          +> fieldText("Status").assert("uat") // def:tag:dead - no content - ShowDead
          +> fieldText("Version").assert("v1.0 v3.x") // def:tag:bad - w/ content - ShowDead
      ))

      'si1 - test("SI-1", SampleProject7.project)(Plan.action(
        *.emptyAction
          +> filterDead.assert(ShowDead)
          +> visibleFields.assert(
          "Req Type",
          "Live Status",
          "Past IDs",
          "Deletion Reason",
          "Codes",
          "Tags",
          "Implications",
          "Description",
          "Priority",
          "Released",
          "Status",
          "Notes",
          "Alternatives",
          "Component",
          "Version")
          +> fieldText("Component").assert("")
          +> fieldText("Priority").assert("")
          +> fieldText("Released").assert("") // dead req with mandatory blank
          +> fieldText("Status").assert("uat3") // dead req with dead default
          +> fieldText("Version").assert("") // dead req with bad live default
      ))
    }

  }
}
