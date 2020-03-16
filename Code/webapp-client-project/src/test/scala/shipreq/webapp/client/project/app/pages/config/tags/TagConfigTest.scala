package shipreq.webapp.client.project.app.pages.config.tags

import utest._
import utest.framework.TestPath
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ApplicableTagGD, Event}
import shipreq.webapp.base.test.SampleProject.Values._
import shipreq.webapp.base.test.SampleProject6
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv

object TagConfigTest extends TestSuite {
  import TagConfigTestDsl._
  import Buttons.{displayFailure => displayButtonFailure}

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftTagConfigPageTests(p).asAction(name),
      page = Page.CfgTags,
      project = project)
  }

  @inline private implicit def autoSomeEnabled(e: Enabled): Option[Enabled] = Some(e)
  @inline private implicit def autoSomeString(s: String): Option[String] = Some(s)

  override def tests = Tests {

    // =================================================================================================================
    'view - runActions(SampleProject6.project)(

      *.emptyAction
      +> filterDead.assert(HideDead)
      +> tagTreeText.assert(
        s"""
           |Priority
           |- [=] pri=high
           |- [=] pri=med
           |- [=] pri=low
           |Status
           |- [=] wip
           |- [=] defer
           |- [=] prod
           |Version
           |- Released
           |- - [=] v1.0
           |- - [=] v1.1
           |- [=] v1.x
           |- [=] v2.x
           |""".stripMargin.trim)
      +> isEditorOpen.assert(false)

      >> clickFilterDead
      +> filterDead.assert(ShowDead)
      +> tagTreeText.assert(
        s"""
           |Priority
           |- [=] pri=high
           |- [=] pri=med
           |- [=] pri=low
           |Status
           |- [=] wip
           |- [=] defer
           |- uat [DEAD]
           |- uat2 [DEAD]
           |- uat3 [DEAD]
           |- [=] prod
           |Version
           |- Released
           |- - v0.9 [DEAD]
           |- - [=] v1.0
           |- - [=] v1.1
           |- [=] v1.x
           |- [=] v2.x
           |- v3.x [DEAD]
           |- v4.x [DEAD]
           |""".stripMargin.trim)
      +> isEditorOpen.assert(false)

    )

    // =================================================================================================================
    'deleteRestoreAT - runActions(SampleProject6.project)(

      selectTag("defer")
        +> filterDead.assert(HideDead)
        +> tagTreeText.assert(
          s"""
             |Priority
             |- pri=high
             |- pri=med
             |- pri=low
             |Status
             |- wip
             |- defer [SELECTED]
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> parentsText.assert("Status")
        +> childrenText.isEmpty.assert(true)

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> tagTreeText.assert(
          s"""
             |Priority
             |- pri=high
             |- pri=med
             |- pri=low
             |Status
             |- wip
             |- defer [DEAD] [SELECTED]
             |- uat [DEAD]
             |- uat2 [DEAD]
             |- uat3 [DEAD]
             |- prod
             |Version
             |- Released
             |- - v0.9 [DEAD]
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |- v3.x [DEAD]
             |- v4.x [DEAD]
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))
        +> parentsText.assert("Status")
        +> childrenText.isEmpty.assert(true)

        >> clickRestoreButton
        +> filterDead.assert(HideDead)
        +> tagTreeText.assert(
          s"""
             |Priority
             |- pri=high
             |- pri=med
             |- pri=low
             |Status
             |- wip
             |- defer [SELECTED]
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> parentsText.assert("Status")
        +> childrenText.isEmpty.assert(true)

        >> clickDeleteButton +> filterDead.assert(ShowDead)
        >> clickCloseButton  +> filterDead.assert(HideDead)
        >> clickFilterDead   +> filterDead.assert(ShowDead)
        >> selectTag("defer")
        >> clickCloseButton  +> filterDead.assert(ShowDead)
    )

    // =================================================================================================================
    'deleteRestoreTG - runActions(SampleProject6.project)(

      selectTag("Priority")
        +> filterDead.assert(HideDead)
        +> tagTreeText.assert(
          s"""
             |Priority [SELECTED]
             |- pri=high
             |- pri=med
             |- pri=low
             |Status
             |- wip
             |- defer
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> parentsText.assert("")
        +> childrenText.assert(
             s"""
                |[=] pri=high
                |[=] pri=med
                |[=] pri=low
                |""".stripMargin.trim)

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> tagTreeText.assert(
          s"""
             |Priority [DEAD] [SELECTED]
             |- pri=high [DEAD]
             |- pri=med [DEAD]
             |- pri=low [DEAD]
             |Status
             |- wip
             |- defer
             |- uat [DEAD]
             |- uat2 [DEAD]
             |- uat3 [DEAD]
             |- prod
             |Version
             |- Released
             |- - v0.9 [DEAD]
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |- v3.x [DEAD]
             |- v4.x [DEAD]
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))
        +> parentsText.assert("")
        +> childrenText.assert(
             s"""
                |pri=high [DEAD]
                |pri=med [DEAD]
                |pri=low [DEAD]
                |""".stripMargin.trim)

        >> clickRestoreButton
        +> filterDead.assert(HideDead)
        +> tagTreeText.assert(
          s"""
             |Priority [SELECTED]
             |- pri=high
             |- pri=med
             |- pri=low
             |Status
             |- wip
             |- defer
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> parentsText.assert("")
        +> childrenText.assert(
             s"""
                |[=] pri=high
                |[=] pri=med
                |[=] pri=low
                |""".stripMargin.trim)

        >> clickDeleteButton +> filterDead.assert(ShowDead)
        >> clickCloseButton  +> filterDead.assert(HideDead)
        >> clickFilterDead   +> filterDead.assert(ShowDead)
        >> selectTag("Priority")
        >> clickCloseButton  +> filterDead.assert(ShowDead)
    )

    // =================================================================================================================
    'deadChildren - runActions(SampleProject6.project)(

      selectTag("Status")
        +> tagTreeText.assert(
          s"""
             |Priority
             |- pri=high
             |- pri=med
             |- pri=low
             |Status [SELECTED]
             |- wip
             |- defer
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> parentsText.assert("")
        +> childrenText.assert(
             s"""
                |[=] wip
                |[=] defer
                |[=] prod
                |""".stripMargin.trim)

        >> clickFilterDead
        +> tagTreeText.assert(
        s"""
           |Priority
           |- pri=high
           |- pri=med
           |- pri=low
           |Status [SELECTED]
           |- wip
           |- defer
           |- uat [DEAD]
           |- uat2 [DEAD]
           |- uat3 [DEAD]
           |- prod
           |Version
           |- Released
           |- - v0.9 [DEAD]
           |- - v1.0
           |- - v1.1
           |- v1.x
           |- v2.x
           |- v3.x [DEAD]
           |- v4.x [DEAD]
           |""".stripMargin.trim)
        +> isEditorOpen.assert(true)
        +> parentsText.assert("")
        +> childrenText.assert(
             s"""
                |[=] wip
                |[=] defer
                |uat [DEAD]
                |uat2 [DEAD]
                |uat3 [DEAD]
                |[=] prod
                |""".stripMargin.trim)
    )

    // =================================================================================================================
    'deadParent - runActions(SampleProject6.project)(

      selectTag("Status")

        >> addParent("Released")
        +> parentsText.assert("Released")
        +> childrenText.assert(
          s"""
             |[=] wip
             |[=] defer
             |[=] prod
             |""".stripMargin.trim)

        >> addParent("Priority")
        +> parentsText.assert("Priority\nReleased")
        +> childrenText.assert(
             s"""
                |[=] wip
                |[=] defer
                |[=] prod
                |""".stripMargin.trim)
        +> tagTreeText.assert( // unchanged cos not saved
          s"""
             |Priority
             |- pri=high
             |- pri=med
             |- pri=low
             |Status [SELECTED]
             |- wip
             |- defer
             |- prod
             |Version
             |- Released
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)

        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Enabled))
        >> clickSaveButton
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> tagTreeText.assert(
          s"""
             |Priority
             |- Status [SELECTED]
             |- - wip
             |- - defer
             |- - prod
             |- pri=high
             |- pri=med
             |- pri=low
             |Version
             |- Released
             |- - Status [SELECTED]
             |- - - wip
             |- - - defer
             |- - - prod
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)

        >> clickCloseButton
        >> selectTag("Released")
        >> clickDeleteButton
        >> clickCloseButton
        +> filterDead.assert(HideDead)

        >> selectTag("Status")
        +> tagTreeText.assert(
          s"""
             |Priority
             |- Status [SELECTED]
             |- - wip
             |- - defer
             |- - prod
             |- pri=high
             |- pri=med
             |- pri=low
             |Version
             |- v1.x
             |- v2.x
             |""".stripMargin.trim)
        +> parentsText.assert("Priority")
        +> childrenText.assert(
             s"""
                |[=] wip
                |[=] defer
                |[=] prod
                |""".stripMargin.trim)

        >> clickFilterDead
        +> tagTreeText.assert(
          s"""
             |Priority
             |- Status [SELECTED]
             |- - wip
             |- - defer
             |- - uat [DEAD]
             |- - uat2 [DEAD]
             |- - uat3 [DEAD]
             |- - prod
             |- pri=high
             |- pri=med
             |- pri=low
             |Version
             |- Released [DEAD]
             |- - Status [SELECTED]
             |- - - wip
             |- - - defer
             |- - - uat [DEAD]
             |- - - uat2 [DEAD]
             |- - - uat3 [DEAD]
             |- - - prod
             |- - v0.9 [DEAD]
             |- - v1.0
             |- - v1.1
             |- v1.x
             |- v2.x
             |- v3.x [DEAD]
             |- v4.x [DEAD]
             |""".stripMargin.trim)
        +> parentsText.assert("Priority\nReleased [DEAD]")
        +> childrenText.assert(
             s"""
                |[=] wip
                |[=] defer
                |uat [DEAD]
                |uat2 [DEAD]
                |uat3 [DEAD]
                |[=] prod
                |""".stripMargin.trim)
    )

    // =================================================================================================================
    'applicableReqTypes - runActions(
      applyEventsSuccessfully(SampleProject6.project,
        Event.CustomReqTypeRestore(dd),
        Event.ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(dd, mf, fr))),
        Event.ApplicableTagUpdate(priHigh, ApplicableTagGD.ValueForApplicableReqTypes(notReqTypes(dd, mf))),
        Event.CustomReqTypeDeleteSoft(dd),
      ))(

      selectTag("pri=med")
        +> filterDead.assert(HideDead)
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("FR, MF")
        +> reqTypesDead.assert.empty
        +> reqTypesError.assert.empty
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("FR, MF")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.empty
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> setApplicableReqTypesText("si mf")
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("SI MF")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.contains("SI has been deleted.")

        >> setApplicableReqTypesText("co")
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("CO")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.empty

        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Enabled))
        >> clickSaveButton
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        >> clickCloseButton

        >> selectTag("pri=high")
        +> reqTypeApplicability.assert("Blacklist")
        +> reqTypesText.assert("MF")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.empty

        >> setReqTypeApplicability("Whitelist")
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("MF")
        +> reqTypesDead.assert.empty
        +> reqTypesError.assert.empty

        >> setReqTypeApplicability("Blacklist")
        +> reqTypeApplicability.assert("Blacklist")
        +> reqTypesText.assert("MF")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.empty

        >> clickCloseButton

        >> selectTag("pri=med")
        +> reqTypeApplicability.assert("Whitelist")
        +> reqTypesText.assert("CO")
        +> reqTypesDead.assert.contains("Deleted req types: DD")
        +> reqTypesError.assert.empty
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
    )
  }
}
