package shipreq.webapp.client.project.app.pages.config.issues

import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv
import utest._
import utest.framework.TestPath

object IssueConfigTest extends TestSuite {
  import IssueConfigTestDsl._
  import SampleProject7.Values._
//  import Buttons.{displayFailure => displayButtonFailure}

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftIssueConfigPageTests(p).asAction(name),
      page = Page.CfgIssues,
      project = project)
  }

  private def t0d0 = "TO"+"DO"
  private def _t0d0 = "#TO"+"DO"

  private val setNewKey = (
       setKey("PENDING") +> editorKeyError.assert("Already in use.")
    >> setKey("pending") +> editorKeyError.assert("Already in use.")
    >> setKey(t0d0)      +> editorKeyError.assert("Already in use.")
    >> setKey("POOP")    +> editorKeyError.assert.empty
  )

  private def testView()(implicit tp: TestPath) = {
    val p = applyEventSuccessfully(
      SampleProject7.project,
      Event.FieldCustomTagUpdate(relField, CustomTagFieldGD(FieldReqTypeRules.mandatory))
    )

    val assertOtherSources =
      otherSources.assert(
        """
          |Exclusive Tag Groups
          |  * Priority
          |
          |Mandatory Fields
          |  * Business Justification(CO and FR excepted)
          |  * Priority(FR and MF only)
          |  * Released
          |  * Title(built-in)
          |  * Use Case Steps(built-in)
          |
          |Req Types with Mandatory Implication
          |  * FR: Functional Requirement
          |
          |""".stripMargin.trim)

    runActions(p)(

      *.emptyAction
        +> filterDead.assert(HideDead)
        +> keyList.assert("#TBD", _t0d0)
        +> isEditorOpen.assert(false)
        +> assertOtherSources

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#TBD", _t0d0)
        +> isEditorOpen.assert(false)
        +> assertOtherSources
    )
  }

  private def testNew()(implicit tp: TestPath) =
    runActions(SampleProject.project)(

      clickNew
        +> isEditorOpen.assert(true)
        +> editorTitle.assert("New issue type")
        +> editorKey.assert("")
        +> editorDesc.assert("")
        +> editorKeyError.assert("Cannot be blank.")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setNewKey
        +> keyList.assert("#TBD", _t0d0)
        +> editorTitle.assert("New issue type")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> keyList.assert("#POOP", "#TBD", _t0d0)
        +> editorTitle.assert("#POOP")
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickCloseButton
        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#POOP", "#TBD", _t0d0)
        +> isEditorOpen.assert(false)
    )

  private def testUpdate()(implicit tp: TestPath) =
    runActions(SampleProject.project)(
      selectIssue("#TBD")
        +> editorTitle.assert("#TBD")
        +> editorKeyError.assert.empty
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> setNewKey
        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> keyList.assert("#POOP", _t0d0)
        +> editorTitle.assert("#POOP")
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickCloseButton
        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#POOP", _t0d0)
        +> isEditorOpen.assert(false)
    )

  private def testDeleteRestore()(implicit tp: TestPath) =
    runActions(SampleProject.project)(
      selectIssue("#TBD")

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#TBD", _t0d0)
        +> editorTitle.assert("#TBD")

        >> clickCloseButton
        +> filterDead.assert(HideDead)
        +> keyList.assert(_t0d0)
        +> isEditorOpen.assert(false)

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#TBD", _t0d0)
        +> isEditorOpen.assert(false)

        >> selectIssue("#TBD")
        +> filterDead.assert(ShowDead)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))

        >> clickRestoreButton
        +> filterDead.assert(ShowDead)
        +> keyList.assert("#PENDING", "#TBD", _t0d0)
        +> isEditorOpen.assert(true)
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(HideDead)
        +> keyList.assert("#TBD", _t0d0)
    )

  override def tests = Tests {
    "view"          - testView()
    "new"           - testNew()
    "update"        - testUpdate()
    "deleteRestore" - testDeleteRestore()
  }
}
