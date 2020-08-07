package shipreq.webapp.client.project.app.pages.config.reqtypes

import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.data._
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

object ReqTypeConfigTest extends TestSuite {
  import ReqTypeConfigTestDsl._
//  import Buttons.{displayFailure => displayButtonFailure}

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftReqTypeConfigTests(p).asAction(name),
      page = Page.CfgReqTypes,
      project = project)
  }

  private val setMnemonicAndName = (
       setMnemonic("UC") +> editorMnemonicError.assert("Already in use.")
    >> setMnemonic("MF") +> editorMnemonicError.assert("Already in use.")
    >> setMnemonic("DD") +> editorMnemonicError.assert("Already in use.")
    >> setMnemonic("DA") +> editorMnemonicError.assert("Already in use.")
    >> setMnemonic("XX") +> editorMnemonicError.assert.empty

    >> setName("Use Case")        +> editorNameError.assert("Already in use.")
    >> setName("MAJOR FEATURE")   +> editorNameError.assert("Already in use.")
    >> setName("Data Definition") +> editorNameError.assert("Already in use.")
    >> setName("blah")            +> editorNameError.assert.empty
  )

  private def testView()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      *.emptyAction
        +> filterDead.assert(HideDead)
        +> mnemonicList.assert("BR", "CO", "FR", "MF", "UC")
        +> isEditorOpen.assert(false)

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "FR", "MF", "SI", "UC")
        +> isEditorOpen.assert(false)
    )

  private def testNew()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      clickNew
        +> isEditorOpen.assert(true)
        +> editorTitle.assert("New req type")
        +> editorMnemonic.assert("")
        +> editorName.assert("")
        +> editorDesc.assert("")
        +> editorMnemonicError.assert("Cannot be blank.")
        +> editorNameError.assert("Cannot be blank.")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setMnemonicAndName
        +> mnemonicList.assert("BR", "CO", "FR", "MF", "UC")
        +> editorTitle.assert("New req type")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> mnemonicList.assert("BR", "CO", "FR", "MF", "UC", "XX")
        +> editorTitle.assert("XX: blah")
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, close = Enabled, save = Disabled))
    )

  private def testNotInUse()(implicit tp: TestPath) =
    runActions(SampleProject.project)(
      selectReqType("FR")
        +> editorTitle.assert("FR: Functional Requirement")
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, close = Enabled, save = Disabled))

        >> setMnemonicAndName
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> mnemonicList.assert("BR", "CO", "MF", "UC", "XX")
        +> editorTitle.assert("XX: blah")
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC", "XX")
        +> pastMnemonics.assert("")

        >> clickSoftDeleteButton
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC", "XX")
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))

        >> clickRestoreButton
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, close = Enabled, save = Disabled))

        +> confirms.assert(0)
        >> clickHardDeleteButton
        +> confirms.assert(1)
        +> isEditorOpen.assert(false)
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC")
    )

  private def testInUse()(implicit tp: TestPath) =
    runActions(SampleProject3.project)(
      selectReqType("FR")
        +> editorTitle.assert("FR: Functional Requirement")
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> setMnemonicAndName
        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> mnemonicList.assert("BR", "CO", "MF", "UC", "XX")
        +> editorTitle.assert("XX: blah")
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC", "XX, FR")
        +> pastMnemonics.assert("FR")

        >> clickSoftDeleteButton
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC", "XX, FR")
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))

        >> clickRestoreButton
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "MF", "SI", "UC", "XX, FR")

        >> clickFilterDead
        +> filterDead.assert(HideDead)
        +> mnemonicList.assert("BR", "CO", "MF", "UC", "XX")
        +> pastMnemonics.assert.empty

        >> setMnemonic("FR")
        >> clickSaveButton
        +> mnemonicList.assert("BR", "CO", "FR", "MF", "UC")

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> mnemonicList.assert("BR", "CO", "DD, DA, DDF", "FR, XX", "MF", "SI", "UC")
        +> pastMnemonics.assert("XX")
    )

  private def testRejectHardDeletion()(implicit tp: TestPath) =
    runActions(SampleProject.project)(
      selectReqType("FR")

        >> setConfirmResponse(false)

        +> confirms.assert(0)
        >> clickHardDeleteButton
        +> confirms.assert(1)

        +> isEditorOpen.assert(true)
        +> mnemonicList.assert("BR", "CO", "FR", "MF", "UC")
        +> editorTitle.assert("FR: Functional Requirement")
        +> buttonsEnabled.assert(Buttons(hardDel = Enabled, delete = Enabled, close = Enabled, save = Disabled))
    )

  override def tests = Tests {
    "view"      - testView()
    "new"       - testNew()
    "notInUse"  - testNotInUse()
    "inUse"     - testInUse()
    "rejectDel" - testRejectHardDeletion()
  }
}
