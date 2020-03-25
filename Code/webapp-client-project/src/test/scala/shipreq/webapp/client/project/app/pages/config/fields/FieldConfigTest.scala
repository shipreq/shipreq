package shipreq.webapp.client.project.app.pages.config.fields

import utest._
import utest.framework.TestPath
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.SampleProject7.Values._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv

object FieldConfigTest extends TestSuite {
  import FieldConfigTestDsl._
  import Buttons.{displayFailure => displayButtonFailure}

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftFieldConfigPageTests(p).asAction(name),
      page = Page.CfgFields,
      project = project)
  }

//  @inline private implicit def autoSomeEnabled(e: Enabled): Option[Enabled] = Some(e)
//  @inline private implicit def autoSomeString(s: String): Option[String] = Some(s)

  import StaticField.{
    ImplicationGraph => IG,
    NormalAltStepTree => NCAC,
    ExceptionStepTree => EC,
    StepGraph => SG,
  }

  private def testFieldListView()(implicit tp: TestPath) =
    runActions(SampleProject6.project)(
      *.emptyAction
        +> filterDead.assert(HideDead)
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        StaticField.StepGraph.name,
        "Status",
        "Notes")

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        "Reporter", // dead
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        StaticField.StepGraph.name,
        "Released", // dead
        "Status",
        "Notes")
    )

  private def testTextFieldEdit()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Description")
        +> filterDead.assert(HideDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, FR", "Not applicable"))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setEditorName("")
        +> editorNameError.assert("Cannot be blank.")

        >> setEditorName("Component")
        +> editorNameError.assert("Already in use.")

        >> addEditorRule
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI"),
        RuleRow("", "Optional", reqTypesError = "Cannot be blank."),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(1, "MF")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI", reqTypesError = "Defined elsewhere: MF"),
        RuleRow("MF", "Optional", reqTypesError = "Defined elsewhere: MF"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(1, "xxx")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI"),
        RuleRow("XXX", "Optional", reqTypesError = "XXX is not a valid req type."),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(1, "DD")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI"),
        RuleRow("DD", "Optional", reqTypesError = "DD has been deleted."),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(1, "co fr")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional", deadReqTypes = "SI"),
        RuleRow("CO FR", "Optional"),
        RuleRow.other("BR, DD", "Not applicable"))

        >> delEditorRule(0)
        +> editorRules.assert(
        RuleRow("CO FR", "Optional"),
        RuleRow.other("BR, DD, MF, SI, UC", "Not applicable"))

        >> clickFilterDead
        +> filterDead.assert(HideDead)
        +> editorRules.assert(
        RuleRow("CO FR", "Optional"),
        RuleRow.other("BR, MF, UC", "Not applicable"))
    )

  override def tests = Tests {

    'fieldList - {
      'view - testFieldListView()
    }

    'textField - {
      'edit - testTextFieldEdit()
    }

  }
}
