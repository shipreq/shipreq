package shipreq.webapp.client.project.app.pages.config.fields

import utest._
import utest.framework.TestPath
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.SampleProject.Values._
import shipreq.webapp.base.test.SampleProject6
import shipreq.webapp.base.test.TestState._
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

  override def tests = Tests {

    'fieldList - {

      'view - runActions(SampleProject6.project)(
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

    }
  }
}
