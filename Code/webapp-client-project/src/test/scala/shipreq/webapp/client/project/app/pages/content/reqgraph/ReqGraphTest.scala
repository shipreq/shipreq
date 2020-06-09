package shipreq.webapp.client.project.app.pages.content.reqgraph

import utest._
import utest.framework.TestPath
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv

object ReqGraphTest extends TestSuite {
  import ReqGraphTestDsl.{savedViews => _, _}
  import ReqGraphTestDsl.savedViews.{* => _, _}

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftReqGraphTests(p).asAction(name),
      page = Page.ReqGraph,
      project = project)
  }

  override def tests = Tests {
    "coloursWithDeadTag" - testColoursWithDeadTag()
  }

  private def testColoursWithDeadTag()(implicit tp: TestPath): Unit = {
    import SampleProject8.Values._

    val allGood =
      ( savedViews.assert("> * yo")
      & filterDead.assert(HideDead)
      & colours.assert.contains("Tag: Status")
      & colourOptions.assert("Tag: Priority", "Tag: Status", "Tag: Version", "Type"))

    val tagIsDead =
      ( savedViews.assert("> * yo")
      & filterDead.assert(ShowDead)
      & colours.assert.contains("Tag: Status")
      & colourOptions.assert("Tag: Priority", "Tag: Released", "Tag: Status", "Tag: Version", "Type"))

    runActions(SampleProject8.project)(
      receiveExternalEvent(Event.TagDelete(relTG))

        >> selectColours("Tag: Status")
        +> savedViews.assert("> Unsaved view")
        +> filterDead.assert(HideDead)
        +> colours.assert.contains("Tag: Status")
        +> colourOptions.assert("Tag: Priority", "Tag: Status", "Tag: Version", "Type")

        >> saveCurrentView("yo")
        +> allGood

        >> receiveExternalEvent(Event.TagDelete(statusTG))
        +> tagIsDead

        >> filterDeadToggleNoOp
        +> tagIsDead

        >> selectColours("Tag: Priority")
        +> savedViews.assert("* yo", "> Unsaved view")
        +> filterDead.assert(HideDead)
        +> colours.assert.contains("Tag: Priority")
        +> colourOptions.assert("Tag: Priority", "Tag: Version", "Type")

        >> filterDeadToggle
        >> selectColours("Tag: Status")
        +> tagIsDead

        >> receiveExternalEvent(Event.TagRestore(statusTG))
        >> filterDeadToggle
        +> allGood
    )
  }
}
