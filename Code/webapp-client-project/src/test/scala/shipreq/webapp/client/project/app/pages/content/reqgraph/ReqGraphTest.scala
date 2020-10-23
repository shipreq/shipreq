package shipreq.webapp.client.project.app.pages.content.reqgraph

import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.{PrepareEnv, TestWebWorkerClient}
import utest._
import utest.framework.TestPath

object ReqGraphTest extends TestSuite {
  import ReqGraphTestDsl.{savedViews => _, _}
  import ReqGraphTestDsl.savedViews.{* => _, _}

  PrepareEnv()

  private def runActions(project: Project,
                         wwPrep : TestWebWorkerClient.Prep = TestWebWorkerClient.noInitialPrep)
                        (a      : *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project, wwPrep)(Plan.action(a))

  private def runPlan(project: Project,
                      wwPrep : TestWebWorkerClient.Prep)
                     (p      : *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftReqGraphTests(p).asAction(name),
      page    = Page.ReqGraph,
      project = project,
      wwPrep  = wwPrep,
    )
  }

  private object wwPrep {
    import TestWebWorkerClient.Prep

    def forSP3: Prep = _.respondToAllGraphsWith(Svg(SampleProject3.reqGraph.showDead))
  }

  override def tests = Tests {
    "coloursWithDeadTag" - testColoursWithDeadTag()
    "edgeEditor" - {
      "newEdge" - {
        "ok" - testEdgeEditorNewEdgeOk()
      }
    }
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
      global.receiveExternalEvent(Event.TagDelete(relTG))

        >> selectColours("Tag: Status")
        +> savedViews.assert("> Unsaved view")
        +> filterDead.assert(HideDead)
        +> colours.assert.contains("Tag: Status")
        +> colourOptions.assert("Tag: Priority", "Tag: Status", "Tag: Version", "Type")

        >> saveCurrentView("yo")
        +> allGood

        >> global.receiveExternalEvent(Event.TagDelete(statusTG))
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

        >> global.receiveExternalEvent(Event.TagRestore(statusTG))
        >> filterDeadToggle
        +> allGood
    )
  }

  private def testEdgeEditorNewEdgeOk()(implicit tp: TestPath): Unit = {
    import SampleProject3._

    runActions(project, wwPrep.forSP3)(
      global.disableAutoResponse
        >> graph.dragNewEdge("MF-17" -> "MF-18")
        +> global.requestCount.assert(1)
    )
  }
}
