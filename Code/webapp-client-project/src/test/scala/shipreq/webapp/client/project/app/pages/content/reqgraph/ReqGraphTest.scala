package shipreq.webapp.client.project.app.pages.content.reqgraph

import japgolly.scalajs.react.test.SimEvent.{Keyboard => KB}
import shipreq.base.util.{Backwards, Forwards}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.protocol.websocket.UpdateContentCmd.PatchImplications
import shipreq.webapp.member.test.project.UnsafeTypes.nesd
import shipreq.webapp.member.test.project.{SampleProject3, SampleProject8}
import utest._
import utest.framework.TestPath

object ReqGraphTest extends TestSuite {
  import ImpGraphObs.DragState
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

  private def testColoursWithDeadTag()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject8.Values._

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

  private def newEdgeCmd(from: ReqId, to: ReqId) =
    PatchImplications(from, Forwards, nesd()(to))

  private def testEdgeEditorNewEdgeOk(mod: *.Actions => *.Actions = identity)(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._, Values._

    val test = mod(
      graph.dragNewEdge("MF-17" -> "MF-18")
        +> graph.dragState.assert(DragState.Valid)

        >> graph.dragEnd("MF-18")
        +> global.requestCount.assert(1)
        +> global.assertLastRequestMsg(newEdgeCmd(mfs(17), mfs(18)))
        +> graph.dragState.assert(DragState.None)
    )

    runActions(project, wwPrep.forSP3)(test)
  }

  private def testEdgeEditorNewEdgeInvalid(from: String, to: String)(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._
    runActions(project, wwPrep.forSP3)(

      graph.dragNewEdge(from -> to)
        +> graph.dragState.assert(DragState.Invalid)

        >> graph.dragEnd(to)
        +> global.requestCount.assert(0)
        +> graph.dragState.assert(DragState.None)
    )
  }

  private def testEdgeEditorNewEdgeRejection(from     : String,
                                             to       : String,
                                             dragState: DragState,
                                             mod      : *.Actions => *.Actions = identity)(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._

    val test = mod(
      graph.dragNewEdge(from -> to)
        +> graph.dragState.assert(dragState)

        >> graph.dragEnd(to)
        +> global.requestCount.assert(0)
        +> graph.dragState.assert(DragState.None)
    )

    runActions(project, wwPrep.forSP3)(test)
  }

  private def testEdgeEditorNewEdgeRefl()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._
    val id = "MF-1"
    runActions(project, wwPrep.forSP3)(
      graph.dragNewEdge(id -> id)
        +> graph.dragState.assert(DragState.Invisible)
    )
  }

  private def delEdgeCmd(from: ReqId, to: ReqId) =
    PatchImplications(from, Forwards, nesd(to)())

  private def edgeEditorDelEdgeOkTest = {
    import shipreq.webapp.member.test.project.SampleProject3._, Values._

    (graph.clickEdge("MF-1" -> "FR-2") +> graph.assertSelectedEdge("MF-1" -> "FR-2")
      >> graph.clickEdge("MF-1" -> "FR-2") +> graph.selectedEdgeId.assert(None)
      >> graph.clickEdge("MF-1" -> "FR-2") +> graph.assertSelectedEdge("MF-1" -> "FR-2")

      >> graph.clickEdge("MF-12" -> "FR-1")
      +> graph.assertSelectedEdge("MF-12" -> "FR-1")
      +> graph.dragState.assert(DragState.None)

      >> global.press(KB.Delete)
      +> global.requestCount.assert(1)
      +> global.assertLastRequestMsg(delEdgeCmd(mfs(12), frs(1)))
      )
  }

  private def testEdgeEditorDelEdgeOk()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._
    runActions(project, wwPrep.forSP3)(edgeEditorDelEdgeOkTest)
  }

  private def testEdgeEditorDelEdgeFocus()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._
    runActions(project, wwPrep.forSP3)(focusFilter >> edgeEditorDelEdgeOkTest)
  }

  private def testEdgeEditorDelEdgeNoOp(fromTo: (String, String) = null)(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._
    val test = global.press(KB.Delete) +> global.requestCount.assert(0)
    runActions(project, wwPrep.forSP3)(
      Option(fromTo) match {
        case Some(e) => graph.clickEdge(e) >> test
        case None    => test
      }
    )
  }

  private def testEdgeEditorReplaceEdgeSameSrc()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._, Values._
    runActions(project, wwPrep.forSP3)(

      graph.clickEdge("MF-22" -> "FR-2")

        >> graph.dragNewEdge("MF-22" -> "MF-27")
        +> graph.dragState.assert(DragState.Valid)
        +> graph.assertSelectedEdge("MF-22" -> "FR-2")

        >> graph.dragEnd("MF-27")
        +> global.requestCount.assert(1)
        +> global.assertLastRequestMsg(PatchImplications(mfs(22), Forwards, nesd[ReqId](frs(2))(add = mfs(27))))
        +> graph.dragState.assert(DragState.None)
        +> graph.selectedEdgeId.assert(None)
    )
  }

  private def testEdgeEditorReplaceEdgeSameTgt()(implicit tp: TestPath): Unit = {
    import shipreq.webapp.member.test.project.SampleProject3._, Values._
    runActions(project, wwPrep.forSP3)(

      graph.clickEdge("MF-22" -> "FR-2")

        >> graph.dragNewEdge("MF-15" -> "FR-2")
        +> graph.dragState.assert(DragState.Valid)
        +> graph.assertSelectedEdge("MF-22" -> "FR-2")

        >> graph.dragEnd("FR-2")
        +> global.requestCount.assert(1)
        +> global.assertLastRequestMsg(PatchImplications(frs(2), Backwards, nesd[ReqId](mfs(22))(add = mfs(15))))
        +> graph.dragState.assert(DragState.None)
        +> graph.selectedEdgeId.assert(None)
    )
  }

  override def tests = Tests {
    "coloursWithDeadTag" - testColoursWithDeadTag()
    "edgeEditor" - {
      "newEdge" - {
        "ok"      - testEdgeEditorNewEdgeOk()
        "deadSrc" - testEdgeEditorNewEdgeInvalid("MF-19", "MF-18")
        "deadTgt" - testEdgeEditorNewEdgeInvalid("MF-17", "MF-19")
        "cycle"   - testEdgeEditorNewEdgeInvalid("FR-2", "MF-1")
        "noop"    - testEdgeEditorNewEdgeRejection("MF-1", "FR-2", DragState.NoOp)
        "refl"    - testEdgeEditorNewEdgeRefl()
      }
      "delEdge" - {
        "ok"      - testEdgeEditorDelEdgeOk()
        "focus"   - testEdgeEditorDelEdgeFocus()
        "empty"   - testEdgeEditorDelEdgeNoOp()
        "deadSrc" - testEdgeEditorDelEdgeNoOp("MF-19" -> "FR-1")
        "deadTgt" - testEdgeEditorDelEdgeNoOp("FR-1" -> "CO-2")
      }
      "replaceEdge" - {
        "sameSrc"     - testEdgeEditorReplaceEdgeSameSrc()
        "sameTgt"     - testEdgeEditorReplaceEdgeSameTgt()
        "sameDeadSrc" - testEdgeEditorNewEdgeRejection("MF-19", "FR-2", DragState.Invalid, graph.clickEdge("MF-19" -> "FR-1") >> _)
        "sameDeadTgt" - testEdgeEditorNewEdgeRejection("FR-1", "MF-27", DragState.Invalid, graph.clickEdge("FR-1" -> "CO-2") >> _)
        "sameEdge"    - testEdgeEditorNewEdgeRejection("MF-1", "FR-2", DragState.NoOp, graph.clickEdge("MF-1" -> "FR-2") >> _)
        "unrelated"   - testEdgeEditorNewEdgeOk(graph.clickEdge("MF-1" -> "FR-2") >> _)
      }
    }
  }
}
