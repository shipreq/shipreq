package shipreq.webapp.client.project.app.pages.admin.access

import shipreq.webapp.base.data.ProjectPerm
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.SampleProject
import utest._
import utest.framework.TestPath

object AccessPageTest extends TestSuite {
  import AccessPageTestDsl._

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftAccessPageTests(p).asAction(name),
      page = Page.Access,
      project = project)
  }

  private val project = applyEventsSuccessfully(SampleProject.project,
    Event.AccessUpdate(Map(PublicUserId2 -> Some(ProjectPerm.Admin))),
  )

  override def tests = Tests {

    "removeSelf" - {

      "cancel" - runActions(project)(
        confirmJs.setNextResponse(false)
          >> clickLeaveProject
          +> confirmJs.calls.assert(1)
          +> leaveProjectButtonLoading.assert(false)
          +> global.requestCount.assert(0)
      )

      "confirm" - runActions(project)(
        global.disableAutoResponse
          >> confirmJs.setNextResponse(true)
          >> clickLeaveProject
          +> confirmJs.calls.assert(1)
          +> leaveProjectButtonLoading.assert(true)
          +> global.requestCount.assert(1)
          >> global.failLastRequest
          +> leaveProjectButtonLoading.assert(false)
      )
    }
  }
}
