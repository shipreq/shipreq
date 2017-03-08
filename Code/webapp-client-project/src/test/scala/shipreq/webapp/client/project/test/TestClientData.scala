package shipreq.webapp.client.project.test

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.data.{Project, ProjectCatalogue}
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.project.app.state.{Changes, ClientData}
import shipreq.webapp.client.project.lib.DataReusability.reusabilityProject

final class TestClientData(init: Project, pi: Option[ProjectCatalogue.Item]) extends ClientData {
  override val pxProject = Px(init).withReuse.manualUpdate

  override var _projectSummary = pi getOrElse summariseProject(init)

  override def applyEvents(ves: VerifiedEvents): Callback =
    CallbackTo(tryApplyVerifiedEvents(ves)) >>= {
      case \/-(c)   => applyChanges(c)
      case -\/(err) => Callback.empty
    }

  def applyChanges(c: Changes): Callback =
    Callback(pxProject.set(c.p2)) >> updateProjectSummary(c.ves) >> broadcast(c)

  def tryApplyVerifiedEvents(ves: VerifiedEvents): String \/ Changes = {
    val p1 = project()
    ApplyEvent.trusted.applyVerified(ves)(p1)
      .map(p2 => Changes(ves, p1, p2))
  }

  def applyTestEvents(e: Event*): Unit = {
    val ves = verifyEvents(project())(e: _*)
    tryApplyVerifiedEvents(ves) match {
      case \/-(c)   => applyChanges(c).runNow()
      case -\/(err) => fail("Failed to apply event(s): " + err)
    }
  }
}

object TestClientData {
  def apply(p: Project, pi: ProjectCatalogue.Item = null) =
    new TestClientData(p, Option(pi))
}
