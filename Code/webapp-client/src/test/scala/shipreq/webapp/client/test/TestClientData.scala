package shipreq.webapp.client.test

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.app.state.{Changes, ClientData}
import shipreq.webapp.client.lib.DataReusability.reusabilityProject
import scalaz.{-\/, \/, \/-}

final class TestClientData(init: Project) extends ClientData {
  override val pxProject = Px(init)

  override def applyEvents(ves: VerifiedEvents): Callback =
    CallbackTo(tryApplyVerifiedEvents(ves)) >>= {
      case \/-(c)   => applyChanges(c)
      case -\/(err) => Callback.empty
    }

  def applyChanges(c: Changes): Callback =
    Callback(pxProject.set(c.p2)) >> broadcast(c)

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
  def apply(p: Project) = new TestClientData(p)
}
