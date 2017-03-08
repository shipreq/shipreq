package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px, Reusability}
import japgolly.scalajs.react.{Callback, CallbackTo}
import java.time.Instant
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data.{Project, ProjectCatalogue}
import shipreq.webapp.base.event.{ApplyEvent, Event, VerifiedEvents}
import shipreq.webapp.base.protocol.ProjectInit
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.lib.Logger
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.lib.DataReusability.reusabilityProject

abstract class ClientData extends Broadcaster[Changes] {

  val pxProject: Px[Project]
  def applyEvents(ves: VerifiedEvents): Callback
  protected var _projectSummary: ProjectCatalogue.Item

  def project(): Project =
    pxProject.value()

  @inline final def projectCB: CallbackTo[Project] =
    CallbackTo(project())

  def applyEventsS(ves: VerifiedEvents): TCB.Success =
    TCB.Success(applyEvents(ves))

  def projectSummary(): ProjectCatalogue.Item =
    _projectSummary

  def updateProjectSummary(ves: VerifiedEvents): Callback =
    Callback {
      val o = _projectSummary
      _projectSummary = o.copy(
        name          = project().name,
        eventCount    = o.eventCount + ves.length,
        reqCount      = o.reqCount + ves.count(Event reqCreationEventFilter _.event),
        lastUpdatedAt = Some(Instant.now()))
    }
}

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

  private[state] final class Impl(init: Project, initSummary: ProjectCatalogue.Item) extends ClientData {
    override val pxProject = Px(init).withReuse.manualUpdate
    override protected var _projectSummary = initSummary

    override def applyEvents(ves: VerifiedEvents): Callback =
      projectCB >>= (p1 =>
        ApplyEvent.trusted.applyVerified(ves)(p1) match {

          case \/-(p2) =>
            Callback(pxProject.set(p2)) >>
              updateProjectSummary(ves) >>
              broadcast(Changes(ves, p1, p2))

          case -\/(err) =>
            // TODO Do more when VerifiedEvent application fails
            Logger(_ error s"Update failed. $err")
        }
      )
  }

  def init(initSummary: ProjectCatalogue.Item,
           cp         : ClientProtocol,
           remoteInit : ProjectInit.Instance)(
           onSuccess  : ClientData => Callback,
           onFailure  : String => Callback): Callback =

    cp.call(remoteInit)((),
      p => TCB.Success(onSuccess(new Impl(p, initSummary))),
      _.consumeAnd(e => TCB.Failure(onFailure(e))))
}