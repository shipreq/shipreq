package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px, Reusability}
import japgolly.scalajs.react.{Callback, CallbackTo}
import java.time.Instant
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.{ApplyEvent, Event, VerifiedEvents}
import shipreq.webapp.base.protocol.{ErrorMsg, ProjectSpaProtocols, ServerSideProc}
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.lib.Logger
import shipreq.webapp.client.base.protocol.{ClientProtocol, ServerSideProcInvoker}
import shipreq.webapp.client.project.lib.DataReusability.reusabilityProject

abstract class ClientData extends Broadcaster[Changes] {

  val pxProject: Px[Project]
  def applyEvents(ves: VerifiedEvents): Callback
  protected var _projectMetaData: ProjectMetaData

  def project(): Project =
    pxProject.value()

  @inline final def projectCB: CallbackTo[Project] =
    CallbackTo(project())

  def applyEventsS(ves: VerifiedEvents): TCB.Success =
    TCB.Success(applyEvents(ves))

  def projectMetaData(): ProjectMetaData =
    _projectMetaData

  def updateProjectMetaData(ves: VerifiedEvents): Callback =
    Callback {
      _projectMetaData = _projectMetaData.applyEvents(ves, Instant.now())
    }

  def serverSideProcToEvents[I](proc: ServerSideProc.Aux[ErrorMsg, I, VerifiedEvents], cp: ClientProtocol): ServerSideProcInvoker[I, VerifiedEvents] =
    new ServerSideProcInvoker((i, s, f) => cp.call(proc)(i, e => applyEventsS(e) >> s(e), _ consumeAnd f))
}

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

  private[state] final class Impl(init: Project, initMetaData: ProjectMetaData) extends ClientData {
    override val pxProject = Px(init).withReuse.manualUpdate
    override protected var _projectMetaData = initMetaData

    override def applyEvents(ves: VerifiedEvents): Callback =
      projectCB >>= (p1 =>
        ApplyEvent.trusted.applyVerified(ves)(p1) match {

          case \/-(p2) =>
            Callback(pxProject.set(p2)) >>
              updateProjectMetaData(ves) >>
              broadcast(Changes(ves, p1, p2))

          case -\/(err) =>
            // TODO Do more when VerifiedEvent application fails
            Logger(_ error s"Update failed. $err")
        }
      )
  }

  def init(initMetaData: ProjectMetaData,
           cp         : ClientProtocol,
           remoteInit : ProjectSpaProtocols.ProjectInit.Instance)(
           onSuccess  : ClientData => Callback,
           onFailure  : String => Callback): Callback =

    cp.call(remoteInit)((),
      p => TCB.Success(onSuccess(new Impl(p, initMetaData))),
      _.consumeAnd(e => TCB.Failure(onFailure(e))))
}