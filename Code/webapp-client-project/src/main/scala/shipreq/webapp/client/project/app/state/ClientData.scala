package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.{CallbackTo, Callback}
import japgolly.scalajs.react.extra.{Px, Reusability, Broadcaster}
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{ApplyEvent, VerifiedEvents}
import shipreq.webapp.base.protocol.ProjectInit
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.lib.Logger
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.lib.DataReusability.reusabilityProject

abstract class ClientData extends Broadcaster[Changes] {
  val pxProject: Px[Project]

  def applyEvents(ves: VerifiedEvents): Callback

  // ---------------------------------------------

  def project(): Project =
    pxProject.value()

  @inline def projectCB: CallbackTo[Project] =
    CallbackTo(project())

  def applyEventsS(ves: VerifiedEvents): TCB.Success =
    TCB.Success(applyEvents(ves))
}

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

  private class Impl(init: Project) extends ClientData {
    override val pxProject = Px(init)

    override def applyEvents(ves: VerifiedEvents): Callback =
      projectCB >>= (p1 =>
        ApplyEvent.trusted.applyVerified(ves)(p1) match {

          case \/-(p2) =>
            Callback(pxProject.set(p2)) >> broadcast(Changes(ves, p1, p2))

          case -\/(err) =>
            // TODO Do more when VerifiedEvent application fails
            Logger(_ error s"Update failed. $err")
        }
      )
  }

  def init(cp: ClientProtocol, remoteInit: ProjectInit.Instance, onSuccess: ClientData => Callback): Callback =
    cp.call(remoteInit)((),
      p => TCB.Success(onSuccess(new Impl(p))),
      cp.consumeGenericFailure) // TODO handle failure properly
}