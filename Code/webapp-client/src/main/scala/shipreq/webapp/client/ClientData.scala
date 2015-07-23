package shipreq.webapp.client

import japgolly.scalajs.react.extra.Broadcaster
import scalaz.{-\/, \/-}
import scalaz.effect.IO
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{ApplyEvent, VerifiedEvents}
import shipreq.webapp.base.protocol.RemoteFns.ProjectInit
import shipreq.webapp.client.lib.{SuccessIO, FailureIO, ConsoleIO}

final class ClientData(init: Project) extends Broadcaster[Changes] {

  private[this] var pvar = init

  @inline def project = pvar

  def applyEvents(ves: VerifiedEvents): IO[Unit] = IO {
    val p1 = pvar
    ApplyEvent.trusted.applyVerified(ves)(p1) match {
      case \/-(p2) =>
        pvar = p2
        broadcast(Changes(ves, p1, p2))
      case -\/(err) =>
        // TODO Do more when VerifiedEvent application fails
        ConsoleIO(_ error s"Update failed. $err").unsafePerformIO()
    }
  }

  def applyEventsS(ves: VerifiedEvents): SuccessIO =
    SuccessIO(applyEvents(ves))
}

object ClientData {

  def init(cp: ClientProtocol, remoteInit: ProjectInit.Instance, onSuccess: ClientData => IO[Unit]): IO[Unit] =
    cp.call(remoteInit)((),
      p => SuccessIO(onSuccess(new ClientData(p))),
      cp.consumeGenericFailure) // TODO handle failure properly
}