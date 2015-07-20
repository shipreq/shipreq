package shipreq.webapp.client

import japgolly.scalajs.react.extra.Broadcaster
import scalaz.effect.IO
import shipreq.webapp.client.delta._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.delta.RemoteDelta
import shipreq.webapp.base.protocol.Routines.ProjectInit
import shipreq.webapp.client.lib.{FailureIO, ConsoleIO}

final class ClientData(init: Project) extends Broadcaster[LocalDelta] {

  private[this] var pvar = init

  @inline def project = pvar

  def update(rd: RemoteDelta): IO[Unit] =
    RemoteDeltaAp(project, rd) match {
      case RemoteDeltaAp.Success(newProject, localDelta) => IO[Unit] {
        pvar = newProject
        broadcast(localDelta)
      }
      case RemoteDeltaAp.Failure =>
        ConsoleIO(_ error s"Update failed.\n\nΠ: $project\n\nΔ: $rd")
    }
}

object ClientData {

  def init(cp: ClientProtocol, rpc: ProjectInit.Remote, s: ClientData => IO[Unit]): IO[Unit] = {
    val f = FailureIO(ConsoleIO(_ error "Page initialisation failed.")) // TODO handle failure properly
    cp.call(rpc)((), p => s(new ClientData(p)), f)
  }

}