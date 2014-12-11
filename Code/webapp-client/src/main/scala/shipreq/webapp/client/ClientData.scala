package shipreq.webapp.client

import japgolly.scalajs.react.experiment.Broadcaster
import scalaz.effect.IO
import shipreq.webapp.client.delta._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.data.delta.RemoteDelta
import shipreq.webapp.base.protocol.Routines.ProjectInit
import shipreq.webapp.client.lib.{FailureIO, Console}

final class ClientData(init: Project) extends Broadcaster[LocalDelta] {

  private[this] var pvar = init

  @inline def project = pvar

  def update(d: RemoteDelta): IO[Unit] =
      RemoteDelta(project, d) match {
        case Applied(p2, d2) => IO[Unit] {
          pvar = p2
          broadcast(d2)
        }
        case CouldntApply =>
          Console.errorIO(s"Update failed.\n\nΠ: $project\n\nΔ: $d")
      }
}

object ClientData {

  // TODO failure callback
  def init(rpc: ProjectInit.Remote, s: ClientData => IO[Unit]): IO[Unit] =
    ClientProtocol.call(rpc)((), p => s(new ClientData(p)), FailureIO.nop)

}