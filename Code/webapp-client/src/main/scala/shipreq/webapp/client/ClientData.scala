package shipreq.webapp.client

import org.scalajs.dom
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.client.delta._
import shipreq.webapp.client.protocol.{FailureIO, ClientProtocol}
import shipreq.webapp.shared.data.Project
import shipreq.webapp.shared.data.delta.{Rev, RemoteDelta}
import japgolly.scalajs.react.experiment.Broadcaster
import shipreq.webapp.shared.protocol.Routines.ProjectInit

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