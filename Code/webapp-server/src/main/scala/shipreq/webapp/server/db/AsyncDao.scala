package shipreq.webapp.server.db

import net.liftweb.actor.SpecializedLiftActor
import scala.slick.jdbc.JdbcBackend.Session
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.ProjectId
import AsyncDb._

object AsyncDb {
  sealed trait Cmd
  case class DeleteProject(id: ProjectId) extends Cmd
}

trait AsyncDb {
  def !(msg: Cmd): Unit
}

object AsyncDbImpl extends AsyncDb with SpecializedLiftActor[Cmd] with DI {

  protected def dba(f: AsyncDao => Unit): Unit =
    daoProvider.withRawSession(implicit s => f(new AsyncDao))

  protected def messageHandler: PartialFunction[Cmd, Unit] = {
    case DeleteProject(id) => dba(_ deleteProject id)
  }
}

private[db] class AsyncDao(implicit val session: Session) {
  import Sql._

  // Deletion automatically cascades to shares and use cases. See FKs in child tables.
  def deleteProject(id: ProjectId): Unit =
    DeleteProjectHard(id).execute
}