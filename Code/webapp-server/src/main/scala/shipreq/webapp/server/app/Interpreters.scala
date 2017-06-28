package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import scalaz.effect.IO
import scalaz.syntax.all._
import scalaz.~>
import shipreq.base.db.DbAccess
import shipreq.base.db.DoobieHelpers._
import shipreq.taskman.api
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.event.{ActiveEvent, EventOrd}
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.ServerProtocol

object Interpreters {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val dbAlgebra: DB.Algebra[ConnectionIO] =
    new DB.Algebra[ConnectionIO] {

      override def createEmptyProject(id: api.UserId): ConnectionIO[ProjectId] =
        DbLogic.project.create(id)

      override def getProjectHeader(id: ProjectId): ConnectionIO[Option[ProjectHeader]] =
        DbLogic.project.findProjectHeader(id)

      override def getProjectMetaData(id: ProjectId): ConnectionIO[Option[ProjectMetaData]] =
        DbLogic.project.findProjectMetaData(id)

      override def getAllProjectEvents(id: ProjectId): ConnectionIO[DB.ProjectEvents] =
        DbLogic.event.findAll2(id)

      override def getAllProjectMetaDataForUser(id: api.UserId): ConnectionIO[List[ProjectMetaData]] =
        DbLogic.project.findAllProjectMetaDataForUser(id)

      override def saveProjectEvent(id: ProjectId)(o: EventOrd, e: ActiveEvent, h: HashRec.Collection): ConnectionIO[Option[Throwable]] =
        DbLogic.event.create(id, o, e, h).attempt.map(_.fold[Option[Throwable]](Some(_), _ => None))

      override def inDbTransaction[A](f: ConnectionIO[A]): ConnectionIO[A] =
        f.inTransaction
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit def runDB(implicit dbAccess: DbAccess): ConnectionIO ~> IO =
    new (ConnectionIO ~> IO) {
      override def apply[A](fa: ConnectionIO[A]) = dbAccess.io.trans(fa)
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val serverAlgebra: Server.Algebra[IO] =
    new Server.Algebra[IO] {
      override def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => IO[p.Response]): IO[p.Instance] =
        IO(ServerProtocol.createServerSideProc(p)(localFn(_)))

      override val now: IO[Instant] =
        IO(Instant.now())

      override def delay[A](f: IO[A], d: Duration): IO[A] =
        IO(Thread.sleep(d.toMillis)) >> f // TODO Thread.sleep lolz

      override def fork[A](f: IO[A]): IO[Unit] =
        IO(LAScheduler.execute(() => f.unsafePerformIO()))
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val projectStore: ProjectServer.StoreAlgebra[IO] =
    Store.Algebra.concurrentHashMap()
}
