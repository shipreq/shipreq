package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import scalaz.effect.IO
import scalaz.syntax.all._
import scalaz.~>
import shipreq.base.db.DoobieHelpers._
import shipreq.taskman.api
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.event.{ActiveEvent, EventOrd}
import shipreq.webapp.base.hash.HashRec.Collection
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.ServerProtocol

object Interpreters {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val dbAlgebra: DB.Algebra[ConnectionIO] =
    new DB.Algebra[ConnectionIO] {

      override def createProject(id: api.UserId): ConnectionIO[ProjectId] =
        DbLogic.project.create(id)

      override def loadProjectHeader(id: ProjectId): ConnectionIO[Option[ProjectHeader]] =
        DbLogic.project.findProjectHeader(id)

      override def loadProjectMetaData(id: ProjectId): ConnectionIO[Option[ProjectMetaData]] =
        DbLogic.project.findProjectMetaData(id)

      override def loadProject(id: ProjectId): ConnectionIO[DB.ProjectLoad] =
        DbLogic.event.findAll2(id)

      override def findAllProjectMetaDataForUser(id: api.UserId): ConnectionIO[List[ProjectMetaData]] =
        DbLogic.project.findAllProjectMetaDataForUser(id)

      override def saveProjectEvent(id: ProjectId, seq: EventOrd, e: ActiveEvent, hrs: Collection): ConnectionIO[Option[Throwable]] =
        DbLogic.event.create(id, seq, e, hrs).attempt.map(_.fold[Option[Throwable]](Some(_), _ => None))

      override def inDbTransaction[A](f: ConnectionIO[A]): ConnectionIO[A] =
        f.inTransaction
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val dbTrans: ConnectionIO ~> IO =
    new (ConnectionIO ~> IO) {
      override def apply[A](fa: ConnectionIO[A]) = DI.dbAccess.io.trans(fa)
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  val homeSpaLogic : HomeSpaLogic [IO] = HomeSpaLogic [ConnectionIO, IO]
  val projectServer: ProjectServer[IO] = ProjectServer[ConnectionIO, IO](ProjectServer.BroadcastTo.All)
}
