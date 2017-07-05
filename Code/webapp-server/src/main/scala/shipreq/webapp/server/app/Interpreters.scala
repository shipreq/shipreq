package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import scalaz.effect.IO
import scalaz.syntax.all._
import scalaz.{\/, ~>}
import shipreq.base.db.DbAccess
import shipreq.base.util.Permission
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.ServerProtocol
import shipreq.webapp.server.security.AppSecurityRealm

object Interpreters {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit def dbAlgebra(implicit config: ServerConfig): DB.Algebra[ConnectionIO] =
    new DbInterpreter()

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit def runDB(implicit dbAccess: DbAccess): ConnectionIO ~> IO =
    new (ConnectionIO ~> IO) {
      override def apply[A](fa: ConnectionIO[A]) = dbAccess.io.trans(fa)
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val serverAlgebra: Server.Algebra[IO] =
    new Server.Algebra[IO] {
      import net.liftweb.common._
      import net.liftweb.http.S

      override def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => IO[p.Response]): IO[p.Instance] =
        IO(ServerProtocol.createServerSideProc(p)(localFn(_)))

      override val now: IO[Instant] =
        IO(Instant.now())

      override def delay[A](f: IO[A], d: Duration): IO[A] =
        IO(Thread.sleep(d.toMillis)) >> f // TODO Thread.sleep lolz

      override def fork[A](f: IO[A]): IO[Unit] =
        IO(LAScheduler.execute(() => f.unsafePerformIO()))

      override val clientIP: IO[Option[IP]] =
        IO {
          // println("X-Real-IP: " + req.header("X-Real-IP"))
          // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
          val box: Box[String] =
            S.originalRequest.filter(_.request ne null).map(_.remoteAddr) or
              S.containerRequest.map(_.remoteAddress) or
              S.request.filter(_.request ne null).map(_.remoteAddr)

          box match {
            case Full(ip) => Some(IP(ip))
            case _        => None
          }
        }
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit def securityAlgebra(implicit config: ServerConfig): Security.Algebra[IO] =
    new Security.Algebra[IO] {

      val delay: IO[Unit] =
        config.attackFrustrationDelayMs match {
          case 0  => IO(())
          case ms => IO(Thread.sleep(ms))
        }

      override def protect[A](vulnerable: IO[A]): IO[A] =
        delay >> vulnerable

      override def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): IO[Permission] =
        IO(AppSecurityRealm.attemptLogin(user.fold(_.value, _.value), password))

      val hashFn = AppSecurityRealm.randomHashFn
      override def hashPassword(p: PlainTextPassword): IO[PasswordAndSalt] =
        IO(hashFn(p))
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  implicit val projectStore: ProjectServer.StoreAlgebra[IO] =
    Store.Algebra.concurrentHashMap()
}
