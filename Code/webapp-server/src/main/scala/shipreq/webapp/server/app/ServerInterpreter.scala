package shipreq.webapp.server.app

import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import net.liftweb.common._
import net.liftweb.http.S
import scalaz.effect.IO
import scalaz.syntax.monad._
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.ServerProtocol

object ServerInterpreter extends Server.Algebra[IO] {

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