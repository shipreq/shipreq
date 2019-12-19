package shipreq.webapp.server.app

import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import net.liftweb.common.{MDC => _, _}
import net.liftweb.http.S
import net.liftweb.http.provider.HTTPRequest
import scala.concurrent.blocking
import scalaz.syntax.monad._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.{HasLogger, MDC}
import shipreq.webapp.server.logic._

object ServerInterpreter extends Server.Algebra[Fx] with HasLogger {

  override val now: Fx[Instant] =
    Fx.now

  override def measureDuration[A](f: Fx[A]): Fx[(A, Duration)] =
    f.measureDuration

  override def measureDuration_[A](f: Fx[A]): Fx[Duration] =
    f.measureDuration_

  override def delay[A](f: Fx[A], d: Duration): Fx[A] =
    Fx(blocking(Thread.sleep(d.toMillis))) >> f // TODO Thread.sleep lolz

  override def fork[A](f: Fx[A]): Fx[Unit] =
    for {
      f2 <- MDC.preserve(f)
      _  <- Fx(LAScheduler.execute(() => f2.unsafeRun()))
    } yield ()

  override val clientIP: Fx[Option[IP]] = {
    def fromRequest(req: HTTPRequest): String = {
      // logger.info(req.headers.map(p => s"req[${p.name}] = ${p.values}").toList.sorted.mkString("Req headers:\n", "\n", ""))
      req.header("X-Forwarded-For").toOption match {
        case Some(fwd) => fwd.takeWhile(_ != ',')
        case None      => req.remoteAddress
      }
    }

    Fx {
      val box: Box[String] =
        S.originalRequest.filter(_.request ne null).map(r => fromRequest(r.request)) or
          S.containerRequest.map(fromRequest) or
          S.request.filter(_.request ne null).map(r => fromRequest(r.request))

      box match {
        case Full(ip) => Some(IP(ip))
        case _        => None
      }
    }
  }
}