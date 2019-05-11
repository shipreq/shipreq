package shipreq.webapp.server.app

import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import net.liftweb.common._
import net.liftweb.http.S
import scala.concurrent.blocking
import scalaz.syntax.monad._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
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
    Fx(LAScheduler.execute(() => f.unsafeRun()))

  override val clientIP: Fx[Option[IP]] =
    Fx {
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