package shipreq.webapp.server.interpreter

import cats.syntax.all._
import java.time.{Duration, Instant}
import net.liftweb.actor.LAScheduler
import net.liftweb.common.{MDC => _, _}
import net.liftweb.http.provider.HTTPRequest
import net.liftweb.http.{Req, S}
import scala.concurrent.blocking
import shipreq.base.util.FxModule._
import shipreq.base.util.log.{HasLogger, MdcUtil}
import shipreq.webapp.base.data.IP
import shipreq.webapp.server.logic.algebra.Server

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
      f2 <- MdcUtil.preserve(f)
      _  <- Fx(LAScheduler.execute(() => f2.unsafeRun()))
    } yield ()

  @inline
  def extractIpFromXForwardedFor(xForwardedFor: String): String = {
    // If a request goes through multiple proxies, the IP addresses of each successive proxy is listed.
    // The left-most IP address is the IP address of the originating client.
    // The right-most IP address is the IP address of the most recent proxy.

    // ip chars are 0-9 [48,57], dot [46] and colon [58]
    // not ip chars are space [32] and comma [44]
    xForwardedFor.takeWhile(_ > ',')
  }

  override val clientIP: Fx[Option[IP]] = {

    val fromRequest: HTTPRequest => String = req => {
      // logger.info(req.headers.map(p => s"req[${p.name}] = ${p.values}").toList.sorted.mkString("Req headers:\n", "\n", ""))
      req.header("X-Forwarded-For") match {
        case f: Full[String] => extractIpFromXForwardedFor(f.value)
        case _               => req.remoteAddress
      }
    }

    val reqExists: Req => Boolean =
      _.request ne null

    val fromReq: Req => String =
      r => fromRequest(r.request)

    Fx {
      val box: Box[String] =
        S.originalRequest.filter(reqExists).map(fromReq) or
          S.containerRequest.map(fromRequest) or
          S.request.filter(reqExists).map(fromReq)

      box match {
        case f: Full[String] => Some(IP(f.value))
        case _               => None
      }
    }
  }

}