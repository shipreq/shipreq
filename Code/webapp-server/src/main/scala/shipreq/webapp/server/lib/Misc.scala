package shipreq.webapp.server.lib

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, ZoneOffset}
import net.liftweb.http.S
import scala.annotation.tailrec
import scala.util.Random
import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.app.DI

object Misc extends Misc {

  val RNG = new Random()

  val Iso8601Format = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZone(ZoneOffset.UTC)

  final class InstantExt(private val i: Instant) extends AnyVal {
    def toStringIso8601: String =
      Iso8601Format.format(i)
  }
}

trait Misc extends HasLogger {
  import Misc._

  implicit def InstantExt(v: Instant): InstantExt =
    new InstantExt(v)

  def clientIp(): Option[String] = (
    S.originalRequest.filter(_.request ne null).map(_.remoteAddr)
      or S.containerRequest.map(_.remoteAddress)
      or S.request.filter(_.request ne null).map(_.remoteAddr)
    // println("X-Real-IP: " + req.header("X-Real-IP"))
    // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
    )

  def isExpired_?(startTime: Instant, timeToLive: Duration, now: Instant = Instant.now()): Boolean =
    startTime plus timeToLive isBefore now

  def randomString(length: Int): String =
    RNG.alphanumeric.take(length).mkString

  def randomConfirmationToken(): String =
    randomString(DI.serverConfig.confirmationTokenLength)

//  @tailrec
//  final def retry[T](n: Int, firstError: Option[Throwable] = None)(fn: => T): T = {
//    import scala.util.{Failure, Success, Try}
//    Try { fn } match {
//      case Success(result)      => result
//      case Failure(e) if n > 0  => retry(n - 1, firstError orElse Some(e))(fn)
//      case Failure(e) if n <= 0 =>
//        firstError.foreach(log.debug("First retry failure.", _))
//        throw e
//    }
//  }
}