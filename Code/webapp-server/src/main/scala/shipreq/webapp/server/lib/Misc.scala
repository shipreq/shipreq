package shipreq.webapp.server.lib

import net.liftweb.http.S
import org.joda.time.{DateTime, DateTimeUtils, Period}
import org.joda.time.format.DateTimeFormat
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag
import scala.util.hashing.Hashing
import scala.util.Random
import scalaz.Memo
import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.app.AppConfig
import shipreq.webapp.server.data.ISO8601

// TODO shipreq.webapp.server.lib.Misc should be salvaged and pruned
object Misc extends Misc {

  val RNG = new Random()

  val NoEffect1: (Any => Unit) = _ => ()

//  val Iso8601Format = ISODateTimeFormat.dateTime.withZoneUTC
  val Iso8601Format = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZoneUTC

  implicit class AnyExt[V](val v: V) extends AnyVal {
    def modIf[VV >: V](cond: Boolean)(mod: V => VV): VV = if (cond) mod(v) else v
  }

  implicit class ShortExt(val a: Short) extends AnyVal {
    def +!(b: Int = 1): Short = (a + b).toShort
  }

  implicit class DateTimeExt(val t: DateTime) extends AnyVal {
    def >(timeToLive: Period) = isExpired_?(t, timeToLive)
    def <=(timeToLive: Period) = ! >(timeToLive)
    def toIso8601: ISO8601 = Misc.toIso8601Str(t)
  }
}

trait Misc extends HasLogger {
  import Misc._

  implicit def impAnyExt[V](v: V) = AnyExt(v)
  implicit def impShortExt(v: Short) = ShortExt(v)
  implicit def impDateTimeExt(v: DateTime) = DateTimeExt(v)

  def clientIp: Option[String] = (
    S.originalRequest.filter(_.request ne null).map(_.remoteAddr)
      or S.containerRequest.map(_.remoteAddress)
      or S.request.filter(_.request ne null).map(_.remoteAddr)
    // println("X-Real-IP: " + req.header("X-Real-IP"))
    // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
    )

  final def currentTimeAsIso8601Str: ISO8601 =
    ISO8601(Iso8601Format.print(DateTimeUtils.currentTimeMillis))

  final def toIso8601Str(d: DateTime): ISO8601 =
    ISO8601(Iso8601Format.print(d))

  def isExpired_?(startTime: DateTime, timeToLive: Period, now: Long = DateTimeUtils.currentTimeMillis): Boolean =
    startTime plus timeToLive isBefore now

  def randomConfirmationToken = randomString(AppConfig.ConfirmationTokenLength)

  def randomString(length: Int): String = RNG.alphanumeric.take(length).mkString

  //def modIf[V, VV >: V](v: V, cond: Boolean)(mod: V => VV): VV = if (cond) mod(v) else v

  @tailrec
  final def retry[T](n: Int, firstError: Option[Throwable] = None)(fn: => T): T = {
    import scala.util.{Try, Success, Failure}
    Try { fn } match {
      case Success(result)      => result
      case Failure(e) if n > 0  => retry(n - 1, firstError orElse Some(e))(fn)
      case Failure(e) if n <= 0 =>
        firstError.foreach(log.debug("First retry failure.", _))
        throw e
    }
  }

  def isCovar[T](a: Any)(implicit m: ClassTag[T]): Boolean =
    m.runtimeClass.isAssignableFrom(a.getClass)

  def filterCovar[T](list: List[_])(implicit m: ClassTag[T]): List[T] =
    list.filter(isCovar[T]).asInstanceOf[List[T]]

  def pluralise(singular: String, plural: String)(c: Long): String =
    if (c == 1)
      s"1 $singular"
    else
      s"$c $plural"

  /**
   * Fast impl that checks if a string contains any alphabetic characters.
   */
  final def containsAlpha(s: String): Boolean = {
    var i = s.length - 1
    while (i >= 0) {
      val ch = s(i)
      if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
        return true
      i -= 1
    }
    false
  }

  def newMemo[K, V](eqFn: Equiv[K] = Equiv.universal[K], hashFn: Hashing[K] = Hashing.default[K]): Memo[K, V] =
    Memo.mutableMapMemo(new TrieMap[K, V](hashFn, eqFn))
}