package shipreq.base.util

import java.time._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object JavaTimeHelpers {

  @inline implicit class DurationInt(private val i: Int) extends AnyVal {
    @inline def milli  : Duration = Duration ofMillis i
    @inline def millis : Duration = Duration ofMillis i
    @inline def second : Duration = Duration ofSeconds i
    @inline def seconds: Duration = Duration ofSeconds i
    @inline def minute : Duration = Duration ofMinutes i
    @inline def minutes: Duration = Duration ofMinutes i
    @inline def hour   : Duration = Duration ofHours i
    @inline def hours  : Duration = Duration ofHours i
    @inline def day    : Duration = Duration ofDays i
    @inline def days   : Duration = Duration ofDays i
  }

  @inline implicit class DurationExt(private val d: Duration) extends AnyVal {
    def toScala: FiniteDuration = FiniteDuration(d.toNanos, TimeUnit.NANOSECONDS)
    def isShorterThan(x: Duration) = d.compareTo(x) < 0
    def isLongerThan(x: Duration) = d.compareTo(x) > 0
  }
}
