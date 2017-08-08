package shipreq.base.util

import java.time._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object JavaTimeHelpers {

  @inline implicit class DurationExt(private val d: Duration) extends AnyVal {
    def toScala: FiniteDuration = FiniteDuration(d.toNanos, TimeUnit.NANOSECONDS)
    def isShorterThan(x: Duration) = d.compareTo(x) < 0
    def isLongerThan(x: Duration) = d.compareTo(x) > 0
  }
}
