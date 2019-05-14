package shipreq.base.util

import java.time._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object JavaTimeHelpers {

  @inline implicit class DurationExt(private val d: Duration) extends AnyVal {
    def toScala: FiniteDuration = FiniteDuration(d.toNanos, TimeUnit.NANOSECONDS)
    def isShorterThan(x: Duration) = d.compareTo(x) < 0
    def isLongerThan(x: Duration) = d.compareTo(x) > 0
    def asSeconds: Double = d.getSeconds.toDouble + d.getNano.toDouble / 1000000000
    def asMinutes: Double = asSeconds / 60.0

    def conciseDesc: String =
      if (d.getSeconds == 0) {
        val n = d.getNano
        if      (n >= 1000000) (n/1000000) + " ms"
        else if (n >= 1000)    (n/1000)    + " us"
        else                   n           + " ns"
      } else {
        val sec = asSeconds
        if (sec <= 60) "%.1f sec".format(sec) else {
          val min = sec / 60.0
          if (min <= 60) "%.1f min".format(min) else {
            val hr = min / 60.0
            if (hr <= 24) "%.2f hr".format(hr) else {
              val days = hr / 24.0
              "%.2f days".format(days)
            }
          }
        }
      }
  }

}
