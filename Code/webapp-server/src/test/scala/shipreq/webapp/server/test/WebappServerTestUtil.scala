package shipreq.webapp.server.test

import java.time._
import shipreq.webapp.base.test.{WebappTestEquality, WebappTestUtil}

trait WebappServerTestEquality extends WebappTestEquality {
}

trait WebappServerTestUtil extends WebappTestUtil {
  import WebappServerTestUtil._

  implicit def toWSTU_IntExt(i: Int) = new WSTU_IntExt(i)
  implicit def toWSTU_DurationExt(d: Duration) = new WSTU_DurationExt(d)
}

object WebappServerTestUtil
  extends WebappServerTestEquality
     with WebappServerTestUtil {

  class WSTU_IntExt(private val i: Int) extends AnyVal {
    def second  = Duration.ofSeconds(i)
    def seconds = Duration.ofSeconds(i)
    def minute  = Duration.ofMinutes(i)
    def minutes = Duration.ofMinutes(i)
    def hour    = Duration.ofHours(i)
    def hours   = Duration.ofHours(i)
    def day     = Duration.ofDays(i)
    def days    = Duration.ofDays(i)
    def week    = Duration.ofDays(i * 7)
    def weeks   = Duration.ofDays(i * 7)
  }

  class WSTU_DurationExt(private val d: Duration) extends AnyVal {
    def ago: Instant = Instant.now().minus(d)
  }

}
