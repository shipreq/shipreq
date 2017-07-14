package shipreq.webapp.server.lib

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import shipreq.base.util.log.HasLogger

object Misc extends Misc {

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
}