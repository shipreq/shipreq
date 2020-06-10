package shipreq.base.db

import doobie._
import doobie.implicits.javatime.JavaOffsetDateTimeMeta
import doobie.postgres.implicits._
import java.time._
import org.postgresql.util.PGInterval

object BaseDoobieCodecs {

  implicit val doobieWriteDuration: Write[Duration] =
    Write[PGInterval].contramap(d =>
      new PGInterval(
        0, 0, 0, 0, 0, // years, months, days, hours, minutes
        d.getSeconds.toDouble + d.getNano/1000000000.0
      )
    )

  private[this] val UTC = ZoneId.of("UTC")

  implicit val doobieMetaInstant: Meta[Instant] =
    Meta[OffsetDateTime].timap(_.toInstant)(OffsetDateTime.ofInstant(_, UTC))

}
