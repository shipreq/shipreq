package shipreq.base.db

import doobie._
import doobie.postgres.implicits._
import java.time._
import org.postgresql.util.PGInterval
import shipreq.base.util.BinaryData

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

  implicit val doobieReadInstant: Read[Instant] =
    Read.fromGet(doobieMetaInstant.get)

  implicit val doobieWriteInstant: Write[Instant] =
    Write.fromPut(doobieMetaInstant.put)

  implicit val doobieMetaBinaryData: Meta[BinaryData] =
    Meta[Array[Byte]].timap(BinaryData.unsafeFromArray)(_.unsafeArray)

  implicit val doobieReadBinaryData: Read[BinaryData] =
    Read.fromGet(doobieMetaBinaryData.get)

  implicit val doobieWriteBinaryData: Write[BinaryData] =
    Write.fromPut(doobieMetaBinaryData.put)

}
