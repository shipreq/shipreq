package shipreq.base.db

import doobie.enum.jdbctype
import doobie.imports._
import io.circe.Json
import io.circe.parser.parse
import java.time.{Duration, Instant, OffsetDateTime, ZoneId}
import doobie.free.{preparedstatement => PS, resultset => RS}
import org.postgresql.util.{PGInterval, PGobject}

object DoobieMeta {

  def pgObject(typ: String, value: String): PGobject = {
    val o = new PGobject()
    o.setType(typ)
    o.setValue(value)
    o
  }

  val doobieMetaPgJsonb: Meta[PGobject] =
    Meta.other[PGobject]("jsonb")

  private def strToJson(s: String): Json =
    parse(s) match {
      case Right(j) => j
      case Left(e)  => throw e
    }

  implicit val doobieMetaJson: Meta[Json] =
    doobieMetaPgJsonb.xmap[Json](
      o => strToJson(o.getValue),
      j => pgObject("jsonb", j.noSpaces))

  implicit val doobieMetaDuration: Meta[Duration] =
    Meta.other[PGInterval]("interval").nxmap(
      i => sys.error("Reading a PGInterval into a Duration is not yet supported."),
      d => new PGInterval(
        0, 0, 0, 0, 0, // years, months, days, hours, minutes
        d.getSeconds.toDouble + d.getNano/1000000000.0
      )
    )

  implicit val doobieMetaOffsetDateTime: Meta[OffsetDateTime] =
    Meta.advanced[OffsetDateTime](
      scalaz.NonEmptyList(jdbctype.TimestampWithTimezone),
      scalaz.NonEmptyList("TIMESTAMPTZ"),
      _.getObject(_, classOf[OffsetDateTime]),
      PS.setObject,
      RS.updateObject
    )

  private[this] val UTC = ZoneId.of("UTC")

  implicit val doobieMetaInstant: Meta[Instant] =
    doobieMetaOffsetDateTime.nxmap(_.toInstant, OffsetDateTime.ofInstant(_, UTC))

}
