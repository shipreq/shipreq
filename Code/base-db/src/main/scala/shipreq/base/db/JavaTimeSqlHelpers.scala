package shipreq.base.db

import java.sql.Timestamp
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{Duration, OffsetDateTime, ZoneId}
import org.postgresql.util.PGInterval
import scala.slick.jdbc.{GetResult, PositionedParameters, SetParameter}

object JavaTimeSqlHelpers {

  // TODO Doesn't new JDBC driver support OffsetDateTime already?

  private val zoneId = ZoneId of "UTC"
  private def timestampToODT(t: Timestamp): OffsetDateTime =
    OffsetDateTime.ofInstant(t.toInstant, zoneId)
  implicit val GR_DateTime = GetResult(r => timestampToODT(r.nextTimestamp))
  implicit val GR_DateTimeOption = GetResult(r => r.nextTimestampOption.map(timestampToODT))

//  private val date2TzDateTimeFormatter =
//    new DateTimeFormatterBuilder()
//      .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
//      .optionalStart()
//      .appendFraction(ChronoField.NANO_OF_SECOND,0,6,true)
//      .optionalEnd()
//      .appendOffset("+HH:mm","+00")
//      .toFormatter()
//
//  implicit val GR_DateTime: GetResult[OffsetDateTime] =
//    GetResult(r => OffsetDateTime from date2TzDateTimeFormatter.parse(r.nextString()))
//
//  implicit val GR_DateTimeOption: GetResult[Option[OffsetDateTime]] =
//    GetResult(r => r.nextStringOption().map(OffsetDateTime from date2TzDateTimeFormatter.parse(_)))

  implicit object SP_Duration extends SetParameter[Duration] {
    def apply(d: Duration, pp: PositionedParameters): Unit = {
      val i = new PGInterval(
        0, 0, 0, 0, 0, // years, months, days, hours, minutes
        d.getSeconds.toDouble + d.getNano/1000000000.0
      )
      pp.setObject(i, java.sql.Types.OTHER)
    }
  }
}
