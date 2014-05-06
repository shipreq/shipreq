package shipreq.base.db

import java.sql.Timestamp
import org.joda.time.{Period, DateTime}
import org.postgresql.util.PGInterval
import scala.slick.jdbc.{PositionedParameters, SetParameter, GetResult}

object JodaTimeSqlHelpers {

  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)

  implicit val GR_DateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))
  implicit val GR_DateTimeOption = GetResult(r => r.nextTimestampOption.map(TimestampToDateTime))

  implicit object SP_Period extends SetParameter[Period] {
    def apply(v: Period, pp: PositionedParameters): Unit = {
      val i = new PGInterval(
        v.getYears, v.getMonths, v.getDays, v.getHours, v.getMinutes,
        v.getSeconds.toDouble + v.getMillis/1000.0
      )
      pp.setObject(i, java.sql.Types.OTHER)
    }
  }
}
