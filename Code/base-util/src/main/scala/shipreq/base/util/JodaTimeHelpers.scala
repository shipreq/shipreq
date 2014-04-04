package shipreq.base.util.jodatime

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.regex.Pattern
import org.joda.time.Period
import scala.concurrent.duration.FiniteDuration
import shipreq.base.util.ExternalValueReader.Retriever
import shipreq.base.util.{ErrorOr, Error, StringParsingBase}

object JodaTimeHelpers {

  implicit class PeriodConv(val p: Period) extends AnyVal {
    def toScala: FiniteDuration = FiniteDuration(p.toStandardDuration.getMillis, MILLISECONDS)
  }

  implicit class Units(val n: Int) extends AnyVal {

    def ms           = Period millis n
    def millis       = Period millis n
    def millisecond  = Period millis n
    def milliseconds = Period millis n

    def sec          = Period seconds n
    def second       = Period seconds n
    def seconds      = Period seconds n

    def min          = Period minutes n
    def minute       = Period minutes n
    def minutes      = Period minutes n

    def hr           = Period hours n
    def hour         = Period hours n
    def hours        = Period hours n

    def day          = Period days n
    def days         = Period days n

    def month        = Period months n
    def months       = Period months n

    def week         = Period weeks n
    def weeks        = Period weeks n

    def year         = Period years n
    def years        = Period years n
  }
}

object JodaTimeValueRetrievers {
  val periodRegex = """^(\d+)\s*([a-zA-Z]+)$""".r.pattern

  sealed abstract class TimeUnit(readRegexS: String) {
    def period(q: Int): Period
    val readRegex = Pattern.compile("^(?:" + readRegexS + ")$", Pattern.CASE_INSENSITIVE)
  }
  object TimeUnit {
    case object Ms extends TimeUnit("ms|millis(?:econds?)?") {
      override def period(n: Int) = Period millis n
    }
    case object Sec extends TimeUnit("s|sec(?:onds?)?") {
      override def period(n: Int) = Period seconds n
    }
    case object Min extends TimeUnit("min(?:utes?)?") {
      override def period(n: Int) = Period minutes n
    }
    case object Hour extends TimeUnit("hr|hours?") {
      override def period(n: Int) = Period hours n
    }
    case object Day extends TimeUnit("d|days?") {
      override def period(n: Int) = Period days n
    }
    case object Week extends TimeUnit("w|weeks?") {
      override def period(n: Int) = Period weeks n
    }
    case object Month extends TimeUnit("months?") {
      override def period(n: Int) = Period months n
    }
    case object Year extends TimeUnit("yr|years?") {
      override def period(n: Int) = Period years n
    }
    val values: List[TimeUnit] = List(Ms, Sec, Min, Hour, Day, Week, Month, Year)
  }
}

case class JodaTimeValueRetrievers(rs: Retriever[String]) extends StringParsingBase(rs) {
  import shipreq.base.util.ExternalValueReader._
  import JodaTimeValueRetrievers._

  def parseTimeUnit(s: String) =
    ErrorOr.fromOption(
      TimeUnit.values.find(_.readRegex.matcher(s).matches),
      s"Unable to parse time unit: '$s'"
    )

  private[this] implicit val retrieverTU: Retriever[TimeUnit] =
    tryParseE(parseTimeUnit)

  implicit val retrieverPeriod: Retriever[Period] =
    tryParseE(s =>
      if (s == "0")
        ErrorOr(Period.ZERO)
      else {
        val m = periodRegex.matcher(s)
        if (!m.matches)
          Error(s"Unable to parse into quantity and unit: '$s'")
        else
          for {
            n <- ErrorOr.safe(java.lang.Integer.parseInt(m group 1))
            u <- parseTimeUnit(m group 2)
          } yield u.period(n)
      }
    )
}
