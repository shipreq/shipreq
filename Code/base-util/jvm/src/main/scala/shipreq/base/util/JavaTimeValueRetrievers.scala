package shipreq.base.util

import java.time._
import java.util.regex.Pattern
import shipreq.base.util.ExternalValueReader.Retriever

object JavaTimeValueRetrievers {

  val durationRegex = """^(\d+)\s*([a-zA-Z]+)$""".r.pattern

  sealed abstract class TimeUnit(readRegexS: String) {
    def duration(q: Int): Duration
    val readRegex = Pattern.compile("^(?:" + readRegexS + ")$", Pattern.CASE_INSENSITIVE)
  }
  object TimeUnit {
    case object Ms extends TimeUnit("ms|millis(?:econds?)?") {
      override def duration(n: Int) = Duration ofMillis n
    }
    case object Sec extends TimeUnit("s|sec(?:onds?)?") {
      override def duration(n: Int) = Duration ofSeconds n
    }
    case object Min extends TimeUnit("min(?:utes?)?") {
      override def duration(n: Int) = Duration ofMinutes n
    }
    case object Hour extends TimeUnit("hr|hours?") {
      override def duration(n: Int) = Duration ofHours n
    }
    case object Day extends TimeUnit("d|days?") {
      override def duration(n: Int) = Duration ofDays n
    }
    case object Week extends TimeUnit("w|weeks?") {
      override def duration(n: Int) = Duration.ofDays(n * 7)
    }
    case object Month extends TimeUnit("months?") {
      override def duration(n: Int) = Duration.ofDays((n * 365.25 / 12).toInt)
    }
    case object Year extends TimeUnit("yr|years?") {
      override def duration(n: Int) = Duration.ofDays((n * 365.25).toInt)
    }
    val values: List[TimeUnit] = List(Ms, Sec, Min, Hour, Day, Week, Month, Year)
  }
}

final case class JavaTimeValueRetrievers(rs: Retriever[String]) extends StringParsingBase(rs) {
  import ExternalValueReader._
  import JavaTimeValueRetrievers._

  def parseTimeUnit(s: String) =
    ErrorOr.fromOptionS(
      TimeUnit.values.find(_.readRegex.matcher(s).matches),
      s"Unable to parse time unit: '$s'"
    )

  private[this] implicit val retrieverTU: Retriever[TimeUnit] =
    tryParseE(parseTimeUnit)

  implicit val retrieverDuration: Retriever[Duration] =
    tryParseE(s =>
      if (s == "0")
        ErrorOr(Duration.ZERO)
      else {
        val m = durationRegex.matcher(s)
        if (!m.matches)
          ErrorOr.error(s"Unable to parse into quantity and unit: '$s'")
        else
          for {
            n <- ErrorOr.safe(java.lang.Integer.parseInt(m group 1))
            u <- parseTimeUnit(m group 2)
          } yield u.duration(n)
      }
    )
}
