package shipreq.webapp.feature.validation

import java.util.regex.Pattern
import java.util.regex.Pattern.quote
import scala.util.matching.Regex
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.util.JsGen
import Constraint._

object Constraints {
  implicit def regexToPattern(regex: Regex): Pattern = regex.pattern

  val nonEmpty = predicate[String](_.nonEmpty, "!!_")("cannot be blank.")

  def matchesR(regex: Pattern) = predicate[String](regex.matcher(_).matches, s"${JsGen translateRegex regex}.test(_)")

  def startsWithR(regex: String) = matchesR(s"^(?:$regex).*".r)

  def endsWithR(regex: String) = matchesR(s".*(?:$regex)$$".r)

  def whitelistCharsR(charRegex: String) = matchesR(s"^[$charRegex]*$$".r)

  def whitelistCharsS(charList: String) = whitelistCharsR(quote(charList))

  def blacklistCharsR(charRegex: String) = matchesR(s"^[^$charRegex]*$$".r)

  def blacklistCharsS(charList: String) = blacklistCharsR(quote(charList))

  def containsR(regex: String) = matchesR(s".*$regex.*".r)

  /** Validates that a string contains at least one letter, and at least one number. */
  val containsAlphaAndNumber = matchesR(
    ".*?[A-Za-z].*?[0-9].*|.*?[0-9].*?[A-Za-z].*".r)(
    "must contain at least one letter, and at least one number.")

  /**
   * Validates that the length of a string is within min & max bounds.
   * @param range inclusive
   */
  def lengthInRange(range: Range) =
    predicate[String](range contains _.length, s"(_.length>=${range.min} && _.length<=${range.max})")(
      s"must be between ${range.min} and ${range.max} characters long.")

  def maximumLength(max: Int) = Constraint.perf[String](
    _.length <= max // avoid creating errMsg if unneeded
    , s => {
      val excess = s.length - max
      if (excess > 0)
        s"is too large by $excess characters." :: Nil
      else
        Nil
    }, s"_.length<=$max")

  val shortTextLimit = maximumLength(ShortTextMaxLength)

  val largeTextLimit = maximumLength(LargeTextMaxLength)

  val containsSurname = nonEmpty && matchesR("""^\s*?\S+?\s+?\S.*""".r)("should include a surname, please.")
}
