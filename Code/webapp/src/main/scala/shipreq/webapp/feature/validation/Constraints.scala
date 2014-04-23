package shipreq.webapp.feature.validation

import java.util.regex.Pattern
import shipreq.webapp.app.AppConfig._

trait Constraint[T <: AnyRef] extends (T => Option[String]) {
  def isValid(t: T): Boolean
}

trait ConstraintWithStaticFailureResult[T <: AnyRef] extends Constraint[T] {
  final def apply(t: T): Option[String] = if (isValid(t)) None else failureResult
  def isValid(t: T): Boolean
  val failureResult: Some[String]
}

final object Constraints {
  implicit def RegexToPattern(regex: scala.util.matching.Regex): Pattern = regex.pattern
  private def CharStrToCharRegex(s: String): String = s.toCharArray.map(c=>Pattern.quote(c.toString)).mkString

  // -------------------------------------------------------------------------------------------------------------------
  // Combinators

  case class Not[T <: AnyRef](v: Constraint[T], errMsg: String) extends ConstraintWithStaticFailureResult[T] {
    override def isValid(t: T) = !v.isValid(t)
    override val failureResult = Some(errMsg)
  }

  case class And[T <: AnyRef](a: Constraint[T], b: Constraint[T], errMsg: String) extends ConstraintWithStaticFailureResult[T] {
    override def isValid(t: T) = a.isValid(t) && b.isValid(t)
    override val failureResult = Some(errMsg)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Instances

  case class MatchesRegex(regex: Pattern, errMsg: String) extends ConstraintWithStaticFailureResult[String] {
    override def isValid(input: String) = regex.matcher(input).matches
    override val failureResult = Some(errMsg)
  }

  object StartsWith {
    def regex(regex: String, errMsg: String) = MatchesRegex(s"^(?:$regex).*".r.pattern, errMsg)
  }

  object EndsWith {
    def regex(regex: String, errMsg: String) = MatchesRegex(s".*(?:$regex)$$".r.pattern, errMsg)
  }

  object Whitelist {
    /** Validates that a string consists only of certain chars. */
    def chars(chars: String, errMsg: String) = charRegex(CharStrToCharRegex(chars), errMsg)

    /** Validates that a string consists only of certain chars. */
    def charRegex(charRegex: String, errMsg: String) = MatchesRegex(s"^[$charRegex]*$$".r.pattern, errMsg)
  }

  object Blacklist {
    /** Validates that a string doesn't contain any prohibited chars. */
    def chars(chars: String, errMsg: String) = charRegex(CharStrToCharRegex(chars), errMsg)

    /** Validates that a string doesn't contain any prohibited chars. */
    def charRegex(charRegex: String, errMsg: String) = Not(Contain.regex(s"[$charRegex]", ""), errMsg)
  }

  /** Validates that a string contains a certain pattern or substring. */
  object Contains {
    def regex(regex: String, errMsg: String) = MatchesRegex(s".*$regex.*".r.pattern, errMsg)
  }
  def Contain = Contains

  /** Validates that a string contains at least one letter, and at least one number. */
  object ContainsAlphaAndNumber extends MatchesRegex(
    ".*?[A-Za-z].*?[0-9].*|.*?[0-9].*?[A-Za-z].*".r.pattern,
    "must contain at least one letter, and at least one number.")

  /**
   * Validates that the length of a string is within min & max bounds.
   *
   * @param range inclusive
   */
  case class HasLengthInRange(range: Range) extends ConstraintWithStaticFailureResult[String] {
    override def isValid(input: String) = range.contains(input.length)
    override val failureResult = Some(s"must be between ${range.min} and ${range.max} characters long.")
  }

  object NonEmpty extends ConstraintWithStaticFailureResult[String] {
    override def isValid(input: String) = input.nonEmpty
    override val failureResult = Some("cannot be blank.")
  }

  case class HasMaximumLength(maxLength: Int) extends Constraint[String] {
    override def apply(input: String) = {
      val excess = input.length - maxLength
      if (excess > 0)
        Some(s"is too large by $excess characters.")
      else
        None
    }
    override def isValid(input: String) = input.length <= maxLength
  }

  object HasShortTextLimit extends HasMaximumLength(ShortTextMaxLength)

  object HasLargeTextLimit extends HasMaximumLength(LargeTextMaxLength)

  object IsNotAFirstNameOnly extends ConstraintWithStaticFailureResult[String] {
    private val regex = "^\\s*\\S+\\s*$".r.pattern
    override def isValid(input: String) = !regex.matcher(input).matches
    override val failureResult = Some("should include a surname, please.")
  }
}
