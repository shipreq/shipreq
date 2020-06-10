package shipreq.webapp.base.validation

import java.util.regex.Pattern
import scalaz.{-\/, \/-}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.validation.Simple._

object CommonValidation {
  private implicit def regexToPattern(regex: scala.util.matching.Regex): Pattern = regex.pattern

  private def invalidBecauseEmpty = Invalidity("Cannot be blank.")

  // ===================================================================================================================
  object endoCorrector {

    val singleLineWhitespace: EndoCorrector[String] =
      TextMod.onlyAllowSpacesAsWhitespace.correctLive append TextMod.singleLineWhitespace.correctFull

    val multiLineWhitespace: EndoCorrector[String] =
      TextMod.onlyAllowSpacesAndNewlinesAsWhitespace.correctLive append TextMod.multiLineWhitespace.correctFull

    val largeText: EndoCorrector[String] =
      multiLineWhitespace append TextMod.niceSymbols.correctFull
  }

  // ===================================================================================================================
  object invalidator {

    val nonEmpty: Invalidator[String] =
      Invalidator.test(_.nonEmpty, invalidBecauseEmpty)

    def nonEmptyVector[A]: Invalidator[Vector[A]] =
      Invalidator.test(_.nonEmpty, invalidBecauseEmpty)

    def matchesRegex(regex: Pattern): InvalidatorLogic[String] =
      Invalidator.logic(regex.matcher(_).matches)

    def startsWithRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s"^(?:$regex).*".r)

    def endsWithRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s".*(?:$regex)$$".r)

    /** @param rangeRegex Like "a-zA-Z". No brackets. */
    def whitelistCharRangeRegex(rangeRegex: String): InvalidatorLogic[String] =
      matchesRegex(s"^[$rangeRegex]*$$".r)

    /** @param rangeRegex Like "a-zA-Z". No brackets. */
    def blacklistCharRangeRegex(rangeRegex: String): InvalidatorLogic[String] =
      matchesRegex(s"^[^$rangeRegex]*$$".r)

    /** @param chars Like "0123456789" */
    def whitelistChars(chars: String): InvalidatorLogic[String] =
      whitelistCharRangeRegex(Pattern.quote(chars))

    /** @param chars Like "0123456789" */
    def blacklistChars(chars: String): InvalidatorLogic[String] =
      blacklistCharRangeRegex(Pattern.quote(chars))

    def blacklistChars(isBlacklisted: Char => Boolean): InvalidatorLogic[String] =
      Invalidator.logic(_.forall(!isBlacklisted(_)))

    def containsRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s".*(?:$regex).*".r)

    /** Validates that a string contains at least one letter. */
    def containsAlpha: Invalidator[String] =
      containsRegex("[A-Za-z]")(
        Invalidity("Must contain at least one letter."))

    /** Validates that a string contains at least one letter, and at least one number. */
    def containsAlphaAndNumber: Invalidator[String] =
      containsRegex("[A-Za-z].*[0-9]|[0-9].*[A-Za-z]")(
        Invalidity("Must contain at least one letter, and at least one number."))

    /**
      * Validates that the length of a string is within min & max bounds.
      *
      * @param range inclusive
      */
    def lengthInRange(range: Range): Invalidator[String] =
      Invalidator.test(range contains _.length,
        Invalidity(s"Must be between ${range.min} and ${range.max} characters long."))

    def maximumLength(max: Int): Invalidator[String] =
      Invalidator.testDyn[String](
        _.length <= max,
        s => Invalidity(s"Too large by ${s.length - max} characters."))

    val shortTextLimit: Invalidator[String] =
      maximumLength(WebappConfig.shortTextMaxLength)

    val largeTextLimit: Invalidator[String] =
      maximumLength(WebappConfig.largeTextMaxLength)

    //  val containsSurname = nonEmpty >> matchesR("""^\s*?\S+?\s+?\S.*""".r)("should include a surname, please.")

    def startsWithUpper: Invalidator[String] =
      startsWithRegex("[A-Z]")(Invalidity("Must start with a capital letter."))

    def startsWithAlpha: Invalidator[String] =
      startsWithRegex("[A-Za-z]")(Invalidity("Must start with a letter."))

    def startsWithAlphaNumeric: Invalidator[String] =
      startsWithRegex("[A-Za-z0-9]")(Invalidity("Must start with a letter or number."))

    def endsWithAlpha: Invalidator[String] =
      endsWithRegex("[A-Za-z]")(Invalidity("Must end with a letter."))

    def endsWithAlphaNumeric: Invalidator[String] =
      endsWithRegex("[A-Za-z0-9]")(Invalidity("Must end with a letter or number."))
  }

  // ===================================================================================================================
  object endoValidator {

    def lengthInRange(range: Range.Inclusive): EndoValidator[String] =
      TextMod.truncateToLength(range).correctLive withInvalidator invalidator.lengthInRange(range)

    def whitelistCharRangeRegex(regexRange: String, errMsg: Invalidity): EndoValidator[String] =
      TextMod.regexReplace(s"[^$regexRange]".r, "").correctLive withInvalidator
        invalidator.whitelistCharRangeRegex(regexRange)(errMsg)

    def blacklistCharRangeRegex(regexRange: String, errMsg: Invalidity): EndoValidator[String] =
      TextMod.regexReplace(s"[$regexRange]".r, "").correctLive withInvalidator
        invalidator.blacklistCharRangeRegex(regexRange)(errMsg)

    def blacklistChars(isBlacklisted: Char => Boolean, errMsg: Invalidity): EndoValidator[String] =
      TextMod.blacklistChars(isBlacklisted).correctLive withInvalidator
        invalidator.blacklistChars(isBlacklisted)(errMsg)
  }

  // ===================================================================================================================
  object auditor {

    def optionDefined[A]: Auditor[Option[A], A] =
      Auditor {
        case Some(a) => \/-(a)
        case None    => -\/(invalidBecauseEmpty)
      }
  }

  // ===================================================================================================================
  // High-level

  import endoCorrector._
  import invalidator._

  /** Empty string not allowed. Carriage returns removed. */
  lazy val mandatoryShortText: EndoValidator[String] =
    singleLineWhitespace withInvalidator nonEmpty.whenValid(shortTextLimit)

  /** Empty string allowed. See also [[optionalLargeText]] */
  lazy val largeText: EndoValidator[String] =
    endoCorrector.largeText withInvalidator largeTextLimit

  /** See also [[largeText]] */
  lazy val optionalLargeText: Validator[String, Option[String], Option[String]] =
    endoCorrector.largeText.toCorrector.imapCorrectedZ(TextMod.nonBlank)
    .withAuditor(largeTextLimit.liftOption.toAuditor)

  def option[A]: Validator[Option[A], Option[A], A] =
    Simple.Validator.option[A, Invalidity](invalidBecauseEmpty)
}
