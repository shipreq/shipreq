package shipreq.webapp.base.vali2

import java.util.regex.Pattern
import scalaz.{-\/, \/-}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.util.TextMod
import Simple._

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

    def matchesRegex(regex: Pattern): InvalidatorLogic[String] =
      Invalidator.logic(regex.matcher(_).matches)

    def startsWithRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s"^(?:$regex).*".r)

    def endsWithRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s".*(?:$regex)$$".r)

    /** @param rangeRegex Like "a-zA-Z". No brackets. */
    def whitelistCharRangeRegex(rangeRegex: String): InvalidatorLogic[String] =
      matchesRegex(s"^[$rangeRegex]*$$".r)

    //  def whitelistCharsS(charList: String) = whitelistCharsR(quote(charList))
    //
    //  def blacklistCharsR(charRegex: String) = matchesR(s"^[^$charRegex]*$$".r)
    //
    //  def blacklistCharsS(charList: String) = blacklistCharsR(quote(charList))

    def containsRegex(regex: String): InvalidatorLogic[String] =
      matchesRegex(s".*(?:$regex).*".r)

    /** Validates that a string contains at least one letter, and at least one number. */
    def containsAlphaAndNumber: Invalidator[String] =
      matchesRegex(".*?[A-Za-z].*?[0-9].*|.*?[0-9].*?[A-Za-z].*".r)(
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
      TextMod.truncateToLength(range).correctLive / invalidator.lengthInRange(range)

    def whitelistCharRangeRegex(regexRange: String, errMsg: Invalidity): EndoValidator[String] =
      TextMod.regexReplace(s"[^$regexRange]".r, "").correctLive /
        invalidator.whitelistCharRangeRegex(regexRange)(errMsg)
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
  val mandatoryShortText: EndoValidator[String] =
    singleLineWhitespace / nonEmpty.whenValid(shortTextLimit)

  /** See also [[optionalLargeText]] */
  val largeText: EndoValidator[String] =
    endoCorrector.largeText / largeTextLimit

  /** See also [[largeText]] */
  val optionalLargeText: Validator[String, Option[String], Option[String]] =
    endoCorrector.largeText.toCorrector.imapCorrected(TextMod.nonBlank) /
      largeTextLimit.liftOption.toAuditor

}
