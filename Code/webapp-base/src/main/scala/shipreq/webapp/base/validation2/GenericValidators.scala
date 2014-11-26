package shipreq.webapp.base.validation2

import shipreq.webapp.base.TextMod._
import shipreq.webapp.base.validation2.Constraints._

object GenericValidators {

  /** Empty string not allowed. Carriage returns removed. */
  def mandatoryShortText(name: String): ValidatorU[String, String, String] =
    Validator(
      CorrectionPart.endo(singleLineWhitespace),
      ValidationPart.forConstraint(name, nonEmpty + shortTextLimit))

  def largeTextValidator(name: String): ValidationPartU[String, String] =
    ValidationPart.forConstraint(name, largeTextLimit)

  private def largeTextCP: CorrectionPartU[String, String] =
    CorrectionPart.endo(multiLineWhitespace andThen niceSymbols)

  /** Empty string is represented as `""`. */
  def largeText(name: String): ValidatorU[String, String, String] =
    Validator(largeTextCP, largeTextValidator(name))

  /** Empty string is represented as `None`. */
  def optionalLargeText(name: String): ValidatorU[String, Option[String], Option[String]] =
    Validator(
      largeTextCP.imapC(nonBlank),
      largeTextValidator(name).liftO)
}
