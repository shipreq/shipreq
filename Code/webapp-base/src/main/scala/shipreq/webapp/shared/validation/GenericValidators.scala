package shipreq.webapp.shared.validation

import shipreq.webapp.shared.TextMod._
import shipreq.webapp.shared.validation.Constraints._

object GenericValidators {

  /** Empty string not allowed. Carriage returns removed. */
  def mandatoryShortText(name: String) = Validator(
    CorrectionPart.endo(singleLineWhitespace),
    ValidationPart.forConstraint(name, nonEmpty + shortTextLimit))

  def largeTextValidator(name: String) =
    ValidationPart.forConstraint(name, largeTextLimit)

  private def largeTextCP =
    CorrectionPart.endo(multiLineWhitespace andThen niceSymbols)

  /** Empty string is represented as `""`. */
  def largeText(name: String) =
    Validator(largeTextCP, largeTextValidator(name))

  /** Empty string is represented as `None`. */
  def optionalLargeText(name: String) = Validator(
    largeTextCP.map(nonBlank),
    ValidationPart.liftO[String, String](largeTextValidator(name).validate))
}
