package shipreq.webapp.feature.validation

import java.lang.{Boolean => JBool}
import scalaz.{Validation, Success, Failure}
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.lib.Misc._
import shipreq.webapp.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import shipreq.webapp.security.PasswordAndSalt
import Constraints._

sealed trait Validator[I <: AnyRef, C <: AnyRef, V <: AnyRef] {

  type CI = C @@ InputCorrected

  def correct(input: I): CI

  def validate(input: CI): ValidationResult[V]

  final def correctAndValidate(input: I): ValidationResult[V] =
    validate(correct(input))

  final def isValid(input: CI): Boolean =
    validate(input).isSuccess
}

final object Validator {

  sealed trait InputValidatorV[T <: AnyRef] extends Validator[T, T, T] {
    protected def validator: ConstraintValidator[T]
    final override def validate(input: CI) = validator.validate(input)
  }

  sealed trait InputCorrectionOnly[I <: AnyRef, O <: AnyRef] extends Validator[I, O, O] {
    final override def validate(input: CI) = Success(input.tag)
  }

  sealed trait ValidationOnly[T <: AnyRef] extends Validator[T, T, T] {
    final override def correct(input: T): CI = input.tag
  }

  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  // -------------------------------------------------------------------------------------------------------------------

  /** Empty string not allowed. Carriage returns removed. */
  sealed abstract class MandatoryShortText(name: String) extends InputValidatorV[String] {
    override def correct(input: String): CI = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = ConstraintValidator[String](name, NonEmpty, HasShortTextLimit)
  }

  private def correctLargeText(input: String) =
    TextReplacements.perform(TextReplacements.GeneralWithWhitespace)(input)

  private def largeTextValidator(name: String) =
    ConstraintValidator[String](name, HasLargeTextLimit)

  /** Empty string is represented as `""`. */
  sealed abstract class LargeText(name: String) extends InputValidatorV[String] {
    override def correct(input: String): CI = correctLargeText(input).tag
    override val validator = largeTextValidator(name)
  }

  /** Empty string is represented as `None`. */
  sealed abstract class LargeTextO(name: String) extends Validator[String, Option[String], Option[String]] {
    override def correct(input: String): CI = nonEmptyString(correctLargeText(input)).tag
    val validator = largeTextValidator(name)
    override def validate(input: CI) = (input: Option[String]) match {
      case None    => Success(input.tag)
      case Some(i) => validator.validate(i.tag).map(s => Some(s).tag)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  object username
    extends InputValidatorV[String] {
    override def correct(input: String): CI = removeAllWhitespace(input).toLowerCase.tag
    override protected val validator = ConstraintValidator[String]("Username",
      HasLengthInRange(UsernameLength),
      Whitelist.charRegex("a-z0-9_", "can only contain letters, numbers and underscores."),
      StartsWith.regex("[a-z]",      "must start with a letter."),
      EndsWith.regex("[a-z0-9]",     "must end with a letter or a number.")
    )
  }

  object email
    extends InputValidatorV[String] {
    override def correct(input: String): CI = removeAllWhitespace(input).tag
    override protected val validator = ConstraintValidator[String]("Email address",
      HasMaximumLength(EmailMaxLength),
      MatchesRegex("^_+@_+?\\._+$".replace("_", "[^&<>]").r, "is invalid.") // loose validation
    )
  }

  object usernameOrEmail
    extends Validator[String, String, String] {
    @inline private def underlying(input: String) = if (input.indexOf('@') == -1) username else email
    override def correct(input: String): CI = underlying(input).correct(input)
    override def validate(input: CI) = underlying(input).validate(input)
  }

  object password
    extends InputValidatorV[String] {
    override def correct(input: String): CI = input.tag
    override protected val validator = ConstraintValidator[String]("Password",
      HasLengthInRange(PasswordLength),
      ContainsAlphaAndNumber
    )
  }

  object passwords
    extends Validator[(String, String), (String, String), String] {
    override def correct(input: (String, String)): CI = input.umap(password.correct).tag
    override def validate(input: CI) = {
      password.validate(input._1.tag) match {
        case f@ Failure(_) => f
        case s@ Success(_) =>
          if (input._1 != input._2)
            Failure(VFailure.looseMsg("Passwords don't match."))
          else
            s
      }
    }
  }

  def currentPassword(ps: PasswordAndSalt): Validator[String, String, Unit2] = new Validator[String, String, Unit2] {
    override def correct(input: String): CI = password.correct(input)
    override def validate(input: CI) =
      if (ps.matches(input))
        Success(Unit2.tag)
      else
        Failure(VFailure.looseMsg("Current password is incorrect."))
  }

  object tosAgreement extends ValidationOnly[JBool] {
    override def validate(b: CI) =
      if (b.booleanValue)
        Success(b.tag)
      else
        Failure(VFailure.looseMsg("You must agree to the terms of service."))
  }

  object projectName extends MandatoryShortText("Project name")

  object useCaseTitle
    extends InputValidatorV[String] {
    override def correct(input: String): CI =
      TextReplacements.perform(TextReplacements.General)(normaliseWhitespaceInSingleLineString(input)).tag
    override protected val validator = ConstraintValidator[String]("Use case title",
      NonEmpty,
      HasShortTextLimit,
      Blacklist.chars("[]⦋⦌［］", "cannot include square brackets."),
      Not(Contain.regex(AnyValidArrowRegexStr, ""), "cannot include arrows.")
    )
  }

  object shareName extends MandatoryShortText("Share name")

  object sharePreface extends LargeTextO("Preface")

  object textFieldText extends LargeText("Text")

  object stepFieldText extends LargeText("Text")

  object landingPageName extends InputValidatorV[String] {
    override def correct(input: String): CI = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = ConstraintValidator[String]("Your name",
      NonEmpty,
      IsNotAFirstNameOnly,
      HasShortTextLimit,
      Not(Contain.regex("[0-9]", ""), "has numbers in it? I don't believe you.")
    )
  }
  def landingPageEmail = email
  object landingPageMsg extends LargeTextO("Your message")
}
