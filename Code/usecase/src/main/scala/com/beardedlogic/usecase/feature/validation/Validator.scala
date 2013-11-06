package com.beardedlogic.usecase.feature.validation

import com.beardedlogic.usecase.app.AppConfig._
import com.beardedlogic.usecase.lib.ScalazSubset._
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.Misc._
import com.beardedlogic.usecase.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import scalaz.{Validation, Success, Failure}
import Constraints._

sealed trait Validator[I <: AnyRef, C <: AnyRef, V <: AnyRef] {

  def correct(input: I): C @@ InputCorrected

  def validate(input: C @@ InputCorrected): ValidationResult[V]

  final def correctAndValidate(input: I): ValidationResult[V] =
    validate(correct(input))

  final def isValid(input: C @@ InputCorrected): Boolean =
    validate(input).isSuccess
}

final object Validator {

  sealed trait InputValidatorV[T <: AnyRef] extends Validator[T, T, T] {
    protected def validator: ConstraintValidator[T]
    final override def validate(input: T @@ InputCorrected) = validator.validate(input)
  }

  sealed trait InputCorrectionOnly[I <: AnyRef, O <: AnyRef] extends Validator[I, O, O] {
    final override def validate(input: O @@ InputCorrected) = Success(input.tag)
  }

  sealed trait UnrestrictedMultiLineString extends InputCorrectionOnly[String, String] {
    override def correct(input: String) = correctMultiLineString(input)
  }
  private def correctMultiLineString(input: String) = normaliseCRLFs(input).trim.tag[InputCorrected]

  sealed trait OptionalMultiLineString extends InputCorrectionOnly[String, Option[String]] {
    override def correct(input: String) = nonEmptyString(correctMultiLineString(input)).tag
  }

  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  // -------------------------------------------------------------------------------------------------------------------

  object username
    extends InputValidatorV[String] {
    override def correct(input: String) = input.trim.toLowerCase.tag
    override protected val validator = ConstraintValidator[String]("Username",
      HasLengthInRange(UsernameLength),
      CharWhitelist.charRegex("a-z0-9_", "can only contain letters, numbers and underscores."),
      StartsWith.regex("[a-z]",          "must start with a letter."),
      EndsWith.regex("[a-z0-9]",         "must end with a letter or a number.")
    )
  }

  object email
    extends InputValidatorV[String] {
    override def correct(input: String) = removeAllWhitespace(input).tag
    override protected val validator = ConstraintValidator[String]("Email address",
      MatchesRegex("^_+@_+?\\._+$".replace("_", "[^&<>]").r, "is invalid.") // loose validation
    )
  }

  object usernameOrEmail
    extends Validator[String, String, String] {
    @inline private def underlying(input: String) = if (input.indexOf('@') == -1) username else email
    override def correct(input: String) = underlying(input).correct(input)
    override def validate(input: String @@ InputCorrected) = underlying(input).validate(input)
  }

  object password
    extends InputValidatorV[String] {
    override def correct(input: String) = input.tag
    override protected val validator = ConstraintValidator[String]("Password",
      HasLengthInRange(PasswordLength),
      ContainsAlphaAndNumber
    )
  }

  object passwords
    extends Validator[(String, String), (String, String), String] {
    override def correct(input: (String, String)) = input.umap(password.correct).tag
    override def validate(input: (String, String) @@ InputCorrected) = {
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

  object projectName
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = ConstraintValidator[String]("Project name", NonEmpty)
  }

  object useCaseTitle
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = ConstraintValidator[String]("Use case title",
      NonEmpty,
      CharBlacklist("[]⦋⦌［］", "cannot include square brackets."),
      Not(Contain.regex(AnyValidArrowRegexStr, "cannot include arrows."))
    )
  }

  object shareName
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = ConstraintValidator[String]("Share name", NonEmpty)
  }

  object sharePreface extends OptionalMultiLineString

  object textFieldText extends UnrestrictedMultiLineString

  object stepFieldText extends UnrestrictedMultiLineString
}
