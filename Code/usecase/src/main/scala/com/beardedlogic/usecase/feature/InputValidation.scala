package com.beardedlogic.usecase
package feature

import scalaz.{\/, -\/, \/-}
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._

import app.AppConfig._
import lib.Misc._
import lib.Types._
import feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import util.Constraints._
import util.Validator

sealed trait InputValidator[I <: AnyRef, O <: AnyRef] {
  def correct(input: I): I @@ InputCorrected
  def validate(input: I @@ InputCorrected): String \/ (O @@ Validated)
  final def correctAndValidate(input: I): String \/ (O @@ Validated) = validate(correct(input))
  final def isValid(input: I @@ InputCorrected) = validate(input).isRight
}

sealed trait InputValidatorV[T <: AnyRef] extends InputValidator[T, T] {
  protected def validator: Validator[T]
  final override def validate(input: T @@ InputCorrected) = validator(input)
}

sealed trait InputCorrectionOnly[T <: AnyRef] extends InputValidator[T, T] {
  final override def validate(input: T @@ InputCorrected) = \/-(input.tag)
}

sealed trait unrestrictedMultiLineString extends InputCorrectionOnly[String] {
  override def correct(input: String) = normaliseCRLFs(input).trim.tag
}

final object InputValidator {

  // -------------------------------------------------------------------------------------------------------------------

  object username
    extends InputValidatorV[String] {
    override def correct(input: String) = input.trim.toLowerCase.tag
    override protected val validator = Validator[String]("Username",
      HasLengthInRange(UsernameLength),
      CharWhitelist.charRegex("a-z0-9_", "can only contain letters, numbers and underscores."),
      StartsWith.regex("[a-z]",          "must start with a letter."),
      EndsWith.regex("[a-z0-9]",         "must end with a letter or a number.")
    )
  }

  object email
    extends InputValidatorV[String] {
    override def correct(input: String) = removeAllWhitespace(input).tag
    override protected val validator = Validator[String]("Email address",
      MatchesRegex("^_+@_+?\\._+$".replace("_", "[^&<>]").r, "is invalid.") // loose validation
    )
  }

  object usernameOrEmail
    extends InputValidator[String, String] {
    @inline private def underlying(input: String) = if (input.indexOf('@') == -1) username else email
    override def correct(input: String) = underlying(input).correct(input)
    override def validate(input: String @@ InputCorrected) = underlying(input).validate(input)
  }

  object password
    extends InputValidatorV[String] {
    override def correct(input: String) = input.tag
    override protected val validator = Validator[String]("Password",
      HasLengthInRange(PasswordLength),
      ContainsAlphaAndNumber
    )
  }

  object passwords
    extends InputValidator[(String, String), String] {
    override def correct(input: (String, String)) = input.umap(password.correct).tag
    override def validate(input: (String, String) @@ InputCorrected) = {
      password.validate(input._1.tag) match {
        case err@ -\/(_) => err
        case \/-(_) =>
          if (input._1 != input._2)
            -\/("Passwords don't match.")
          else
            \/-(input._1.tag)
      }
    }
  }

  object projectName
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = Validator[String]("Project name", NonEmpty)
  }

  object useCaseTitle
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = Validator[String]("Use case title",
      NonEmpty,
      CharBlacklist("[]⦋⦌［］", "cannot include square brackets."),
      Not(Contain.regex(AnyValidArrowRegexStr, "cannot include arrows."))
    )
  }

  object shareName
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input).tag
    override protected val validator = Validator[String]("Share name", NonEmpty)
  }

  object sharePreface extends unrestrictedMultiLineString

  object textFieldText extends unrestrictedMultiLineString

  object stepFieldText extends unrestrictedMultiLineString
}
