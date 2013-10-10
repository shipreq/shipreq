package com.beardedlogic.usecase
package lib

import scalaz.\/
import Misc._
import Types._
import app.AppConfig._
import text.ParsingConfig.AnyValidArrowRegex
import util.Constraints._
import util.Validator

sealed trait InputValidator[T <: AnyRef] {
  def correct(input: T): T @@ InputCorrected
  def validate(input: T @@ InputCorrected): String \/ (T @@ Validated)
  final def correctAndValidate(input: T): String \/ (T @@ Validated) = validate(correct(input))
  final def isValid(input: T @@ InputCorrected) = validate(input).isRight
}

sealed trait InputValidatorV[T <: AnyRef] extends InputValidator[T] {
  protected def validator: Validator[T]
  final override def validate(input: T @@ InputCorrected): String \/ (T @@ Validated) = validator(input)
}

final object InputValidator {
  private implicit def autotag[T <: AnyRef](t: T): T @@ InputCorrected = t.tag[InputCorrected]

  // -------------------------------------------------------------------------------------------------------------------

  object username
    extends InputValidatorV[String] {
    override def correct(input: String) = input.trim.toLowerCase
    override protected val validator = Validator[String]("Username",
      HasLengthInRange(UsernameLength),
      CharWhitelist.charRegex("a-z0-9_", "can only contain letters, numbers and underscores."),
      StartsWith.regex("[a-z]",          "must start with a letter."),
      EndsWith.regex("[a-z0-9]",         "must end with a letter or a number.")
    )
  }

  object email
    extends InputValidatorV[String] {
    override def correct(input: String) = removeAllWhitespace(input)
    override protected val validator = Validator[String]("Email address",
      MatchesRegex("^_+@_+?\\._+$".replace("_", "[^&<>]").r, "is invalid.") // loose validation
    )
  }

  object usernameOrEmail
    extends InputValidator[String] {
    @inline private def underlying(input: String) = if (input.indexOf('@') == -1) username else email
    override def correct(input: String) = underlying(input).correct(input)
    override def validate(input: String @@ InputCorrected) = underlying(input).validate(input)
  }

  object password
    extends InputValidatorV[String] {
    override def correct(input: String) = input
    override protected val validator = Validator[String]("Password",
      HasLengthInRange(PasswordLength),
      ContainsAlphaAndNumber
    )
  }

  object projectName
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input)
    override protected val validator = Validator[String]("Project name", NonEmpty)
  }

  object useCaseTitle
    extends InputValidatorV[String] {
    override def correct(input: String) = normaliseWhitespaceInSingleLineString(input)
    override protected val validator = Validator[String]("Use case title",
      NonEmpty,
      CharBlacklist("[]⦋⦌［］",               "cannot include square brackets."),
      Not(Contain.regex(AnyValidArrowRegex, "cannot include arrows."))
    )
  }
}
