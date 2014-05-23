package shipreq.webapp.feature.validation

import java.lang.{Boolean => JBool}
import scalaz.{Success, Failure}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.lib.Misc._
import shipreq.webapp.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import shipreq.webapp.security.PasswordAndSalt
import Constraint.not
import Constraints._

object Validators {

  /** Empty string not allowed. Carriage returns removed. */
  private def mandatoryShortText(name: String) = Validator(
    CorrectionPart.lift(normaliseWhitespaceInSingleLineString),
    ValidationPart.forConstraint(name, nonEmpty + shortTextLimit))

  private def correctLargeText(input: String): String =
    TextReplacements.perform(TextReplacements.GeneralWithWhitespace)(input)

  private def largeTextValidator(name: String) =
    ValidationPart.forConstraint(name, largeTextLimit)

  /** Empty string is represented as `""`. */
  private def largeText(name: String) =
    Validator(CorrectionPart.lift(correctLargeText), largeTextValidator(name))

  /** Empty string is represented as `None`. */
  private def optionalLargeText(name: String) = Validator(
    CorrectionPart[String, Option[String]](i => nonEmptyString(correctLargeText(i)).tag),
    ValidationPart.liftO[String, String](largeTextValidator(name).validate))

  // ===================================================================================================================

  val email = Validator(
    CorrectionPart.lift(removeAllWhitespace),
    ValidationPart.forConstraint("Email address",
      maximumLength(EmailMaxLength)
        + matchesR("^_+@_+?\\._+$".replace("_", "[^&<>]").r)("is invalid.") // loose validation
    ))

  val emailEA = email.map[EmailAddr]((s: String) => s.tag)

  val password = Validator(
    CorrectionPart.nop[String],
    ValidationPart.forConstraint("Password", lengthInRange(PasswordLength) + containsAlphaAndNumber))

  val passwords = Validator(
    CorrectionPart.lift[(String, String)](_ umap password.correct),
    ValidationPart[(String, String), String](input =>
      password.validate(input._1.tag) match {
        case f@ Failure(_) => f
        case s@ Success(_) =>
          if (input._1 != input._2)
            Failure(VFailure.looseMsg("Passwords don't match."))
          else
            s
      }))

  def currentPassword(ps: PasswordAndSalt) = Validator(
    password.cp,
    ValidationPart.untyped[String, Unit](input =>
      if (ps matches input)
        Success(())
      else
        Failure(VFailure.looseMsg("Current password is incorrect."))
    ))

  /** (currentPassword, (newPassword, confirmNewPassword)) */
  type PasswordChange = (String, (String, String))

  def passwordChange(ps: PasswordAndSalt) = (currentPassword(ps) &&& passwords).map(_._2)

  /** `passwords` in the shape of `passwordChange`. i.e. change password without checking current. */
  val passwordSet = Validator(
    CorrectionPart[PasswordChange, PasswordChange](_.map2(passwords.correct).tag),
    ValidationPart[PasswordChange, String](passwords validate _._2.tag))

  val tosAgreement = Validator(
    CorrectionPart[Boolean, JBool](JBool.valueOf(_).tag),
    ValidationPart.test[JBool](_.booleanValue, VFailure.looseMsg("You must agree to the terms of service.")))

  val humanFullName = Validator(
    CorrectionPart.lift(normaliseWhitespaceInSingleLineString),
    ValidationPart.forConstraint("Your name",
      containsSurname
        + shortTextLimit
        + blacklistCharsS("<>\"[]{}%$@!;:|?*+_")("mustn't contain symbols.")
        + blacklistCharsR("0-9")("mustn't contain numbers.")
    ))

  // -------------------------------------------------------------------------------------------------------------------

  object user {

    val username = Validator(
      CorrectionPart.lift[String](removeAllWhitespace(_).toLowerCase),
      ValidationPart.forConstraint("Username",
        lengthInRange(UsernameLength)
          + whitelistCharsR("a-z0-9_")("can only contain letters, numbers and underscores.")
          + startsWithR("[a-z]")("must start with a letter.")
          + endsWithR("[a-z0-9]")("must end with a letter or a number.")
      ))

    val usernameOrEmail = Validator.choose((i: String) => if (i.indexOf('@') == -1) username else email)

    def name = humanFullName
  }

  // -------------------------------------------------------------------------------------------------------------------

  object project {
    val name = mandatoryShortText("Project name")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object usecase {

    val title = Validator(
      CorrectionPart.lift[String](i =>
        TextReplacements.perform(TextReplacements.General)(normaliseWhitespaceInSingleLineString(i))),
      ValidationPart.forConstraint("Use case title",
        nonEmpty
          + shortTextLimit
          + blacklistCharsS("[]⦋⦌［］")("cannot include square brackets.")
          + not(containsR(AnyValidArrowRegexStr))("cannot include arrows.")
      ))

    val textFieldText = largeText("Text")
    val stepFieldText = largeText("Text")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object share {
    val name = mandatoryShortText("Share name")
    val preface = optionalLargeText("Preface")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object landingPage {
    def name = humanFullName
    def email = Validators.emailEA
    val msg = optionalLargeText("Your message")
  }

}
