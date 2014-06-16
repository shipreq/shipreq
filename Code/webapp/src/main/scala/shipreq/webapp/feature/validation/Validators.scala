package shipreq.webapp.feature.validation

import scalaz.{Success, Failure}
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.EmailAddr
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.TextMod._
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import shipreq.webapp.security.PasswordAndSalt
import Constraint.not
import Constraints._

object Validators {

  /** Empty string not allowed. Carriage returns removed. */
  private def mandatoryShortText(name: String) = Validator(
    CorrectionPart.endo(singleLineWhitespace),
    ValidationPart.forConstraint(name, nonEmpty + shortTextLimit))

  private val largeTextCP = CorrectionPart.endo(multiLineWhitespace andThen niceSymbols)
  private def largeTextValidator(name: String) = ValidationPart.forConstraint(name, largeTextLimit)

  /** Empty string is represented as `""`. */
  private def largeText(name: String) =
    Validator(largeTextCP, largeTextValidator(name))

  /** Empty string is represented as `None`. */
  private def optionalLargeText(name: String) = Validator(
    largeTextCP.mapQ[Option[String]](nonBlank),
    ValidationPart.liftO[String, String](largeTextValidator(name).validate))

  // ===================================================================================================================

  val email = Validator(
    CorrectionPart.endo(noWhitespace),
    ValidationPart.forConstraint("Email address",
      maximumLength(EmailMaxLength)
        + matchesR("^_+@_+?\\._+$".replace("_", "[^&<>]").r)("is invalid.") // loose validation
    ))

  val emailEA = email.map(EmailAddr)

  val password = Validator(
    CorrectionPart.nop[String],
    ValidationPart.forConstraint("Password", lengthInRange(PasswordLength) + containsAlphaAndNumber))

  val passwords = Validator(
    CorrectionPart.liftE[(String, String)](_ umap password.correctU),
    ValidationPart[(String, String), String](input =>
      password.validate(input.value._1) match {
        case f@ Failure(_) => f
        case s@ Success(_) =>
          if (input.value._1 != input.value._2)
            Failure(VFailure.looseMsg("Passwords don't match."))
          else
            s
      }))

  def currentPassword(ps: PasswordAndSalt) = Validator(
    password.cp,
    ValidationPart[String, Unit](input =>
      if (ps matches input.value)
        Success(())
      else
        Failure(VFailure.looseMsg("Current password is incorrect."))
    ))

  /** (currentPassword, (newPassword, confirmNewPassword)) */
  type PasswordChange = (String, (String, String))

  def passwordChange(ps: PasswordAndSalt) = (currentPassword(ps) *** passwords).map(_._2)

  /** `passwords` in the shape of `passwordChange`. i.e. change password without checking current. */
  val passwordSet = Validator(
    CorrectionPart.liftE[PasswordChange](_.map2(passwords.correctU)),
    ValidationPart[PasswordChange, String](passwords validate _.value._2))

  val tosAgreement = Validator(
    CorrectionPart.nop[Boolean],
    ValidationPart.test[Boolean](_.value, VFailure.looseMsg("You must agree to the terms of service.")))

  val humanFullName = Validator(
    CorrectionPart.endo(singleLineWhitespace),
    ValidationPart.forConstraint("Your name",
      containsSurname
        + shortTextLimit
        + blacklistCharsS("<>\"[]{}%$@!;:|?*+_")("mustn't contain symbols.")
        + blacklistCharsR("0-9")("mustn't contain numbers.")
    ))

  // -------------------------------------------------------------------------------------------------------------------

  object user {

    val username = Validator(
      CorrectionPart.endo(noWhitespace andThen lowerCase),
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
      CorrectionPart.endo(singleLineWhitespace andThen niceSymbols),
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
