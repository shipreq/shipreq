package shipreq.webapp.feature.validation

import scalaz.{Success, Failure}
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.EmailAddr
import shipreq.webapp.base.AppConsts
import shipreq.webapp.base.TextMod._
import shipreq.webapp.base.validation._
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import shipreq.webapp.security.PasswordAndSalt
import Constraint.not
import Constraints._
import GenericValidators._

object Validators {

  val email_ = Validator(
    CorrectionPartU.endo(noWhitespace),
    ValidationPartU.forConstraint("Email address",
      maximumLength(AppConsts.emailMaxLength)
        + matchesR("^_+@_+?\\._+$".replace("_", "[^&<>]").r)("is invalid.") // loose validation
    ))

  val email = email_ map EmailAddr.apply

  val password = Validator(
    CorrectionPartU.nop[String],
    ValidationPartU.forConstraint("Password", lengthInRange(AppConsts.passwordLength) + containsAlphaAndNumber))

  val passwords = Validator(
    CorrectionPartU.liftE[(String, String)](_ umap password.correctU),
    ValidationPartU[(String, String), String](input =>
      password.validateU(input.map(_._1)) match {
        case f@ Failure(_) => f
        case s@ Success(_) =>
          if (input.value._1 != input.value._2)
            Failure(VFailure.looseMsg("Passwords don't match."))
          else
            s
      }))

  def currentPassword(ps: PasswordAndSalt) = Validator(
    password.cp,
    ValidationPartU[String, Unit](input =>
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
    CorrectionPartU.liftE[PasswordChange](_.map2(passwords.correctU)),
    ValidationPartU[PasswordChange, String](passwords validateU _.map(_._2)))

  val tosAgreement = Validator(
    CorrectionPartU.nop[Boolean],
    ValidationPartU.test[Boolean](_.value, VFailure.looseMsg("You must agree to the terms of service.")))

  val humanFullName = Validator(
    CorrectionPartU.endo(singleLineWhitespace),
    ValidationPartU.forConstraint("Your name",
      containsSurname
        + shortTextLimit
        + blacklistCharsS("<>\"[]{}%$@!;:|?*+_")("mustn't contain symbols.")
        + blacklistCharsR("0-9")("mustn't contain numbers.")
    ))

  // -------------------------------------------------------------------------------------------------------------------

  object user {

    val username_ = Validator(
      CorrectionPartU.endo(noWhitespace andThen lowerCase),
      ValidationPartU.forConstraint("Username",
        lengthInRange(AppConsts.usernameLength)
          + whitelistCharsR("a-z0-9_")("can only contain letters, numbers and underscores.")
          + startsWithR("[a-z]")("must start with a letter.")
          + endsWithR("[a-z0-9]")("must end with a letter or a number.")
      ))

    val username = username_ map Username.apply

    val usernameOrEmail = Validator.choose((i: String) => if (i.indexOf('@') == -1) username_ else email_)

    def name = humanFullName
  }

  // -------------------------------------------------------------------------------------------------------------------

  object project {
    val name = mandatoryShortText("Project name")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object usecase {

    val title = Validator(
      CorrectionPartU.endo(singleLineWhitespace andThen niceSymbols),
      ValidationPartU.forConstraint("Use case title",
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
    def email = Validators.email
    val msg = optionalLargeText("Your message")
  }

}
