package shipreq.webapp.feature.validation

import java.lang.{Boolean => JBool}
import scalaz.{Success, Failure}
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.lib.Misc._
import shipreq.webapp.feature.uc.text.ParsingConfig.AnyValidArrowRegexStr
import shipreq.webapp.security.PasswordAndSalt
import Constraint.not
import Constraints._
import Validator._

object Validators {

  def Ap = Validator.Ap

  /** Empty string not allowed. Carriage returns removed. */
  abstract class MandatoryShortText(name: String)
    extends UseConstraintValidator(ConstraintValidator[String](name, nonEmpty + shortTextLimit)) {
    override def correct(input: String): CI = normaliseWhitespaceInSingleLineString(input).tag
  }

  private def correctLargeText(input: String) =
    TextReplacements.perform(TextReplacements.GeneralWithWhitespace)(input)

  private def largeTextValidator(name: String) =
    ConstraintValidator[String](name, largeTextLimit)

  /** Empty string is represented as `""`. */
  abstract class LargeText(name: String)
    extends UseConstraintValidator(largeTextValidator(name)) {
    override def correct(input: String): CI = correctLargeText(input).tag
  }

  /** Empty string is represented as `None`. */
  abstract class LargeTextO(name: String) extends ValidatorT[String, Option[String], Option[String]] {

    override def correct(input: String): CI =
      nonEmptyString(correctLargeText(input)).tag

    val validator =
      largeTextValidator(name)

    override def validate(input: CI) = (input: Option[String]) match {
      case None    => Success(input.tag)
      case Some(i) => validator.validate(i.tag).map(s => Some(s).tag)
    }
  }

  // ===================================================================================================================

  object email
    extends Typical[String](
      removeAllWhitespace(_).tag,
      ConstraintValidator("Email address",
        maximumLength(EmailMaxLength)
          + matchesR("^_+@_+?\\._+$".replace("_", "[^&<>]").r)("is invalid.") // loose validation
      )
    )

  val emailEA = email.map[EmailAddr]((s: String) => s.tag)

  object password
    extends UseConstraintValidator[String](
      ConstraintValidator("Password",
        lengthInRange(PasswordLength)
          + containsAlphaAndNumber)
    ) with NoInputCorrection[String]

  object passwords
    extends ValidatorT[(String, String), (String, String), String] {
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

  def currentPassword(ps: PasswordAndSalt): Validator[String, String, Unit] =
    new Validator[String, String, Unit] {
      override def correct(input: String): CI =
        password.correct(input)
      override def validate(input: CI) =
        if (ps.matches(input))
          Success(())
        else
          Failure(VFailure.looseMsg("Current password is incorrect."))
    }

  type PasswordChangeIn = (String, (String, String))
  def passwordChange(ps: PasswordAndSalt): ValidatorT[PasswordChangeIn, PasswordChangeIn, String] = {
    val cur = currentPassword(ps)
    new ValidatorT[PasswordChangeIn, PasswordChangeIn, String] {
      override def correct(i: PasswordChangeIn): CI =
        (cur correct i._1, passwords correct i._2).tag
      override def validate(i: CI) =
        Validators.Ap.apply2(cur validate i._1.tag, passwords validate i._2.tag)((_,n) => n)
    }
  }

  /** `passwords` in the shape of `passwordChange`. i.e. change password without checking current. */
  object passwordSet
    extends ValidatorT[PasswordChangeIn, PasswordChangeIn, String] {
      override def correct(i: PasswordChangeIn): CI = (i._1, passwords correct i._2).tag
      override def validate(i: CI) = passwords validate i._2.tag
    }

  object tosAgreement extends ValidatorT[Boolean, JBool, JBool] {
    override def correct(i: Boolean): CI = JBool.valueOf(i).tag
    override def validate(b: CI) =
      if (b.booleanValue)
        Success(b.tag)
      else
        Failure(VFailure.looseMsg("You must agree to the terms of service."))
  }

  object humanFullName extends Typical[String](
    normaliseWhitespaceInSingleLineString(_).tag,
    ConstraintValidator("Your name",
      containsSurname
        + shortTextLimit
        + blacklistCharsS("<>\"[]{}%$@!;:|?*+_")("mustn't contain symbols.")
        + blacklistCharsR("0-9")("mustn't contain numbers.")
    )
  )

  // -------------------------------------------------------------------------------------------------------------------

  object user {

    object username extends Typical[String](
      removeAllWhitespace(_).toLowerCase.tag,
      ConstraintValidator("Username",
        lengthInRange(UsernameLength)
          + whitelistCharsR("a-z0-9_")("can only contain letters, numbers and underscores.")
          + startsWithR("[a-z]")("must start with a letter.")
          + endsWithR("[a-z0-9]")("must end with a letter or a number.")
      )
    )

    object usernameOrEmail
      extends Validator[String, String, String] {
      @inline private def underlying(input: String) = if (input.indexOf('@') == -1) username else email
      override def correct(input: String): CI = underlying(input).correct(input)
      override def validate(input: CI) = underlying(input).validate(input)
    }

    def name = humanFullName
  }

  // -------------------------------------------------------------------------------------------------------------------

  object project {
    object name extends MandatoryShortText("Project name")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object usecase {

    object title extends Typical[String](
      i => TextReplacements.perform(TextReplacements.General)(normaliseWhitespaceInSingleLineString(i)).tag,
      ConstraintValidator("Use case title",
        nonEmpty
          + shortTextLimit
          + blacklistCharsS("[]⦋⦌［］")("cannot include square brackets.")
          + not(containsR(AnyValidArrowRegexStr))("cannot include arrows.")
      )
    )

    object textFieldText extends LargeText("Text")
    object stepFieldText extends LargeText("Text")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object share {
    object name extends MandatoryShortText("Share name")
    object preface extends LargeTextO("Preface")
  }

  // -------------------------------------------------------------------------------------------------------------------

  object landingPage {
    def name = humanFullName
    def email = Validators.emailEA
    object msg extends LargeTextO("Your message")
  }
  
}
