package shipreq.webapp.server.feature.validation

import scalaz.{-\/, \/-}
import shipreq.base.util.Identity
import shipreq.base.util.univeq._
import shipreq.taskman.api.EmailAddr
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.base.validation.{CommonValidation => CV, _}
import Simple._
import Simple.Implicits._

object ServerSideValidators {

  private def emailPattern =
    "^_+@_+?\\._+$".replace("_", "[^&<>]").r.pattern // loose validation

  val email =
    TextMod.noWhitespace.correctFull
      .withInvalidator(CV.invalidator.maximumLength(WebappConfig.emailMaxLength))
      .addInvalidator(CV.invalidator.matchesRegex(emailPattern)(Invalidity("Invalid address.")))
      .toValidator
      .mapValid(EmailAddr.apply)
      .named("Email address")

  val password: Composite.Stateless[String, String, String] =
    CV.endoValidator.lengthInRange(WebappConfig.passwordLength)
      .addInvalidator(CV.invalidator.containsAlphaAndNumber)
      .toValidator
      .named("Password")

  def currentPasswordConfirmation(ps: PasswordAndSalt): Composite.Validator[String, String, Unit] =
    password.corrector
      .withAuditor(Auditor(input =>
        if (ps matches input)
          Auditor.unitResult
        else
          -\/(Invalidity("Current password is incorrect."))
      ))
      .loose

  /** i.e. [password] and [confirm password] */
  type PasswordTwice = (String, String)

  val passwordTwice: Composite.Validator[PasswordTwice, PasswordTwice, String] =
    password.corrector.pair
      .withAuditor(Auditor(inputs =>
        password.named.auditor(inputs._1).flatMap(s =>
          if (s ==* inputs._2)
            \/-(s)
          else
            -\/(Composite.Invalidity.loose("Passwords don't match."))
        )))

  /** (currentPassword, (newPassword, confirmNewPassword)) */
  type PasswordChange = (String, PasswordTwice)

  /** Output is the new password if successful */
  def passwordChange(ps: PasswordAndSalt): Composite.Validator[PasswordChange, PasswordChange, String] =
    (currentPasswordConfirmation(ps) tuple passwordTwice).mapValid(_._2)

  val tosAgreement: Composite.Validator[Boolean, Boolean, Boolean] =
    Invalidator.test(Identity[Boolean], Invalidity("You must agree to the terms of service."))
      .toValidator
      .loose

  val humanFullName =
    CV.endoCorrector.singleLineWhitespace
      .withInvalidator(
        CV.invalidator.nonEmpty.whenValid(
          CV.invalidator.shortTextLimit merge
          CV.invalidator.matchesRegex("""^\s*?\S+?\s+?\S.*""".r.pattern)(Invalidity("Include a surname, please.")) merge
          CV.invalidator.blacklistChars("<>\"[]{}%$@!;:|?*+_")(Invalidity("Mustn't contain symbols.")) merge
          CV.invalidator.blacklistCharRangeRegex("0-9")(Invalidity("Mustn't contain numbers."))
        )
      )

  val yourFullName: Composite.Stateless[String, String, String] =
    humanFullName.toValidator.named("Your name")

  // -------------------------------------------------------------------------------------------------------------------

  object user {

    val username: Composite.Stateless[String, String, Username] =
      CV.endoValidator.lengthInRange(WebappConfig.usernameLength)
        .prependCorrector(TextMod.noWhitespace.andThen(TextMod.lowerCase).correctLive)
        .addInvalidator(CV.invalidator.whitelistCharRangeRegex("a-z0-9_")(Invalidity("Can only contain letters, numbers and underscores.")))
        .addInvalidator(CV.invalidator.startsWithRegex("[a-z]")(Invalidity("Must start with a letter.")))
        .addInvalidator(CV.invalidator.endsWithRegex("[a-z0-9]")(Invalidity("Must end with a letter or a number.")))
        .toValidator
        .mapValid(Username.apply)
        .named("Username")

    val usernameOrEmail: Composite.Validator[String, String, String] = {
      val vu = username.named.mapValid(_.value)
      val ve = email.named.mapValid(_.value)
      Validator.choose(s => if (s.indexOf('@') == -1) vu else ve)
    }

    def name = yourFullName
  }

  // -------------------------------------------------------------------------------------------------------------------

  object landingPage {
    def name = yourFullName
    def email = ServerSideValidators.email
    val msg = CV.optionalLargeText.named("Your message")
  }

}
