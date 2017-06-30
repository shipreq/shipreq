package shipreq.webapp.base.user

import scalaz.{-\/, \/, \/-}
import shipreq.base.util.univeq._
import shipreq.webapp.base.{CommmonUiText, WebappConfig}
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.{CommonValidation => CV, _}
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.validation.Implicits._

object UserValidators {

  private def emailPattern =
    "^_+@_+?\\._+$".replace("_", "[^&<>]").r.pattern // loose validation

  val emailAddr: Composite.Stateless[String, String, EmailAddr] =
    TextMod.noWhitespace.correctFull
      .withInvalidator(CV.invalidator.maximumLength(WebappConfig.emailMaxLength))
      .addInvalidator(CV.invalidator.matchesRegex(emailPattern)(Invalidity("Invalid.")))
      .toValidator
      .mapValid(EmailAddr.apply)
      .named(CommmonUiText.emailAddr)

  val password: Composite.Stateless[String, String, String] =
    CV.endoValidator.lengthInRange(WebappConfig.passwordLength)
      .addInvalidator(CV.invalidator.containsAlphaAndNumber)
      .toValidator
      .named(CommmonUiText.password)

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

  /** Returns true when arg matches current */
  type CurrentPasswordTest = String => Boolean

  private def currentPassword(matchesCurrent: CurrentPasswordTest): Composite.Validator[String, String, Unit] =
    password.corrector
      .withAuditor(Auditor(input =>
        if (matchesCurrent(input))
          Auditor.unitResult
        else
          -\/(Invalidity(CommmonUiText.currentPassword + " is incorrect."))
      ))
      .loose

  /** (currentPassword, (newPassword, confirmNewPassword)) */
  type PasswordChange = (String, PasswordTwice)

  /** Successful output = new password */
  def passwordChange(matchesCurrent: CurrentPasswordTest): Composite.Validator[PasswordChange, PasswordChange, String] =
    (currentPassword(matchesCurrent) tuple passwordTwice).mapValid(_._2)

  val username: Composite.Stateless[String, String, Username] =
    CV.endoValidator.lengthInRange(WebappConfig.usernameLength)
      .prependCorrector(TextMod.noWhitespace.andThen(TextMod.lowerCase).correctLive)
      .addInvalidator(CV.invalidator.whitelistCharRangeRegex("a-z0-9_")(Invalidity("Can only contain letters, numbers and underscores.")))
      .addInvalidator(CV.invalidator.startsWithRegex("[a-z]")(Invalidity("Must start with a letter.")))
      .addInvalidator(CV.invalidator.endsWithRegex("[a-z0-9]")(Invalidity("Must end with a letter or a number.")))
      .toValidator
      .mapValid(Username.apply)
      .named(CommmonUiText.username)

  val usernameOrEmail: Composite.Validator[String, String, Username \/ EmailAddr] = {
    type R = Username \/ EmailAddr
    val vu = username.named.mapValid(-\/(_): R)
    val ve = emailAddr.named.mapValid(\/-(_): R)
    Validator.choose(s => if (s.indexOf('@') == -1) vu else ve)
  }

  val personName: Validator[String, String, PersonName] =
    CV.endoCorrector.singleLineWhitespace
      .withInvalidator(
        CV.invalidator.nonEmpty.whenValid(
          CV.invalidator.shortTextLimit))
      .toValidator
      .mapValid(PersonName.apply)
}
