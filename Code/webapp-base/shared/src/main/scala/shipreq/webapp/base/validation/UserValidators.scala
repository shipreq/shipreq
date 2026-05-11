package shipreq.webapp.base.validation

import shipreq.base.util.{Invalid, Validity}
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.ui.CommmonUiText
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.lib.Implicits._
import shipreq.webapp.base.validation.lib.Simple._
import shipreq.webapp.base.validation.lib.{CommonValidation => CV, _}

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

  val password: Composite.Stateless[String, String, PlainTextPassword] =
    CV.endoValidator.lengthInRange(WebappConfig.passwordLength)
      .mapInvalidator(_.whenValid(CV.invalidator.containsAlphaAndNumber))
      .toValidator
      .mapValid(PlainTextPassword.apply)
      .named(CommmonUiText.password)

  def password2(password1: String, whenEmpty: Validity = Invalid): Simple.Validator[String, String, Unit] =
    password.corrector.withAuditor(
      Auditor(s =>
        if (s !=* password1) -\/(Invalidity("Doesn't match."))
        else if (whenEmpty.is(Invalid)) CV.invalidator.nonEmpty.audit(s).map(_ => ())
        else \/-(())))

  /** i.e. [password] and [confirm password] */
  type PasswordTwice = (String, String)

  val passwordTwice: Composite.Validator[PasswordTwice, PasswordTwice, PlainTextPassword] =
    password.corrector.pair
      .withAuditor(Auditor(inputs =>
        password.named.auditor(inputs._1).flatMap(s =>
          if (s.value ==* inputs._2)
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
  def passwordChange(matchesCurrent: CurrentPasswordTest): Composite.Validator[PasswordChange, PasswordChange, PlainTextPassword] =
    (currentPassword(matchesCurrent) tuple passwordTwice).mapValid(_._2)

  val usernameBlacklist: Set[String] = {
    val b = Set.newBuilder[String]
    def addWithPlural(s: String) = {
      b += s
      b += s + "s"
    }
    addWithPlural("admin")
    addWithPlural("author")
    addWithPlural("collaborator")
    addWithPlural("contributor")
    addWithPlural("creator")
    addWithPlural("group")
    addWithPlural("owner")
    addWithPlural("project")
    addWithPlural("subscriber")
    addWithPlural("team")
    addWithPlural("watcher")
    b += "davidbarri"
    b += "help"
    b += "helpstaff"
    b += "staff"
    b += "support"
    b += "supportstaff"
    b.result()
  }

  val username: Composite.Stateful[Set[Username], String, String, Username] =
    CV.endoValidator.lengthInRange(WebappConfig.usernameLength)
      .prependCorrector(TextMod.noWhitespace.andThen(TextMod.lowerCase).correctLive)
      .appendCorrector(TextMod.removeLeadingChar('@').correctFull)
      .addInvalidator(CV.invalidator.whitelistCharRangeRegex("a-z0-9_")(Invalidity("Can only contain letters, numbers and underscores.")))
      .addInvalidator(CV.invalidator.startsWithRegex("[a-z]")(Invalidity("Must start with a letter.")))
      .addInvalidator(CV.invalidator.endsWithRegex("[a-z0-9]")(Invalidity("Must end with a letter or a number.")))
      .addInvalidator(CV.invalidator.blacklistValuesP(usernameBlacklist)(_.replace("_", ""))(Uniqueness.notUnique))
      .addInvalidator(Invalidator.logic[String](s => !s.replace("_", "").contains("shipreq"))(Uniqueness.notUnique))
      .toValidator
      .mapValid(Username.apply)
      .named(CommmonUiText.username)
      .stateful(_ appendInvalidator Uniqueness.set(_))

  val usernameOrEmail: Composite.Validator[String, String, Username \/ EmailAddr] = {
    type R = Username \/ EmailAddr
    val vu = username.stateless.named.mapValid(-\/(_): R)
    val ve = emailAddr.named.mapValid(\/-(_): R)
    Validator.choose(s => if (EmailAddr.isEmailAddr(s)) ve else vu)
  }

  val personName: Composite.Stateless[String, String, PersonName] =
    CV.endoCorrector.singleLineWhitespace
      .withInvalidator(
        CV.invalidator.nonEmpty.whenValid(
          CV.invalidator.shortTextLimit))
      .toValidator
      .mapValid(PersonName.apply)
      .named(CommmonUiText.userPersonName)
}
