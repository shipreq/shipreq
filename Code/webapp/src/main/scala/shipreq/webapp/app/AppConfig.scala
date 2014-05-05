package shipreq.webapp.app

import net.liftweb.util.Helpers._
import org.joda.time.Period
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.jodatime.JodaTimeValueRetrievers
import shipreq.webapp.util.PropsRetrievers._
import shipreq.webapp.util.ExpireAfter

final object AppConfig {
  implicit def PropScope = scopeByNS("shipreq")
  private val jtr = JodaTimeValueRetrievers(retrieverS)
  import jtr.retrieverPeriod
  private implicit def rts: Retriever[TimeSpan] = jtr.retrieverPeriod.map(p => p)

  // Make sure this is in sync with application.js
  val AppName = "ShipReq"

  val SupportEmailAddress = need[String]("support.email")

  val BaseUrl = need[String]("url")

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  val AttackFrustrationDelayMs = need[Period]("attack_frustration_delay").toStandardDuration.getMillis

  /** Number of characters in tokens used for email & reset-password verification. */
  val ConfirmationTokenLength = need[Int]("token.length")

  /** The DB schema in which the Taskman interfaces reside. */
  val TaskmanSchema = need[String]("taskman.schema")

  /** Passwords' min & max lengths. */
  val PasswordLength = 8 to 128

  /** How long confirmation tokens are valid for after issuing. */
  val TokenLifespan = need[TimeSpan]("token.lifespan.email_conf")

  /** How long password-reset tokens are valid for after issuing. */
  val PasswordResetTokenLifespan = need[TimeSpan]("token.lifespan.resetpw")

  /** Usernames' min & max lengths. */
  val UsernameLength = 3 to 32

  /** Email address max length. */
  val EmailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  val ShortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  val LargeTextMaxLength = 20000

  /** The amount of time that a user is allowed to view a share after authenticating, without re-authenticating. */
  val ShareViewAuthPeriod = need[Period]("share.auth_period")

  /** Maximum time a flash variable will be retained. (default) */
  val FlashVarTTL = Period seconds 12

  val QuoteCachePolicy = ExpireAfter(Period minutes 30)

  val DemoUseCaseMaxSteps = 50

  /**
   * Whether or not new registrations are allowed.
   * (Registration tokens already issued will still be accepted.)
   */
  var AllowRegister: () => Boolean = { // non-volatile var allowed because modification will only occur in test-mode.
    val v = tryNeed("allow.register", true)
    () => v
  }

  val jQueryVersion = "2.1.0"
}
