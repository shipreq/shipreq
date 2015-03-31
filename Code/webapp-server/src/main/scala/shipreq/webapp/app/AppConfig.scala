package shipreq.webapp.app

import net.liftweb.util.Helpers._
import org.joda.time.Period
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.jodatime.JodaTimeValueRetrievers
import shipreq.webapp.util.PropsRetrievers._
import shipreq.webapp.util.ExpireAfter

object AppConfig {
  implicit def PropScope = scopeByNS("shipreq")
  private final val jtr = JodaTimeValueRetrievers(retrieverS)
  import jtr.retrieverPeriod
  private implicit def rts: Retriever[TimeSpan] = jtr.retrieverPeriod.map(p => p)

  final val SupportEmailAddress = need[String]("support.email")

  final val BaseUrl = need[String]("url")

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  final val AttackFrustrationDelayMs = need[Period]("attack_frustration_delay").toStandardDuration.getMillis

  /** Number of characters in tokens used for email & reset-password verification. */
  final val ConfirmationTokenLength = need[Int]("token.length")

  /** The DB schema in which the Taskman interfaces reside. */
  final val TaskmanSchema = need[String]("taskman.schema")

  /** How long confirmation tokens are valid for after issuing. */
  final val TokenLifespan = need[TimeSpan]("token.lifespan.email_conf")

  /** How long password-reset tokens are valid for after issuing. */
  final val PasswordResetTokenLifespan = need[TimeSpan]("token.lifespan.resetpw")

  /** The amount of time that a user is allowed to view a share after authenticating, without re-authenticating. */
  final val ShareViewAuthPeriod = need[Period]("share.auth_period")

  /** Maximum time a flash variable will be retained. (default) */
  final val FlashVarTTL = Period seconds 12

  final val QuoteCachePolicy = ExpireAfter(Period minutes 30)

  final val DemoUseCaseMaxSteps = 50

  /**
   * Whether or not new registrations are allowed.
   * (Registration tokens already issued will still be accepted.)
   */
  var AllowRegister: () => Boolean = { // non-volatile var allowed because modification will only occur in test-mode.
    val v = tryNeed("allow.register", true)
    () => v
  }

  final val jQueryVersion = "2.1.1"

  /** URL prefix for dev & test only assets */
  final val devAssetPath = "/assets/dev"

  /** URL prefix for vendor assets */
  final val vendorAssetPath = "/assets/vendor"
}
