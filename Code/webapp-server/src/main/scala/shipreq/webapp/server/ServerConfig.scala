package shipreq.webapp.server

import net.liftweb.util.Helpers._
import org.joda.time.Period
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.jodatime.JodaTimeValueRetrievers
import shipreq.webapp.server.util.ExpireAfter
import shipreq.webapp.server.util.PropsRetrievers._

object ServerConfig {
  implicit def PropScope = scopeByNS("shipreq")
  private val jtr = JodaTimeValueRetrievers(retrieverS)
  import jtr.retrieverPeriod
  private implicit def rts: Retriever[TimeSpan] = jtr.retrieverPeriod.map(p => p)

  val SupportEmailAddress = need[String]("support.email")

  val BaseUrl = need[String]("url")

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  val AttackFrustrationDelayMs: Long =
    need[Period]("attack_frustration_delay").toStandardDuration.getMillis

  /** Number of characters in tokens used for email & reset-password verification. */
  val ConfirmationTokenLength = need[Int]("token.length")

  /** The DB schema in which the Taskman interfaces reside. */
  val TaskmanSchema = need[String]("taskman.schema")

  /** How long confirmation tokens are valid for after issuing. */
  val TokenLifespan = need[TimeSpan]("token.lifespan.email_conf")

  /** How long password-reset tokens are valid for after issuing. */
  val PasswordResetTokenLifespan = need[TimeSpan]("token.lifespan.resetpw")

  /** Maximum time a flash variable will be retained. (default) */
  val FlashVarTTL = Period seconds 12

  val QuoteCachePolicy = ExpireAfter(Period minutes 30)

  /**
   * Whether or not new registrations are allowed.
   * (Registration tokens already issued will still be accepted.)
   */
  var AllowRegister: () => Boolean = { // non-volatile var allowed because modification will only occur in test-mode.
    val v = tryNeed("allow.register", true)
    () => v
  }
}
