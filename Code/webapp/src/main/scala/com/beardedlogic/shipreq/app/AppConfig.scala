package com.beardedlogic.shipreq.app

import net.liftweb.util.Helpers._
import org.joda.time.Period
import com.beardedlogic.shipreq.util.ExternalValueReader._
import com.beardedlogic.shipreq.util.RuntimePropReaders._
import com.beardedlogic.shipreq.util.ExpireAfter

final object AppConfig {
  implicit def PropScope = GlobalScope

  // Make sure this is in sync with application.js
  val AppName = "ShipReq"

  val SupportEmailAddress = "bearded.logic@gmail.com"

  val BaseUrl = need[String]("server.url")

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  val AttackFrustrationDelayMs: Long = 120

  /** Number of characters in tokens used for email & reset-password verification. */
  val ConfirmationTokenLength = 49

  val MailFromAddress = need[String]("mail.from")

  /** Passwords' min & max lengths. */
  val PasswordLength = 8 to 128

  /** How long confirmation tokens are valid for after issuing. */
  val TokenLifespan = 3 days

  /** How long password-reset tokens are valid for after issuing. */
  val PasswordResetTokenLifespan = 24 hours

  /** Usernames' min & max lengths. */
  val UsernameLength = 3 to 32

  /** Email address max length. */
  val EmailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  val ShortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  val LargeTextMaxLength = 20000

  /** The amount of time that a user is allowed to view a share after authenticating, without re-authenticating. */
  val ShareViewAuthPeriod = Period minutes 30

  /** Maximum time a flash variable will be retained. (default) */
  val FlashVarTTL = Period seconds 12

  val QuoteCachePolicy = ExpireAfter(Period minutes 30)

  val DemoUseCaseMaxSteps = 50

  /**
   * Whether or not new registrations are allowed.
   * (Registration tokens already issued will still be accepted.)
   */
  var AllowRegister: () => Boolean = { // non-volatile var allowed because modification will only occur in test-mode.
    val v = get[Boolean]("app.allow.register") openOr true
    () => v
  }

  val jQueryVersion = "1.10.2"
}
