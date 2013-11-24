package com.beardedlogic.usecase.app

import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.joda.time.Period

final object AppConfig {

  val AppName = "Golly's Use Case Editor"

  // TODO BaseUrl hardcoded to localhost
  val BaseUrl = "http://localhost:8080"

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  val AttackFrustrationDelayMs: Long = 120

  /** Number of characters in tokens used for email & reset-password verification. */
  val ConfirmationTokenLength = 49

  val MailFromAddress = Props.get("mail.from").openOrThrowException("Property not specified: mail.from")

  /** Passwords' min & max lengths. */
  val PasswordLength = 8 to 128

  /** How long confirmation tokens are valid for after issuing. */
  val TokenLifespan = 3 days

  /** Usernames' min & max lengths. */
  val UsernameLength = 3 to 32

  /** Email address max length. */
  val EmailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  val ShortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  val LargeTextMaxLength = 20000

  /** The amount of time that a user is allowed to view a share after authenticating, without re-authenticating. */
  val ShareViewAuthPeriod = Period.minutes(30)

  /** Maximum time a flash variable will be retained. (default) */
  val FlashVarTTL = Period.seconds(12)
}