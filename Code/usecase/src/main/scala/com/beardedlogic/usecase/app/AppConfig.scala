package com.beardedlogic.usecase.app

import net.liftweb.util.Helpers._
import net.liftweb.util.Props

object AppConfig {

  final val AppName = "Golly's Use Case Editor"

  final val BaseUrl = "http://www.TODO.com"

  /** Number of characters in tokens used for email & reset-password verification. */
  final val ConfirmationTokenLength = 49

  val MailFromAddress = Props.get("mail.from").openOrThrowException("Property not specified: mail.from")

  /** How long confirmation tokens are valid for after issuing. */
  final val TokenLifespan = 3 days
}
