package com.beardedlogic.usecase
package mail

import net.liftweb.util.Mailer.{MailTypes, PlainMailBodyType, Subject}
import app.AppConfig._
import app.AppSiteMap._
import app.AppSiteMap.Implicits._

object RegistrationEmails {
  type Mail = (Subject, List[MailTypes])

  private def simple(subj: String, body: String): Mail = (Subject(subj), List(PlainMailBodyType(body.trim)))

  val subject = s"Registration at $AppName"

  def LinkToCompleteRegistration(token: String): Mail =
    simple(subject, s"""

Your email address has been used to register a $AppName account.

To continue your registration, simply click on the following link:
${Register2.absoluteUrl(token)}

If you were not expecting this message, please ignore and delete it.

""")

  val AlreadyRegistered: Mail =
    simple(subject, s"""

Somebody, probably you, has tried to re-register your email address.
As you already have a registered account, no action has been taken.

To login or reset your password, simply click on the following link:
${Login.absoluteUrl}

If you were not expecting this message, please ignore and delete it.

""")
}
