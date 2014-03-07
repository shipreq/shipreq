package shipreq.webapp
package mail

import app.AppConfig._
import app.AppSiteMap.Implicits._
import app.AppSiteMap._
import lib.MailCompositionHelpers

object RegistrationEmails extends MailCompositionHelpers {

  val subject = s"Registration at $AppName"

  def LinkToCompleteRegistration(token: String) =
    plainTextMail(subject, s"""

Your email address has been used to register a $AppName account.

To continue your registration, simply click on the following link:
${Register2.absoluteUrl(token)}

If you were not expecting this message, please ignore and delete it.

""".trim)

  val AlreadyRegistered =
    plainTextMail(subject, s"""

Somebody, probably you, has tried to re-register your email address.
As you already have a registered account, no action has been taken.

To login or reset your password, simply click on the following link:
${Login.absoluteUrl}

If you were not expecting this message, please ignore and delete it.

""".trim)
}
