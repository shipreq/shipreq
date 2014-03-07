package shipreq.webapp
package mail

import app.AppConfig._
import app.AppSiteMap.Implicits._
import app.AppSiteMap._
import lib.MailCompositionHelpers

object PasswordResetEmails extends MailCompositionHelpers {

  val subject = s"$AppName Password Change Request"

  def PasswordChangeRequest(token: String) =
    plainTextMail(subject, s"""

Hi,

Someone recently requested a password change to your $AppName account.

If this was you, you can set a new password here:
${ResetPassword2.absoluteUrl(token)}

If you didn't request this, please ignore this email - your password will not be changed.

""".trim)
}
