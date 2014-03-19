package shipreq.taskman.server.business

import scalaz.NonEmptyList
import shipreq.taskman.api.Types._

object Email {

  trait Ctx {
    val shipreq: String
    val loginUrl: String // TODO get from webapp via config table in DB
    val defaultFromAddress: EmailAddr
  }

  case class Envelope(from: EmailAddr
                      , to: NonEmptyList[EmailAddr]
                      , cc: List[EmailAddr] = Nil
                      , bcc: List[EmailAddr] = Nil)

  case class Content(subject: String, body: String)
}

class Emails(ctx: Email.Ctx) {
  import Email.Content
  import ctx._

  def sendToUser(addr: EmailAddr, c: Content): Bop[Unit] = {
    val e = Email.Envelope(ctx.defaultFromAddress, NonEmptyList(addr))
    Bop.SendEmail(e, c)
  }

  // ===================================================================================================================

  private val passwordChangeRequestS = s"$shipreq Password Change Request"

  def passwordChangeRequest(url: String) =
    Content(passwordChangeRequestS, s"""
Hi,

Someone recently requested a password change to your $shipreq account.

If this was you, you can set a new password here:
$url

If you didn't request this, please ignore this email - your password will not be changed.

    """.trim)

  // ===================================================================================================================

  private val registrationS = s"Registration at $shipreq"

  def linkToCompleteRegistration(url: String) =
    Content(registrationS, s"""

Your email address has been used to register a $shipreq account.

To continue your registration, simply click on the following link:
$url

If you were not expecting this message, please ignore and delete it.

    """.trim)

  // ===================================================================================================================

  val reRegistrationAttempted =
    Content(registrationS, s"""

Somebody, probably you, has tried to re-register your email address.
As you already have a registered account, no action has been taken.

To login or reset your password, simply click on the following link:
$loginUrl

If you were not expecting this message, please ignore and delete it.

    """.trim)

}