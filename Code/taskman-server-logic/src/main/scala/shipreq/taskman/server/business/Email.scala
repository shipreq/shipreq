package shipreq.taskman.server.business

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scalaz.NonEmptyList
import shipreq.base.util.{Util, ErrorOr, Error}
import shipreq.base.util.ScalaExt.StringBuilderExt
import shipreq.taskman.api.Types.EmailAddr
import shipreq.taskman.server.MsgDetail
import Email._

object Email {

  final case class Addr(addr: EmailAddr, preParsed: Option[AnyRef] = None) {

    override def toString =
      if (preParsed.isDefined) preParsed.toString else addr

    def tryParse[P](reuse: PartialFunction[AnyRef, P], parse: EmailAddr => ErrorOr[P]): ErrorOr[P] =
      preParsed match {
        case Some(p) if reuse.isDefinedAt(p) => ErrorOr(reuse(p))
        case _ => parse(addr)
      }
  }

  trait EnvelopeProps {
    val publicFrom: Addr
    val landingPageEnv: EnvelopeFront
    val supportEnv: Envelope
  }

  trait TokenValues {
    val shipreqName: String
    val loginUrl: String
  }

  case class EnvelopeFront(to: NonEmptyList[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {
    override def toString =
      Util.quickToString(getClass)(
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty)
      )

    def from(from: Addr) = Envelope(from, to, cc, bcc)
  }

  final case class Envelope(from: Addr, to: NonEmptyList[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {
    override def toString =
      Util.quickToString(getClass)(
        _.kv("from", from),
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty)
      )
  }

  final case class Content(subject: String, body: String)

  val timeFormat = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
}

final class Emails(ep: EnvelopeProps, tv: TokenValues) {
  import ep._
  import tv._

  type SendOp = Bop.SendEmail

  def sendToUser(a: Addr, c: Content): SendOp = {
    val e = Envelope(publicFrom, NonEmptyList(a))
    Bop.SendEmail(e, c)
  }

  def diagnosticEmail(subject: String, body: String, msg: MsgDetail) =
    Content(s"[DIAG] $subject", s"$body\n\n${"=" * 40}\nMsg header: ${msg.hdr}\nFailure count: ${msg.failureCount}")

  def propagateLandingPageMsg(a: Addr, name: String, msg: Option[String], newsletter: Boolean): SendOp = {
    val e = landingPageEnv.from(a)
    var b = s"Name: $name\nEmail: ${a.addr}\nNewsletter: $newsletter"
    for (m <- msg) b += s"\n\n$m"
    val c = Content(s"Landing Page: ${a.addr}", b)
    Bop.SendEmail(e, c)
  }

  // ===================================================================================================================

  def notifySupportOfWorkerFailure(t: DateTime, m: MsgDetail, e: Error): SendOp =
    Bop.SendEmail(supportEnv, Content(
      s"[TASKMAN] Worker failed on #${m.hdr.id.value}",
      s"TIME: ${t toString timeFormat}\n\nMSG: $m\n\nERROR: ${e.stackTraceStr}"))

  def notifySupportOfTaskmanError(t: DateTime, e: Error, m: Option[MsgDetail]): SendOp =
    Bop.SendEmail(supportEnv, Content(
      s"[TASKMAN] Taskman infrastructure itself failed",
      s"TIME: ${t toString timeFormat}\n\nERROR: ${e.stackTraceStr}\n\nMSG: $m"))

  // ===================================================================================================================

  private val passwordChangeRequestS = s"$shipreqName Password Change Request"

  def passwordChangeRequest(url: String) =
    Content(passwordChangeRequestS, s"""
Hi,

Someone recently requested a password change to your $shipreqName account.

If this was you, you can set a new password here:
$url

If you didn't request this, please ignore this email - your password will not be changed.

    """.trim)

  // ===================================================================================================================

  private val registrationS = s"Registration at $shipreqName"

  def linkToCompleteRegistration(url: String) =
    Content(registrationS, s"""

Your email address has been used to register a $shipreqName account.

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