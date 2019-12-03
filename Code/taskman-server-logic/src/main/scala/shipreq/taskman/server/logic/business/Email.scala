package shipreq.taskman.server.logic.business

import java.time.{Instant, ZoneId, ZoneOffset}
import scalaz.{\/, \/-}
import scalaz.old.NonEmptyList
import shipreq.base.util.ScalaExt.StringBuilderExt
import shipreq.base.util.{ArticulateError, Util}
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.api.Msg.LandingPageHit
import shipreq.taskman.server.logic.business.Email._
import shipreq.taskman.server.logic.{MsgDetail, MsgHeader}

object Email {

  final case class Addr(addr: EmailAddr, preParsed: Option[AnyRef] = None) {

    override def toString =
      preParsed match {
        case None    => s"Unparsed(${addr.value})"
        case Some(p) => s"${p.getClass.getSimpleName}($p)"
      }

    def tryParse[E, P](reuse: PartialFunction[AnyRef, P], parse: EmailAddr => E \/ P): E \/ P =
      preParsed match {
        case Some(p) if reuse.isDefinedAt(p) => \/-(reuse(p))
        case _                               => parse(addr)
      }
  }

  final case class EnvelopeProps(publicFrom: Addr, archiveAddrs: List[Addr])

  final case class TokenValues(shipreqName: String, loginUrl: String)

  final case class EnvelopeFront(to: NonEmptyList[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {
    override def toString =
      Util.quickToString(getClass)(
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty))

    def from(from: Addr) = Envelope(from, to, cc, bcc)
  }

  final case class Envelope(from: Addr, to: NonEmptyList[Addr], cc: List[Addr] = Nil, bcc: List[Addr] = Nil) {

    def showTo: String =
      to.list.iterator.map(_.addr.value).mkString(",")

    override def toString =
      Util.quickToString(getClass)(
        _.kv("from", from),
        _.kv("to", to),
        _.kv("cc", cc, cc.nonEmpty),
        _.kv("bcc", bcc, bcc.nonEmpty))
  }

  final case class Content(subject: String, body: String)
}

// =====================================================================================================================

final class Emails(ep: EnvelopeProps, tv: TokenValues) {
  import ep._
  import tv._

  type SendOp = BusinessOp.SendEmail

  def sendToUser(a: Addr, c: Content): SendOp = {
    val e = Envelope(publicFrom, NonEmptyList(a), bcc = archiveAddrs)
    BusinessOp.SendEmail(e, c)
  }

  def diagnosticEmail(subject: String, body: String, msg: MsgDetail) =
    Content(s"[DIAG] $subject", s"$body\n\n${"=" * 40}\nMsg header: ${msg.hdr}\nFailure count: ${msg.failureCount}")

  // ---------------------------------------------------------------------------

  val archiveEnv: Option[Envelope] =
    archiveAddrs match {
      case Nil    => None
      case h :: t => Some(Envelope(publicFrom, NonEmptyList.nel(h, t)))
    }

  def archive(c: => Content): Option[SendOp] =
    archiveEnv.map(BusinessOp.SendEmail(_, c))

  private def timeFieldsForSupport(i: Instant): String = {
    val utc = i.atOffset(ZoneOffset.UTC)
    val sydney = i.atZone(ZoneId of "Australia/Sydney")
    s"TIME: $utc\n      $sydney"
  }

  def workerFailureEmail(t: Instant, m: MsgDetail, e: ArticulateError): Content =
    Content(
      s"Taskman worker failed on msg (${m.id.value}) ${m.msg.msgTypeStr}",
      s"""
         |${timeFieldsForSupport(t)}
         |
         |Msg: $m
         |
         |${e.show}
       """.stripMargin.trim)

  def taskmanErrorEmail(t: Instant, e: ArticulateError, m: Option[MsgDetail]): Content =
    Content(
      "Taskman infrastructure failed",
      s"""
         |${timeFieldsForSupport(t)}
         |
         |Msg: $m
         |
         |${e.show}
       """.stripMargin.trim)

  // ---------------------------------------------------------------------------

  def landingPageEmail(m: MsgHeader, l: LandingPageHit) = {
    val body =
      Util.quickSB(_.mkStringF("","\n","")(
        _.kv("MsgId", m.id.value)
        ,_.kv("Contact time", m.created)
        ,_.kv("Name", l.name)
        ,_.kv("Email", l.email.value)
        ,_.kv("Newsletter", l.newsletter)
        ,_.kv("Message", l.msg.fold("<no msg>")("\n\n" + _))))
    Email.Content("Landing Page Contact", body)
  }

  // ---------------------------------------------------------------------------

  private val passwordChangeRequestS = s"$shipreqName Password Change Request"

  def passwordChangeRequest(url: String) =
    Content(passwordChangeRequestS, s"""
Hi,

Someone recently requested a password change to your $shipreqName account.

If this was you, you can set a new password here:
$url

If you didn't request this, please ignore this email - your password will not be changed.

    """.trim)

  // ---------------------------------------------------------------------------

  private val registrationS = s"Registration at $shipreqName"

  def linkToCompleteRegistration(url: String) =
    Content(registrationS, s"""

Your email address has been used to register a $shipreqName account.

To continue your registration, simply click on the following link:
$url

If you were not expecting this message, please ignore and delete it.

    """.trim)

  val reRegistrationAttempted =
    Content(registrationS, s"""

Somebody, probably you, has tried to re-register your email address.
As you already have a registered account, no action has been taken.

To login or reset your password, simply click on the following link:
$loginUrl

If you were not expecting this message, please ignore and delete it.

    """.trim)

}