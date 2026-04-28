package shipreq.taskman.server.logic.business

import java.time.Instant
import shipreq.base.util.ArticulateError
import shipreq.taskman.api.Task.{LandingPageHit, UserFeedbackReceived}
import shipreq.taskman.server.logic.{TaskDetail, TaskHeader}

final class Emails(ep: Email.EnvelopeProps, tv: Email.TokenValues) {
  import Email._
  import Email.ContentUtil._
  import ep._
  import tv._

  type SendOp = BusinessOp.SendEmail

  def sendToUser(a: Addr, c: Content): SendOp = {
    val e = Envelope(publicFrom, NonEmptyVector(a), bcc = archiveAddrs)
    BusinessOp.SendEmail(e, c)
  }

  def diagnosticEmail(subject: String, body: String, task: TaskDetail) =
    Content(s"[DIAG] $subject", s"$body\n\n${"=" * 40}\nTask header: ${task.hdr}\nFailure count: ${task.failureCount}")

  // ---------------------------------------------------------------------------

  val archiveEnv: Option[Envelope] =
    archiveAddrs match {
      case Nil    => None
      case h :: t => Some(Envelope(publicFrom, NonEmptyVector(h, t.toVector)))
    }

  def archive(c: => Content): Option[SendOp] =
    archiveEnv.map(BusinessOp.SendEmail(_, c))

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Emails for support staff

  def clientError(user: Option[ShipReqUser], nameKey: String, messageKey: String, data: Map[String, String]) = {
    val info    = Info(data).addUser(user)
    val subject = exceptionSubject("Error occurred in client", data.get(nameKey), data.get(messageKey))
    Email.Content(subject, info.format)
  }

  def serverError(user: Option[ShipReqUser], nameKey: String, messageKey: String, data: Map[String, String]) = {
    val info    = Info(data).addUser(user)
    val subject = exceptionSubject("Error occurred in server", data.get(nameKey), data.get(messageKey))
    Email.Content(subject, info.format)
  }

  def workerFailureEmail(t: Instant, td: TaskDetail, e: ArticulateError): Content = {
    val info = Info.empty
      .addTask(td)
      .add("occurred.at", t)
    Content(
      s"Taskman worker failed on task (${td.id.value}) ${td.task.taskTypeStr}",
      s"""
         |${info.format}
         |
         |$separator
         |
         |${e.show}
       """.stripMargin.trim)
  }

  def taskmanErrorEmail(t: Instant, e: ArticulateError, m: Option[TaskDetail]): Content = {
    val info = Info.empty
      .addTask(m)
      .add("occurred.at", t)
    Content(
      "Taskman infrastructure failed",
      s"""
         |${info.format}
         |
         |$separator
         |
         |${e.show}
       """.stripMargin.trim)
  }

  def landingPage(m: TaskHeader, l: LandingPageHit) = {
    val info = Info.empty
        .addTask(m)
        .add("contact.time", m.created)
        .add("contact.name", l.name)
        .add("contact.email", l.email.value)
        .add("contact.newsletter", l.newsletter)
        .addOption("contact.ip", l.ip)
    Email.Content("Landing page contact",
      s"""
         |${l.msg.getOrElse("<no msg>")}
         |
         |$separator
         |
         |${info.format}
         |""".stripMargin
    )
  }

  def userFeedback(u: UserFeedbackReceived, user: ShipReqUser) = {
    val info = Info(u.metadata).addUser(user)
    Email.Content("User feedback received",
      s"""
         |${u.feedback}
         |
         |$separator
         |
         |${info.format}
         |""".stripMargin.trim)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Emails for end-users

  private val passwordChangeRequestS = s"$shipreqName Password Change Request"

  def passwordChangeRequest(url: String) =
    Content(passwordChangeRequestS, s"""
Hi,

Someone recently requested a password change to your $shipreqName account.

If this was you, you can set a new password here:
$url

If you didn't request this, please ignore this email - your password will not be changed.

    """.trim)

  // -------------------------------------------------------------------------------------------------------------------

  private val registrationS = s"Registration at $shipreqName"

  def linkToCompleteRegistration(url: String) =
    Content(registrationS, s"""

Your email address has been used to register a $shipreqName account.

To continue your registration, simply click on the following link:
$url

If you were not expecting this message, please ignore and delete it.

    """.trim)

  // -------------------------------------------------------------------------------------------------------------------

  val reRegistrationAttempted =
    Content(registrationS, s"""

Somebody, probably you, has tried to re-register your email address.
As you already have a registered account, no action has been taken.

To login or reset your password, simply click on the following link:
$loginUrl

If you were not expecting this message, please ignore and delete it.

    """.trim)

}
