package shipreq.taskman.server.logic.business

import japgolly.microlibs.stdlib_ext.MutableArray
import java.time.{Instant, ZoneId, ZoneOffset}
import scalaz.{\/, \/-}
import scalaz.old.NonEmptyList
import shipreq.base.util.ScalaExt.StringBuilderExt
import shipreq.base.util.{ArticulateError, Util}
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.taskman.api.Task.{LandingPageHit, UserFeedbackReceived}
import shipreq.taskman.server.logic.{TaskDetail, TaskHeader}

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

  private[business] object ContentUtil {

    val separator = "=" * 80

    final case class Info(data: Map[String, String]) extends AnyVal {

      def add[A](key: String, value: A)(implicit t: Info.ValueAdd[A]): Info =
        t.add(this, key, value)

      def addOption[A](key: String, value: Option[A])(implicit t: Info.ValueAdd[A]): Info =
        value match {
          case Some(v) => add(key, v)
          case None    => this
        }

      def addTask(v: TaskHeader): Info =
        add("task.id", v.id.value)

      def addTask(v: TaskDetail): Info =
        this
          .addTask(v.hdr)
          .add("task.failureCount", v.failureCount)
          .add("task.instance", v.task.toString)

      def addTask(v: Option[TaskDetail]): Info =
        v.fold(this)(addTask(_))

      def addUserId(id: UserId): Info =
        add("user.id", id.value.toString)

      def addUser(u: ShipReqUser): Info =
        addUserId(u.id)
          .add("user.email", u.email.value)
          .add("user.name", u.name)
          .add("user.username", u.username)

      def addUser(userId: UserId, u: Option[ShipReqUser]): Info =
        u.fold(addUserId(userId))(addUser(_))

      def format: String =
        MutableArray(data.iterator.map { case (k, v) => s"$k = $v" })
          .sort
          .mkString("\n")
    }

    object Info {
      def empty: Info =
        apply(Map.empty)

      def create(kvs: (String, String)*): Info =
        apply(kvs.toMap)

      final case class ValueAdd[A](add: (Info, String, A) => Info) {
        def contramap[B](f: B => A): ValueAdd[B] =
          ValueAdd((i, k, v) => add(i, k, f(v)))
      }

      object ValueAdd {
        implicit val string : ValueAdd[String ] = apply((i, k, v) => Info(i.data.updated(k, v)))
        implicit val int    : ValueAdd[Int    ] = string.contramap(_.toString)
        implicit val long   : ValueAdd[Long   ] = string.contramap(_.toString)
        implicit val boolean: ValueAdd[Boolean] = string.contramap(_.toString)

        implicit val instant: ValueAdd[Instant] =
          apply((i, k, v) => i
            .add(k + ".utc", v.atOffset(ZoneOffset.UTC).toString)
            .add(k + ".local", v.atZone(ZoneId of "Australia/Melbourne").toString))
      }
    }
  }
}

// =====================================================================================================================

final class Emails(ep: Email.EnvelopeProps, tv: Email.TokenValues) {
  import Email._
  import Email.ContentUtil._
  import ep._
  import tv._

  type SendOp = BusinessOp.SendEmail

  def sendToUser(a: Addr, c: Content): SendOp = {
    val e = Envelope(publicFrom, NonEmptyList(a), bcc = archiveAddrs)
    BusinessOp.SendEmail(e, c)
  }

  def diagnosticEmail(subject: String, body: String, task: TaskDetail) =
    Content(s"[DIAG] $subject", s"$body\n\n${"=" * 40}\nTask header: ${task.hdr}\nFailure count: ${task.failureCount}")

  // ---------------------------------------------------------------------------

  val archiveEnv: Option[Envelope] =
    archiveAddrs match {
      case Nil    => None
      case h :: t => Some(Envelope(publicFrom, NonEmptyList.nel(h, t)))
    }

  def archive(c: => Content): Option[SendOp] =
    archiveEnv.map(BusinessOp.SendEmail(_, c))

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Emails for support staff

  def serverError(usrd: String, url: Option[String], report: String): Content = {
    val subj = s"Webapp failure${url.fold("")(" on: " + _)}"
    val desc = s"User: $usrd\n\nURL: $url\n\n$report"
    Content(subj, desc)
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