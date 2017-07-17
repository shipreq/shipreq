package shipreq.taskman.server.business

import scalaz.{-\/, \/-}
import scalaz.old.NonEmptyList
import scalaz.syntax.bind._
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.FxModule._
import shipreq.base.util.effect.FxE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.taskman.server.{MsgHeader, MsgDetail, Deliberate, Deterministic}
import shipreq.taskman.server.Worker.{AsyncScheduler, MsgProcessor, MsgProcessorOut, ProcessorResult}
import Bop._
import ErrorOr.Implicits._
import ProcessorResult._

final class BusinessLogic[F[_]](
      bopReifier: BopReifier,
      emails: Emails,
      emailScheduler: AsyncScheduler[F],
      mailingListId: MailingList.ListId
    ) extends MsgProcessor[F] with HasLogger {

  type MO = MsgProcessorOut[F]

  @inline private[this] def complete(io: FxE[_]): MO = io |>-> Complete

  private[this] implicit def autoParseEa(ea: EmailAddr): Email.Addr = Email.Addr(ea)

  @inline private[this] def emailUser(to: Email.Addr, c: Email.Content): MO =
    sendEmailAsync(emails.sendToUser(to, c))

  @inline private[this] def sendEmailAsync(e: Bop.SendEmail): MO =
    FxE pure Schedule(emailScheduler, complete(bopReifier(e)))

  @inline private[this] def run[A](a: MailingList.API[A]): FxE[A] = bopReifier(MailingListOp(a))
  @inline private[this] def run[A](a: Support.API[A])    : FxE[A] = bopReifier(SupportOp(a))
  @inline private[this] def run[A](a: Bop[A])            : FxE[A] = bopReifier(a)

  override def apply(md: MsgDetail): MO = md.msg match {

    case RegistrationRequested(addr, url) =>
      emailUser(addr, emails.linkToCompleteRegistration(url))

    case ReRegistrationAttempted(addr) =>
      emailUser(addr, emails.reRegistrationAttempted)

    case PasswordResetRequested(addr, url) =>
      emailUser(addr, emails.passwordChangeRequest(url))

    case SendDiagEmail(addr, subject, body) =>
      emailUser(addr, emails.diagnosticEmail(subject, body, md))

    case l: LandingPageHit =>
      LandingPage(md, l)

    case RegistrationCompleted(id) =>
      complete(ActiveUser updateML id)

    case UserUpdated(id) =>
      complete(ActiveUser updateML id)

    case SyncToMailingList(cond) =>
      complete(ActiveUser syncToML cond)

    case WebappErrorOccurred(usr, url, report) =>
      val usrDescIo = usr.fold(Fx("None"))(ActiveUser.tryDesc)
      usrDescIo.flatMap { usrd =>
        val subj = s"Webapp failure${url.fold("")(" on: " + _)}"
        val desc = s"User: $usrd\n\nURL: $url\n\n$report"
        val op = Support.API.ReportFailure(subj, desc, Support.Priority.High)
        complete(run(op))
      }

    case d: DummyMsg =>
      dummy(md, d)
  }

  object ActiveUser {
    import MailingList._

    def get(id: UserId): FxE[ShipReqUser] =
      run(FindShipReqUser(-\/(id))) >=> (ErrorOr.fromOptionS(_, s"User not found: $id"))

    def tryDesc(id: UserId): Fx[String] =
      get(id).map {
        case \/-(u) => u.toString
        case -\/(e) => s"Error occurred looking up user #$id: ${e.msg}"
      }

    def subscription(u: ShipReqUser) =
      Subscription(u.email, u.name, u.newsletter, AccountStatus.Active)

    def updateML(id: UserId): FxE[Unit] =
      get(id) >==> updateML

    def updateML(u: ShipReqUser): FxE[Unit] =
      run(API.BatchSubscribe(mailingListId, NonEmptyList(subscription(u))))

    def syncToML(sqlCond: Option[String]): FxE[Unit] =
      run(FindShipReqUsers(sqlCond)) >-> (_ map subscription) >==> {
        case Nil =>
          FxE(log info "No users to sync to mailing list.")
        case h :: t =>
          val ss = NonEmptyList.nel(h, t)
          Fx(log.info z s"Syncing ${ss.size} users to mailing list...") >>
            run(API.BatchSubscribe(mailingListId, ss))
      }
  }

  object LandingPage {

    def apply(m: MsgHeader, l: LandingPageHit) = {
      val c = emails.landingPageEmail(m, l)
      val io = updateMailingListIfNeeded(l.email, l.name, l.newsletter) |>==> createSupportTicket(m, l, c)
      emails.archive(c) match {
        case None     => complete(io)
        case Some(op) => io |>==> sendEmailAsync(op)
      }
    }

    def updateMailingListIfNeeded(addr: EmailAddr, name: String, newsletter: Boolean): FxE[Unit] =
      unlessShipReqUser(addr)(updateMailingList(addr, name, newsletter))

    def unlessShipReqUser(addr: EmailAddr)(action: => FxE[Unit]): FxE[Unit] =
      run(FindShipReqUser(\/-(addr))) >==> {
        case None    => action
        case Some(_) => FxE.nop
      }

    def updateMailingList(addr: EmailAddr, name: String, newsletter: Boolean): FxE[Unit] = {
      import MailingList._
      val s = Subscription(addr, name, newsletter, AccountStatus.Never)
      run(API.Subscribe(mailingListId, s, newsletter)) >==> {
        case Ok =>
          FxE.nop
        case AlreadySubscribed =>
          run(API.UpdateMember(mailingListId, s)) >=> {
            case Ok => ErrorOr.unit
            case f  => ErrorOr error s"Failed to update mailing list: $f"
          }
      }
    }

    def createSupportTicket(m: MsgHeader, l: LandingPageHit, c: Email.Content): FxE[Support.TicketId] = {
      import Support._
      val from = s"${l.name} <${l.email}>"
      val p = if (l.msg.isDefined) Priority.Medium else Priority.Low
      run(API.NotifyLandingPage(from, c.subject, c.body, p))
    }
  }

  private[this] def dummy(md: MsgDetail, msg: DummyMsg): MO = {
    import msg._

    val io: FxE[Unit] = Fx {
      if (processingTimeMs > 0)
        Thread.sleep(processingTimeMs)
      ErrorOr.tag[Unit](Deliberate)(
        if (md.failureCount < retryCount)
          ErrorOr.error(s"Failure count (${md.failureCount}) < desired ($retryCount).")
        else failureMsg match {
          case Some(e) => Error(e).tag(Deterministic).toErrorOr
          case None    => ErrorOr.unit
        }
      )
    }

    async match {
      case true  => FxE pure Schedule(emailScheduler, complete(io))
      case false => complete(io)
    }
  }
}
