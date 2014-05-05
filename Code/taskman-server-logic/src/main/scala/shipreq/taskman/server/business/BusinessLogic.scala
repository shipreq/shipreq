package shipreq.taskman.server.business

import scalaz.{NonEmptyList, -\/, \/-}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.{Util, ErrorOr, Error}
import shipreq.base.util.ScalaExt.StringBuilderExt
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.Types._
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

  @inline private[this] def complete(io: IOE[_]): MO = io |>-> Complete

  private[this] implicit def autoParseEa(ea: EmailAddr): Email.Addr = Email.Addr(ea)

  @inline private[this] def emailUser(to: Email.Addr, c: Email.Content): MO =
    sendEmailAsync(emails.sendToUser(to, c))

  @inline private[this] def sendEmailAsync(e: Bop.SendEmail): MO =
    IOE pure Schedule(emailScheduler, complete(bopReifier(e)))

  @inline private[this] def run[A](a: MailingList.API[A]): IOE[A] = bopReifier(MailingListOp(a))
  @inline private[this] def run[A](a: Support.API[A])    : IOE[A] = bopReifier(SupportOp(a))
  @inline private[this] def run[A](a: Bop[A])            : IOE[A] = bopReifier(a)

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
      complete(LandingPage(md, l))

    case RegistrationCompleted(id) =>
      complete(ActiveUser updateML id)

    case UserUpdated(id) =>
      complete(ActiveUser updateML id)

    case SyncToMailingList(cond) =>
      complete(ActiveUser syncToML cond)

    case d: DummyMsg =>
      dummy(md, d)
  }

  object ActiveUser {
    import MailingList._

    def get(id: UserId): IOE[ShipReqUser] =
      run(FindShipReqUser(-\/(id))) >=> (ErrorOr.fromOptionS(_, s"User not found: $id"))

    def subscription(u: ShipReqUser) =
      Subscription(u.email, u.name, u.newsletter, AccountStatus.Active)

    def updateML(id: UserId): IOE[Unit] =
      get(id) >==> updateML

    def updateML(u: ShipReqUser): IOE[Unit] =
      run(API.BatchSubscribe(mailingListId, NonEmptyList(subscription(u))))

    def syncToML(sqlCond: Option[String]): IOE[Unit] =
      run(FindShipReqUsers(sqlCond)) >-> (_ map subscription) >==> {
        case Nil =>
          IOE(log info "No users to sync to mailing list.")
        case h :: t =>
          val ss = NonEmptyList.nel(h, t)
          IO(log.info z s"Syncing ${ss.size} users to mailing list...") >>
            run(API.BatchSubscribe(mailingListId, ss))
      }
  }

  object LandingPage {

    def apply(m: MsgHeader, l: LandingPageHit) =
      updateMailingListIfNeeded(l.email, l.name, l.newsletter) |>==> createSupportTicket(m, l)

    def updateMailingListIfNeeded(addr: EmailAddr, name: String, newsletter: Boolean): IOE[Unit] =
      unlessShipReqUser(addr)(updateMailingList(addr, name, newsletter))

    def unlessShipReqUser(addr: EmailAddr)(action: => IOE[Unit]): IOE[Unit] =
      run(FindShipReqUser(\/-(addr))) >==> {
        case None    => action
        case Some(_) => IOE.nop
      }

    def updateMailingList(addr: EmailAddr, name: String, newsletter: Boolean): IOE[Unit] = {
      import MailingList._
      val s = Subscription(addr, name, newsletter, AccountStatus.Never)
      run(API.Subscribe(mailingListId, s, newsletter)) >==> {
        case Ok =>
          IOE.nop
        case AlreadySubscribed =>
          run(API.UpdateMember(mailingListId, s)) >=> {
            case Ok => ErrorOr.unit
            case f  => ErrorOr error s"Failed to update mailing list: $f"
          }
      }
    }

    def createSupportTicket(m: MsgHeader, l: LandingPageHit): IOE[Support.TicketId] = {
      import Support._
      val from = s"${l.name} <${l.email}>"
      val desc = Util.quickSB(_.mkStringF("","\n","")(
        _.kv("MsgId", m.id.value)
        ,_.kv("Contact time", m.created)
        ,_.kv("Newsletter", l.newsletter)
        ,_.kv("Message", l.msg.map("\n\n" + _) getOrElse "<no msg>")
      ))
      val p = if (l.msg.isDefined) Priority.Medium else Priority.Low
      run(API.NotifyLandingPage(from, "Landing Page", desc, p))
    }
  }

  private[this] def dummy(md: MsgDetail, msg: DummyMsg): MO = {
    import msg._

    val io: IOE[Unit] = IO {
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
      case true  => IOE pure Schedule(emailScheduler, complete(io))
      case false => complete(io)
    }
  }
}
