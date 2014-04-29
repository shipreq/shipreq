package shipreq.taskman.server.business

import scalaz.{NonEmptyList, -\/, \/-}
import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.effect.IOE
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.Types._
import shipreq.taskman.server.{MsgDetail, Deliberate, Deterministic}
import shipreq.taskman.server.Worker.{AsyncScheduler, MsgProcessor, MsgProcessorIn, MsgProcessorOut}
import Bop._
import ErrorOr.Implicits._

final class BusinessLogic[F[_]](
      bopReifier: BopReifier,
      emails: Emails,
      emailScheduler: AsyncScheduler[F],
      mailingListId: MailingList.ListId
    ) extends MsgProcessor[F] {

  type MI = MsgProcessorIn[F]
  type MO = MsgProcessorOut[F]

  private[this] implicit def autoParseEa(ea: EmailAddr): Email.Addr = Email.Addr(ea)

  @inline private[this] def emailUser(to: Email.Addr, c: Email.Content)(implicit i: MI): MO =
    send(emails.sendToUser(to, c))

  @inline private[this] def send(e: Bop.SendEmail)(implicit i: MI): MO =
    i.async(emailScheduler)(bopReifier(e))

  @inline private[this] def run[A](a: MailingList.API[A]): IOE[A] =
    bopReifier(MailingListOp(a))

  @inline private[this] def run[A](a: Bop[A]): IOE[A] =
    bopReifier(a)

  override def apply(i: MI): MO = {
    @inline def md = i.m
    @inline implicit def _i = i
    md.msg match {

      case RegistrationRequested(addr, url) =>
        emailUser(addr, emails.linkToCompleteRegistration(url))

      case ReRegistrationAttempted(addr) =>
        emailUser(addr, emails.reRegistrationAttempted)

      case PasswordResetRequested(addr, url) =>
        emailUser(addr, emails.passwordChangeRequest(url))

      case SendDiagEmail(addr, subject, body) =>
        emailUser(addr, emails.diagnosticEmail(subject, body, md))

      case l: LandingPageHit =>
        i sync LandingPage(l)

      case RegistrationCompleted(id) =>
        i.sync(ActiveUser.get(id) >==> ActiveUser.updateML)

      case d: DummyMsg =>
        dummy(md, d)
    }
  }

  object ActiveUser {
    import MailingList._

    def get(id: UserId): IOE[ShipReqUser] =
      run(LookupShipReqUser(-\/(id))) >=> (ErrorOr.fromOption(_, s"User not found: $id"))

    def subscription(u: ShipReqUser) =
      Subscription(u.email, u.name, u.newsletter, AccountStatus.Active)

    def updateML(u: ShipReqUser): IOE[Unit] =
      run(API.BatchSubscribe(mailingListId, NonEmptyList(subscription(u))))
  }

  object LandingPage {

    def apply(l: LandingPageHit): IOE[Unit] = {
      import l._
      updateMailingListIfNeeded(email, name, newsletter) |>==>
        run(emails.propagateLandingPageMsg(email, name, msg, newsletter))
    }

    def updateMailingListIfNeeded(addr: EmailAddr, name: String, newsletter: Boolean): IOE[Unit] =
      unlessShipReqUser(addr)(updateMailingList(addr, name, newsletter))

    def unlessShipReqUser(addr: EmailAddr)(action: => IOE[Unit]): IOE[Unit] =
      run(LookupShipReqUser(\/-(addr))) >==> {
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
  }

  private[this] def dummy(md: MsgDetail, msg: DummyMsg)(implicit i: MI): MO = {
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
      case true  => i.async(emailScheduler)(io)
      case false => i.sync(io)
    }
  }
}
