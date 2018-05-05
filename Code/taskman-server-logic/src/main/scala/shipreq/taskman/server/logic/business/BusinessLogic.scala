package shipreq.taskman.server.logic.business

import scalaz.{-\/, \/-, ~>}
import scalaz.old.NonEmptyList
import scalaz.syntax.bind._
import scalaz.syntax.catchable._
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.taskman.server.logic.{Deliberate, MsgDetail, MsgHeader, Worker}
import BusinessOp._

final class BusinessLogic[F[_]](emails        : Emails,
                                emailScheduler: Worker.AsyncScheduler[F],
                                mailingListId : MailingList.ListId)
                               (implicit businessOpFx: BusinessOp ~> Fx) extends Worker.MsgProcessor[F] with HasLogger {

  type MO = Worker.MsgProcessorOut[F]

  @inline private def complete(io: Fx[_]): MO =
    io.map(_ => Worker.ProcessorResult.Complete)

  private implicit def autoParseEa(ea: EmailAddr): Email.Addr = Email.Addr(ea)

  @inline private def emailUser(to: Email.Addr, c: Email.Content): MO =
    sendEmailAsync(emails.sendToUser(to, c))

  @inline private def sendEmailAsync(e: BusinessOp.SendEmail): MO =
    Fx pure Worker.ProcessorResult.Schedule[F](emailScheduler, complete(businessOpFx(e)))

  @inline private def run[A](a: MailingList.API[A]): Fx[A] = businessOpFx(MailingListOp(a))
  @inline private def run[A](a: Support.API[A])    : Fx[A] = businessOpFx(SupportOp(a))
  @inline private def run[A](a: BusinessOp[A])     : Fx[A] = businessOpFx(a)

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
      LandingPage(md.hdr, l)

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

    def get(id: UserId): Fx[ShipReqUser] =
      run(FindShipReqUser(-\/(id))) getOrFail s"User not found: $id"

    def tryDesc(id: UserId): Fx[String] =
      get(id).attempt.map {
        case \/-(u)                  => u.toString
        case -\/(e: ArticulateError) => e.getMessage
        case -\/(e)                  => s"Error occurred looking up user #$id: ${e.getMessage}"
      }

    def subscription(u: ShipReqUser) =
      Subscription(u.email, u.name, u.newsletter, AccountStatus.Active)

    def updateML(id: UserId): Fx[Unit] =
      get(id) flatMap updateML

    def updateML(u: ShipReqUser): Fx[Unit] =
      run(API.BatchSubscribe(mailingListId, NonEmptyList(subscription(u))))

    def syncToML(sqlCond: Option[String]): Fx[Unit] =
      run(FindShipReqUsers(sqlCond)).map(_ map subscription).flatMap {
        case Nil =>
          Fx(log info "No users to sync to mailing list.")
        case h :: t =>
          val ss = NonEmptyList.nel(h, t)
          Fx(log info s"Syncing ${ss.size} users to mailing list...") >>
            run(API.BatchSubscribe(mailingListId, ss))
      }
  }

  object LandingPage {

    def apply(m: MsgHeader, l: LandingPageHit): MO = {
      val c = emails.landingPageEmail(m, l)
      val fx = updateMailingListIfNeeded(l.email, l.name, l.newsletter) >> createSupportTicket(m, l, c)
      emails.archive(c) match {
        case None     => complete(fx)
        case Some(op) => fx >> sendEmailAsync(op)
      }
    }

    def updateMailingListIfNeeded(addr: EmailAddr, name: String, newsletter: Boolean): Fx[Unit] =
      unlessShipReqUser(addr)(updateMailingList(addr, name, newsletter))

    def unlessShipReqUser(addr: EmailAddr)(action: => Fx[Unit]): Fx[Unit] =
      run(FindShipReqUser(\/-(addr))) flatMap {
        case None    => action
        case Some(_) => Fx.unit
      }

    def updateMailingList(addr: EmailAddr, name: String, newsletter: Boolean): Fx[Unit] = {
      import MailingList._
      val s = Subscription(addr, name, newsletter, AccountStatus.Never)
      run(API.Subscribe(mailingListId, s, newsletter)) flatMap {
        case Ok =>
          Fx.unit
        case AlreadySubscribed =>
          run(API.UpdateMember(mailingListId, s)).flatMap {
            case Ok => Fx.unit
            case f  => Fx fail ArticulateError(s"Failed to update mailing list: $f")
          }
      }
    }

    def createSupportTicket(m: MsgHeader, l: LandingPageHit, c: Email.Content): Fx[Support.TicketId] = {
      import Support._
      val from = s"${l.name} <${l.email}>"
      val p = if (l.msg.isDefined) Priority.Medium else Priority.Low
      run(API.NotifyLandingPage(from, c.subject, c.body, p))
    }
  }

  private def dummy(md: MsgDetail, msg: DummyMsg): MO = {
    import msg._

    val fx = Fx[Unit] {
      if (processingTimeMs > 0)
        Thread.sleep(processingTimeMs)

      if (md.failureCount < retryCount)
        throw ArticulateError(s"Failure count (${md.failureCount}) < desired ($retryCount).").tag(Deliberate)

      failureMsg match {
        case Some(e) => throw ArticulateError(e).tag(Deliberate, ArticulateError.Deterministic)
        case None    => ()
      }
    }

    if (async)
      Fx pure Worker.ProcessorResult.Schedule[F](emailScheduler, complete(fx))
    else
      complete(fx)
  }
}
