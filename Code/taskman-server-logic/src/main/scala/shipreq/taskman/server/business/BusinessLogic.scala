package shipreq.taskman.server.business

import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.effect.IOE
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.Types.EmailAddr
import shipreq.taskman.server.{MsgDetail, Deliberate, Deterministic}
import shipreq.taskman.server.Worker.{AsyncScheduler, MsgProcessor, MsgProcessorIn, MsgProcessorOut}

final class BusinessLogic[F[_]](
      bopReifier: BopReifier,
      emails: Emails,
      emailScheduler: AsyncScheduler[F]
    ) extends MsgProcessor[F] {

  type MI = MsgProcessorIn[F]
  type MO = MsgProcessorOut[F]

  private[this] implicit def autoParseEa(ea: EmailAddr): Email.Addr = Email.Addr(ea)

  @inline private[this] def emailUser(to: Email.Addr, c: Email.Content)(implicit i: MI): MO =
    send(emails.sendToUser(to, c))

  @inline private[this] def send(e: Bop.SendEmail)(implicit i: MI): MO =
    i.async(emailScheduler)(bopReifier(e))

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

      case d: DummyMsg =>
        dummy(md, d)
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
