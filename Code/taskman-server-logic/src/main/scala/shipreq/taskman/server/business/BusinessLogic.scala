package shipreq.taskman.server.business

import scalaz.~>
import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Error}
import shipreq.taskman.api.Msg._
import shipreq.taskman.api.Types.EmailAddr
import shipreq.taskman.server.{IOE, Deliberate, Deterministic}
import shipreq.taskman.server.Worker.{MsgProcessor, MsgProcessorIn, MsgProcessorOut}

final class BusinessLogic[EA, F[_]](
      ctx: Email.Ctx[EA],
      bopReifier: BopReifier,
      emailScheduler: IO ~> F
    ) extends MsgProcessor[F] {

  type MI = MsgProcessorIn[F]
  type MO = MsgProcessorOut[F]

  private[this] val emails = new Emails[EA](ctx)
  private[this] implicit def autoParseEa(ea: EmailAddr): EA = ctx.addrParser(ea)
  @inline private[this] def emailUser(to: EA, c: Email.Content)(implicit i: MI): MO = send(emails.sendToUser(to, c))
  @inline private[this] def send(e: Bop.SendEmail[EA])(implicit i: MI): MO = i.syncU(bopReifier(e))

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

      case DummyMsg(desc, async, processingTimeMs, retryCount, _, failureMsg) => {
        val io: IOE[Unit] = IO {
          if (processingTimeMs > 0)
            Thread.sleep(processingTimeMs)
          if (processingTimeMs > 0)
            Thread.sleep(processingTimeMs)
          ErrorOr.tag[Unit](Deliberate)(
            if (md.failureCount < retryCount)
              Error(s"Failure count (${md.failureCount}) < desired ($retryCount).")
            else failureMsg match {
              case Some(e) => ErrorOr.tag(Deterministic)(Error(e))
              case None    => ErrorOr.unit
            }
          )
        }
        async match {
          case true  => i.asyncT(emailScheduler)(io)
          case false => i.sync(io)
        }
      }

    }
  }
}
