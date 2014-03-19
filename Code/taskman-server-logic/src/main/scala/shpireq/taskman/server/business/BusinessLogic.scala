package shpireq.taskman.server.business

import scalaz.{~>, \/-, NonEmptyList}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ErrorOr
import shpireq.taskman.server._
import shipreq.taskman.api.{Types, Msg}
import Types._
import BusinessLogic._

object BusinessLogic {
  type R = IO[ErrorOr[Unit]]

  private[this] val nop: R = IO(\/-(()))

  implicit class BopExt[A](val op: Bop[A]) extends AnyVal {
    def toIOE(implicit opToIo: Bop ~> IO): IO[ErrorOr[A]] = op.toIO.map(\/-(_))
    def toIOU(implicit opToIo: Bop ~> IO, ev: A =:= Unit): IO[ErrorOr[Unit]] = op.toIO >> nop
  }

}

class BusinessLogic(implicit ctx: Email.Ctx, bopToIo: Bop ~> IO) {

  val emails = new Emails(ctx)

  private def sendEmailToUser(addr: EmailAddr, c: Email.Content): R = {
    val e = Email.Envelope(ctx.defaultFromAddress, NonEmptyList(addr))
    val op = Bop.SendEmail(e, c)
    op.toIOU
  }

  val msgProcessor: Msg => R = {

    case Msg.RegistrationRequested(addr, url) =>
      sendEmailToUser(addr, emails.linkToCompleteRegistration(url))

    case Msg.ReRegistrationAttempted(addr) =>
      sendEmailToUser(addr, emails.reRegistrationAttempted)

    case Msg.PasswordResetRequested(addr, url) =>
      sendEmailToUser(addr, emails.passwordChangeRequest(url))
  }
}
