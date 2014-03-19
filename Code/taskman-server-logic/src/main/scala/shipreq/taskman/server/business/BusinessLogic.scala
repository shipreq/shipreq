package shipreq.taskman.server.business

import scalaz.{~>, \/-}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ErrorOr
import shipreq.taskman.server._
import shipreq.taskman.api.Msg
import BusinessLogic._

object BusinessLogic {
  type Task[A] = IO[ErrorOr[A]]

  private[this] val nop: Task[Unit] = IO(\/-(()))

  implicit class BopExt[A](val op: Bop[A]) extends AnyVal {
    def toTask(implicit opToIo: Bop ~> IO): Task[A] = op.toIO.map(\/-(_))
    def toTaskU(implicit opToIo: Bop ~> IO, ev: A =:= Unit): Task[Unit] = op.toIO >> nop
  }

  implicit def autoBopToTaskU(b: Bop[Unit])(implicit bopToIo: Bop ~> IO): Task[Unit] = b.toTaskU
}

class BusinessLogic(implicit ctx: Email.Ctx, bopToIo: Bop ~> IO) {

  val email = new Emails(ctx)

  val msgProcessor: Msg => Task[Unit] = {

    case Msg.RegistrationRequested(addr, url) =>
      email.sendToUser(addr, email.linkToCompleteRegistration(url))

    case Msg.ReRegistrationAttempted(addr) =>
      email.sendToUser(addr, email.reRegistrationAttempted)

    case Msg.PasswordResetRequested(addr, url) =>
      email.sendToUser(addr, email.passwordChangeRequest(url))
  }
}
