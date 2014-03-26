package shipreq.taskman.server.business

import shipreq.taskman.api.Msg
import shipreq.taskman.server.Worker.MsgProcessor

object BusinessLogic {

  def apply(ctx: Email.Ctx, reifier: BopReifier): MsgProcessor = {

    val email = new Emails(ctx)

    implicit def autoReifyBop(bop: Bop[Unit]) = reifier(bop)

    val m: MsgProcessor = {

      case Msg.RegistrationRequested(addr, url) =>
        email.sendToUser(addr, email.linkToCompleteRegistration(url))

      case Msg.ReRegistrationAttempted(addr) =>
        email.sendToUser(addr, email.reRegistrationAttempted)

      case Msg.PasswordResetRequested(addr, url) =>
        email.sendToUser(addr, email.passwordChangeRequest(url))

    }
    m
  }
}

