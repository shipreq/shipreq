package shipreq.taskman.server.business

import doobie.ConnectionIO
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import scalaz.~>
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.logic.business.BusinessOp._
import shipreq.taskman.server.logic.business._

final class BusinessOpFx(sendMailFx   : BusinessOp.SendEmail => Fx[Unit],
                         mailingListFx: MailingList.API ~> Fx,
                         supportFx    : Support.API ~> Fx,
                         dbRun        : ConnectionIO ~> Fx,
                         shipreqSchema: Option[String]) extends (BusinessOp ~> Fx) with HasLogger {

  private[this] val sri = ShipReqInterface(shipreqSchema)

  override def apply[A](op: BusinessOp[A]): Fx[A] =
    applyTimed(op)

  def applyTimed[A](op: BusinessOp[A]): Fx[A] =
    applyUntimed(op).attemptFx.measureDuration.flatMap { case (result, dur) =>
      logCompletion(op, result, dur).flatMap(_ => Fx.lift(result))
    }

  def logCompletion[A](op: BusinessOp[A], res: Throwable \/ A, dur: Duration): Fx[Unit] =
    Fx(res match {
      case \/-(_) => logger.info(s"${simpleName(op)} completed in ${dur.conciseDesc}.")
      case -\/(e) => logger.error(s"${simpleName(op)} failed after ${dur.conciseDesc} with [${e.getMessage}]. Op: $op")
    })

  def applyUntimed[A](bop: BusinessOp[A]): Fx[A] =
    bop match {
      case s: SendEmail              => sendMailFx(s)
      case MailingListOp(op)         => mailingListFx(op)
      case SupportOp(op)             => supportFx(op)
      case FindShipReqUser(-\/(id))  => dbRun(sri.findUserById(id))
      case FindShipReqUser(\/-(ea))  => dbRun(sri.findUserByEmail(ea))
      case FindShipReqUsers(None)    => dbRun(sri.findAllUsers)
      case FindShipReqUsers(Some(c)) => dbRun(sri.findAllUsersW(c))
    }
}

