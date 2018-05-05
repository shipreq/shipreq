package shipreq.taskman.server.business

import doobie.imports._
import java.time.Duration
import scalaz.{-\/, \/, \/-, ~>}
import scalaz.syntax.catchable._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.logic.business._
import shipreq.taskman.server.logic.business.BusinessOp._

final class BusinessOpFx(sendMailFx   : BusinessOp.SendEmail => Fx[Unit],
                         mailingListFx: MailingList.API ~> Fx,
                         supportFx    : Support.API ~> Fx,
                         db           : Transactor[Fx],
                         shipreqSchema: Option[String]) extends (BusinessOp ~> Fx) with HasLogger {

  private[this] val sri = ShipReqInterface(shipreqSchema)

  override def apply[A](op: BusinessOp[A]): Fx[A] =
    applyTimed(op)

  def applyTimed[A](op: BusinessOp[A]): Fx[A] =
    applyUntimed(op).attempt.measureDuration.flatMap { case (result, dur) =>
      logCompletion(op, result, dur).flatMap(_ => Fx.lift(result))
    }

  def logCompletion[A](op: BusinessOp[A], res: Throwable \/ A, dur: Duration): Fx[Unit] =
    Fx(res match {
      case \/-(_) => log.info(s"${simpleName(op)} completed in ${dur.toMillis}ms.")
      case -\/(e) => log.error(s"${simpleName(op)} failed after ${dur.toMillis}ms with [${e.getMessage}]. Op: $op")
    })

  def applyUntimed[A](bop: BusinessOp[A]): Fx[A] =
    bop match {
      case s: SendEmail              => sendMailFx(s)
      case MailingListOp(op)         => mailingListFx(op)
      case SupportOp(op)             => supportFx(op)
      case FindShipReqUser(-\/(id))  => db.trans(sri.findUserById(id))
      case FindShipReqUser(\/-(ea))  => db.trans(sri.findUserByEmail(ea))
      case FindShipReqUsers(None)    => db.trans(sri.findAllUsers)
      case FindShipReqUsers(Some(c)) => db.trans(sri.findAllUsersW(c))
    }
}

