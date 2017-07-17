package shipreq.taskman.server.business

import doobie.imports._
import java.time.Duration
import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorOr
import shipreq.base.util.FxModule._
import shipreq.base.util.effect._
import shipreq.base.util.log.HasLogger
import Bop._

final class BopImpl(db: Transactor[Fx],
                    emailer: EmailImpl,
                    mailchimp: MailChimp,
                    freshDesk: FreshDesk,
                    shipreqSchema: Option[String]) extends BopReifier with HasLogger {

  private[this] val sri = new ShipReqInterface(shipreqSchema)

  private[this] def dbio[A](q: ConnectionIO[A]): FxE[A] =
    db.trans(q).map(\/-(_))

  override def apply[A](op: Bop[A]): FxE[A] = applyTimed(op)

  def applyTimed[A](op: Bop[A]): FxE[A] =
    for {
      x <- applyUntimed(op).measureDuration
      (r, dur) = x
      _ <- logCompletion(op, r, dur)
    } yield r

  def logCompletion[A](op: Bop[A], res: ErrorOr[A], dur: Duration): Fx[Unit] =
    Fx(res match {
      case \/-(_) => log.info.z(s"${simpleName(op)} completed in ${dur.toMillis}ms.")
      case -\/(e) => log.error.z(s"${simpleName(op)} failed after ${dur.toMillis}ms with [${e.msg}]. Op: $op")
    })

  def applyUntimed[A](op: Bop[A]): FxE[A] =
    ErrorOr.catchExceptionM(op match {
      case s: SendEmail              => emailer.send(s)
      case MailingListOp(op)         => mailchimp.run(op)
      case SupportOp(op)             => freshDesk.run(op)
      case FindShipReqUser(-\/(id))  => dbio(sri.findUserById(id))
      case FindShipReqUser(\/-(ea))  => dbio(sri.findUserByEmail(ea))
      case FindShipReqUsers(None)    => dbio(sri.findAllUsers)
      case FindShipReqUsers(Some(c)) => dbio(sri.findAllUsersW(c))
    })
}

