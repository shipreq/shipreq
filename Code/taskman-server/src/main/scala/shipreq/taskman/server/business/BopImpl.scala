package shipreq.taskman.server.business

import doobie.imports._
import scalaz.{-\/, \/-}
import scalaz.effect.IO
import shipreq.base.util.ErrorOr
import shipreq.base.util.effect.{IOE, IoUtils}
import shipreq.base.util.log.HasLogger
import Bop._

final class BopImpl(db: Transactor[IO],
                    emailer: EmailImpl,
                    mailchimp: MailChimp,
                    freshDesk: FreshDesk,
                    shipreqSchema: Option[String]) extends BopReifier with HasLogger {

  private[this] val sri = new ShipReqInterface(shipreqSchema)

  private[this] def dbio[A](q: ConnectionIO[A]): IOE[A] =
    db.trans(q).map(\/-(_))

  override def apply[A](op: Bop[A]): IOE[A] = applyTimed(op)

  def applyTimed[A](op: Bop[A]): IOE[A] =
    IoUtils.time_(applyUntimed(op))(logCompletion(op))

  def logCompletion[A](op: Bop[A]): ErrorOr[A] => Long => IO[Unit] =
    res => time => IO(
      res match {
          case \/-(_) =>
            log.info.z(s"${simpleName(op)} completed in ${time}ms.")
          case -\/(e) =>
            log.error.z(s"${simpleName(op)} failed after ${time}ms with [${e.msg}]. Op: $op")
        }
      )

  def applyUntimed[A](op: Bop[A]): IOE[A] =
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

