package shipreq.taskman.server.business

import scalaz.effect.IO
import scalaz.{-\/, \/-}
import scala.slick.jdbc.JdbcBackend.Database
import shipreq.base.util.ErrorOr
import shipreq.base.util.effect.{IoUtils, IOE}
import shipreq.base.util.log.HasLogger
import Bop._

final class BopImpl(db: Database,
                    emailer: EmailImpl,
                    mailchimp: MailChimp,
                    freshDesk: FreshDesk,
                    shipreqSchema: Option[String]) extends BopReifier with HasLogger {

  private[this] val shipreqSql = new ShipReqInterface.Sql(shipreqSchema)

  private[this] def shipreqDao[A](f: ShipReqInterface.Dao => A): IOE[A] =
    IOE(db.withSession(s => f(new ShipReqInterface.Dao(shipreqSql)(s))))

  override def apply[A](op: Bop[A]): IOE[A] = applyUntimed(op)

  def applyTimed[A](op: Bop[A]): IOE[A] =
    IoUtils.time_(applyUntimed(op))(logCompletion(op))

  def logCompletion[A](op: Bop[A]): ErrorOr[A] => Long => IO[Unit] =
    res => time => IO(
      res match {
        case \/-(_) =>
          log.info.z(s"${op.getClass.getSimpleName} completed in ${time}ms.")
        case -\/(e) =>
          log.error.z(s"${op.getClass.getSimpleName} failed after ${time}ms with [${e.msg}]. Op: $op")
      }
    )

  def applyUntimed[A](op: Bop[A]): IOE[A] =
    ErrorOr.catchExceptionM(op match {
      case s: SendEmail              => emailer send s
      case MailingListOp(op)         => mailchimp run op
      case SupportOp(op)             => freshDesk run op
      case FindShipReqUser(-\/(id))  => shipreqDao(_ findUser id)
      case FindShipReqUser(\/-(ea))  => shipreqDao(_ findUser ea)
      case FindShipReqUsers(None)    => shipreqDao(_.findAllUsers())
      case FindShipReqUsers(Some(c)) => shipreqDao(_ findAllUsers c)
    })
}

