package shipreq.taskman.server.business

import scalaz.effect.IO
import scalaz.{-\/, \/-}
import scala.slick.session.Database
import shipreq.base.util.ErrorOr
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.IoUtils
import Bop._

final class BopImpl(db: Database, emailer: EmailImpl, mailchimp: MailChimp, shipreqSchema: Option[String]) extends BopReifier with HasLogger {

  private[this] val shipreqSql = new ShipReqInterface.Sql(shipreqSchema)

  private[this] def shipreqDao[A](f: ShipReqInterface.Dao => A): IOE[A] =
    IOE(db.withSession(s => f(new ShipReqInterface.Dao(shipreqSql)(s))))

  override def apply[A](op: Bop[A]): IOE[A] =
    IoUtils.timeU(
      ErrorOr.catchExceptionM(applyOnly(op))
    )(logAfterWork(op))

  def logAfterWork[A](op: Bop[A]): ErrorOr[A] => Long => IO[Unit] =
    res => time => IO(
      res match {
        case \/-(_) =>
          log.info.z(s"${op.getClass.getSimpleName} completed in ${time}ms.")
        case -\/(e) =>
          log.error.z(s"${op.getClass.getSimpleName} failed after ${time}ms with [${e.msg}]. Op: $op")
      }
    )

  def applyOnly[A]: Bop[A] => IOE[A] = {
    case s: SendEmail               => emailer send s
    case MailingListOp(op)          => mailchimp run op
    case LookupShipReqUser(-\/(id)) => shipreqDao(_ userQueryById id)
    case LookupShipReqUser(\/-(ea)) => shipreqDao(_ userQueryByEmail ea)
  }
}

