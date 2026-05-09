package shipreq.taskman.server.business

import cats.~>
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.logic.business.MailingList
import shipreq.taskman.server.logic.business.MailingList.API._
import shipreq.taskman.server.logic.business.MailingList._

/** An interpretter for [[MailingList.API]] that simply logs and does nothing. */
object MailingListNoOp extends (MailingList.API ~> Fx) with HasLogger {

  private def log[A](api: API[A]): Unit =
    logger.info("Ignoring " + api)

  override def apply[A](api: API[A]): Fx[A] =
    api match {
      case a: Subscribe      => Fx { log(a); Ok }
      case a: UpdateMember   => Fx { log(a); Ok }
      case a: BatchSubscribe => Fx { log(a) }
    }
}
