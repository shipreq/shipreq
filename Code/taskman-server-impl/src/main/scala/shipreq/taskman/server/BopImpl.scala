package shipreq.taskman.server

import scalaz.{-\/, \/-}
import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Logger}
import shipreq.taskman.server.business.{BopReifier, Bop}
import Bop._
import BopImpl._

object BopImpl {
  trait Ctx {
    def emailer: EmailImpl
  }
}

final class BopImpl(ctx: Ctx) extends BopReifier with Logger {
  import ctx._

  override def apply[A](op: Bop[A]): IOE[A] =
    IoUtils.timeU(
      ErrorOr.catchExceptionM(applyOnly(op))
    )(logAfterWork(op))

  def logAfterWork[A](op: Bop[A]): ErrorOr[A] => Long => IO[Unit] =
    res => time => IO(
      res match {
        case \/-(_) =>
          log.info("{} completed in {}ms.", op.getClass.getSimpleName, time)
        case -\/(e) =>
          log.error("{} failed after {}ms with [{}]. Op: {}",
            op.getClass.getSimpleName, java.lang.Long.valueOf(time), e.msg, op)
      }
    )

  def applyOnly[A](op: Bop[A]): IOE[A] =
    op match {
      case s: SendEmail => emailer.send(s)
    }
}

