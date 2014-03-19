package shpireq.taskman.server

import org.joda.time.Period
import scalaz.~>
import scalaz.effect.{IO, MonadIO}
import shipreq.base.util.Error
import shipreq.taskman.api.Priority

sealed trait Op[A]

object Op {

  /**
   * Assigns msgs to the given node id, and retrieves them.
   *
   * @param limit The maximum number of msgs to assign and return.
   * @param minPriority All msgs will be at least this priority or higher.
   * @param assignmentTrustPeriod Period of time for which another node's assignment is respected.
   */
  case class GetMsgsAssignNode(n: NodeId, limit: Int, minPriority: Option[Priority], assignmentTrustPeriod: Period)
    extends Op[Seq[MsgHeader]]

  case class GetMsgAssignWorker(n: NodeId, w: WorkerId, m: MsgHeader) extends Op[Option[MsgDetail]]

  case class MarkMsgComplete(m: MsgDetail) extends Op[Unit]
  case class MsgFailedAbort(m: MsgDetail) extends Op[Unit]
  case class MsgFailedRetry(m: MsgDetail, p: Period) extends Op[Unit]

  case class NotifySupportWorkerFailed(m: MsgDetail, e: Error) extends Op[Unit]
  case class NotifySupportTaskmanError(e: Error, m: Option[MsgDetail]) extends Op[Unit]

  implicit class OpExt[A](val op: Op[A]) extends AnyVal {
    def toIO(implicit opToIo: Op ~> IO): IO[A] = opToIo(op)
    def toIOM[M[_]](implicit opToIo: Op ~> IO, m: MonadIO[M]): M[A] = toIO.liftIO[M]
  }
}
