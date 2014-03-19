package shpireq.taskman.server

import org.joda.time.Period
import shipreq.base.util.Error
import shipreq.taskman.api.Priority

/**
 * Server Operation.
 * An operation in the domain of taskman-server, rather than taskman-api or any business logic.
 */
sealed trait Sop[A]

object Sop {

  /**
   * Assigns msgs to the given node id, and retrieves them.
   *
   * @param limit The maximum number of msgs to assign and return.
   * @param minPriority All msgs will be at least this priority or higher.
   * @param assignmentTrustPeriod Period of time for which another node's assignment is respected.
   */
  case class GetMsgsAssignNode(n: NodeId, limit: Int, minPriority: Option[Priority], assignmentTrustPeriod: Period)
    extends Sop[Seq[MsgHeader]]

  case class GetMsgAssignWorker(n: NodeId, w: WorkerId, m: MsgHeader) extends Sop[Option[MsgDetail]]

  case class MarkMsgComplete(m: MsgDetail) extends Sop[Unit]
  case class MsgFailedAbort(m: MsgDetail) extends Sop[Unit]
  case class MsgFailedRetry(m: MsgDetail, p: Period) extends Sop[Unit]

  case class NotifySupportWorkerFailed(m: MsgDetail, e: Error) extends Sop[Unit]
  case class NotifySupportTaskmanError(e: Error, m: Option[MsgDetail]) extends Sop[Unit]
}
