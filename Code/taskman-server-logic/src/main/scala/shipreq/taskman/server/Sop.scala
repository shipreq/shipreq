package shipreq.taskman.server

import java.time.{Duration, Instant}
import shipreq.base.util.Error
import shipreq.taskman.api.Priority

/**
 * Server Operation.
 * An operation in the domain of taskman-server, rather than taskman-api or any business logic.
 */
sealed trait Sop[A]

/** Represents an operation to handle a failed job. */
sealed trait FailedJobReaction extends Sop[Unit]

object Sop {

  /**
   * Loads a configuration value.
   *
   * @param k The config key.
   */
  case class CfgGet(k: String) extends Sop[Option[String]]

  /**
   * Assigns msgs to the given node id, and retrieves them.
   *
   * @param batchSize The maximum number of msgs to assign and return.
   * @param assignmentTrustPeriod Period of time for which another node's assignment is respected.
   * @param qs Queue status: The highest priority msg in, and size of the in-memory queue.
   */
  case class GetMsgsAssignNode(n: NodeId, batchSize: Int, assignmentTrustPeriod: Duration, qs: Option[(Priority, Int)])
    extends Sop[List[MsgHeader]]

  case class GetMsgAssignWorker(n: NodeId, w: WorkerId, m: MsgHeader) extends Sop[Option[MsgDetail]]

  case class ReAssignWorker(n: NodeId, w: WorkerId, m: MsgDetail) extends Sop[Boolean]

  case class UpdateMsgSuccess(n: NodeId, w: WorkerId, m: MsgDetail) extends Sop[Unit]
  case class UpdateMsgRetry(n: NodeId, w: WorkerId, m: MsgDetail, delay: Duration) extends FailedJobReaction
  case class UpdateMsgAbort(n: NodeId, w: WorkerId, m: MsgDetail) extends FailedJobReaction

  case class NotifySupportWorkerFailed(t: Instant, m: MsgDetail, e: Error) extends Sop[Unit]
  case class NotifySupportTaskmanError(t: Instant, e: Error, m: Option[MsgDetail]) extends Sop[Unit]

  case object Nop extends Sop[Unit]
}
