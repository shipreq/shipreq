package shipreq.taskman.server

import org.joda.time.Period
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
  case class GetMsgsAssignNode(n: NodeId, batchSize: Int, assignmentTrustPeriod: Period, qs: Option[(Priority, Int)])
    extends Sop[Seq[MsgHeader]]

  case class GetMsgAssignWorker(n: NodeId, w: WorkerId, m: MsgHeader) extends Sop[Option[MsgDetail]]

  case class UpdateMsgSuccess(m: MsgDetail) extends Sop[Unit]
  case class UpdateMsgRetry(m: MsgDetail) extends FailedJobReaction
  case class UpdateMsgAbort(m: MsgDetail, delay: Period) extends FailedJobReaction

  case class NotifySupportWorkerFailed(m: MsgDetail, e: Error) extends Sop[Unit]
  case class NotifySupportTaskmanError(e: Error, m: Option[MsgDetail]) extends Sop[Unit]

  case object Nop extends Sop[Unit]
}
