package shipreq.taskman.server.logic

import java.time.{Duration, Instant}
import shipreq.base.util.ArticulateError
import shipreq.taskman.api.Priority

/**
 * Server Operation.
 * An operation in the domain of taskman-server, rather than taskman-api or any business logic.
 */
sealed trait ServerOp[A]

/** Represents an operation to handle a failed job. */
sealed trait FailedJobReaction extends ServerOp[Unit]

object ServerOp {

  /** Loads a configuration value.
    *
    * @param key The config key.
    */
  final case class CfgGet(key: String) extends ServerOp[Option[String]]

  /** Assigns msgs to the given node id, and retrieves them.
    *
    * @param batchSize             The maximum number of msgs to assign and return.
    * @param assignmentTrustPeriod Period of time for which another node's assignment is respected.
    * @param queueStatus           The highest priority msg in, and size of the in-memory queue.
    */
  final case class GetMsgsAssignNode(nodeId               : NodeId,
                                     batchSize            : Int,
                                     assignmentTrustPeriod: Duration,
                                     queueStatus          : Option[(Priority, Int)]) extends ServerOp[List[MsgHeader]]

  final case class GetMsgAssignWorker(nodeId  : NodeId,
                                      workerId: WorkerId,
                                      mh      : MsgHeader) extends ServerOp[Option[MsgDetail]]

  /** Result = true if worker was successfully reassigned. */
  final case class ReassignWorker(nodeId  : NodeId,
                                  workerId: WorkerId,
                                  md      : MsgDetail) extends ServerOp[Boolean]

  final case class UpdateMsgSuccess(nodeId  : NodeId,
                                    workerId: WorkerId,
                                    md      : MsgDetail) extends ServerOp[Unit]

  final case class UpdateMsgRetry(nodeId  : NodeId,
                                  workerId: WorkerId,
                                  md      : MsgDetail,
                                  delay   : Duration) extends FailedJobReaction

  final case class UpdateMsgAbort(nodeId  : NodeId,
                                  workerId: WorkerId,
                                  md      : MsgDetail) extends FailedJobReaction

  final case class NotifySupportWorkerFailed(when: Instant,
                                             md  : MsgDetail,
                                             err : ArticulateError) extends ServerOp[Unit]

  final case class NotifySupportTaskmanError(when: Instant,
                                             err : ArticulateError,
                                             md  : Option[MsgDetail]) extends ServerOp[Unit]

  case object Nop extends ServerOp[Unit]

}
