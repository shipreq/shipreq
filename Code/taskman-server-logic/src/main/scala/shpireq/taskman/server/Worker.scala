package shpireq.taskman.server

import scalaz.{-\/, \/-, ~>}
import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list.listInstance
import shipreq.base.util.{ErrorOr, Error}
import shipreq.taskman.api.Msg
import Sop._

object Worker {

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailurePolicyR(reaction: FailedJobReaction, additionalOps: List[Sop[Unit]])

  type FailurePolicy = MsgDetail => Error => FailurePolicyR

  type MsgProcessor = Msg => IO[ErrorOr[Unit]]

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult {
    val io = IO(this)
  }

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    case object CouldntAssign extends WorkResult

    /** Work completed successfully. */
    case object Completed extends WorkResult

    /** The worker business logic failed. */
    case object WorkerFailed extends WorkResult

    /** An error occurred in Taskman's generic work management. */
    case object TaskmanFailed extends WorkResult
  }

  import WorkResult._

  // -------------------------------------------------------------------------------------------------------------------

  case class Reified(worker: WorkerId)(
    implicit node: NodeId,
             opToIo: Sop ~> IO,
             failurePolicy: FailurePolicy,
             msgProcessor: MsgProcessor) {

    private[this] def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t =>
        opToIo(NotifySupportTaskmanError(Error.error(t), m)) >> TaskmanFailed.io)

    private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

    private[this] def performWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchExceptionM(msgProcessor(m.m)) >>= {
        case \/-(_) =>
          MarkMsgComplete(m).toIO >> Completed.io
        case -\/(err) =>
          val FailurePolicyR(f, extra) = failurePolicy(m)(err)
          f.toIO >> extra.traverse_(opToIo) >> WorkerFailed.io
      }

    private[this] val processAssignment: Option[MsgDetail] => IO[WorkResult] = {
      case Some(m) => catchTaskmanErrors(Some(m))(performWork(m))
      case None    => CouldntAssign.io
    }

    def process(m: MsgHeader): IO[WorkResult] =
      catchTaskmanErrorsN(
        GetMsgAssignWorker(node, worker, m).toIO >>= processAssignment)

  }
}
