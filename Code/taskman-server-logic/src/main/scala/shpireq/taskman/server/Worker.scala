package shpireq.taskman.server

import org.joda.time.Period
import scalaz.{-\/, \/-, ~>}
import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list.listInstance
import shipreq.base.util.{ErrorOr, Error}
import shipreq.taskman.api.Msg
import Sop._

class Worker {

  /** Represents how to handle a failed job. */
  sealed trait FailedJobReaction

  case class Retry(delay: Period) extends FailedJobReaction

  case object Abort extends FailedJobReaction

  type FailurePolicy = MsgDetail => Error => (FailedJobReaction, List[Sop[Unit]])

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult {
    val io = IO(this)
  }

  /** Unable to assign the worker to the job. Someone else must've taken it. */
  case object CouldntAssign extends WorkResult

  /** Work completed successfully. */
  case object Completed extends WorkResult

  /** The worker business logic failed. */
  case object WorkerFailed extends WorkResult

  /** An error occurred in Taskman's generic work management. */
  case object TaskmanFailed extends WorkResult

  // -------------------------------------------------------------------------------------------------------------------

  case class Reified(worker: WorkerId)(
    implicit node: NodeId,
             opToIo: Sop ~> IO,
             jfToIo: FailedJobReaction => IO[Unit],
             failurePolicy: FailurePolicy,
             msgProcessor: Msg => IO[ErrorOr[Unit]]) {

    private[this] def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t =>
        opToIo(NotifySupportTaskmanError(Error.error(t), m)) >> TaskmanFailed.io)

    private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

    private[this] def performWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchExceptionM(msgProcessor(m.m)) >>= {
        case \/-(_) =>
          MarkMsgComplete(m).toIO >> Completed.io
        case -\/(err) =>
          val (jf, extra) = failurePolicy(m)(err)
          jfToIo(jf) >> extra.traverse_(opToIo) >> WorkerFailed.io
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
