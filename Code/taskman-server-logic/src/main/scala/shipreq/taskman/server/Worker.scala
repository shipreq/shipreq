package shipreq.taskman.server

import org.joda.time.DateTime
import scalaz.{-\/, \/-, ~>}
import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list.listInstance
import shipreq.base.util.{ErrorOr, Error}
import shipreq.taskman.api.Msg
import Sop._

object Worker {

  type FailurePolicy = FailureCtx => FailureResponse

  case class FailureCtx(m: MsgDetail, err: Error, now: DateTime)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[Sop[Unit]])

  type MsgProcessor = Msg => IOE[Unit]

  val nopTask: IOE[Unit] = IO(ErrorOr(()))

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult {
    val toIO = IO(this)
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
             sopToIo: Sop ~> IO,
             clock: IO[DateTime],
             failurePolicy: FailurePolicy,
             msgProcessor: MsgProcessor) {

    private[this] def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t =>
        sopToIo(NotifySupportTaskmanError(Error.error(t), m)) >> TaskmanFailed.toIO)

    private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

    private[this] def performWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchExceptionM(msgProcessor(m.msg)) >>= {
        case \/-(_) =>
          MarkMsgComplete(m).toIO >> Completed.toIO
        case -\/(err) =>
          clock >>= handleTaskFailure(m, err)
      }

    private[this] def handleTaskFailure(m: MsgDetail, err: Error)(now: DateTime): IO[WorkResult] = {
      val f = failurePolicy(FailureCtx(m, err, now))
      f.reaction.toIO >> f.additionalOps.traverse_(sopToIo) >> WorkerFailed.toIO
    }

    private[this] val processAssignment: Option[MsgDetail] => IO[WorkResult] = {
      case Some(m) => catchTaskmanErrors(Some(m))(performWork(m))
      case None    => CouldntAssign.toIO
    }

    def process(m: MsgHeader): IO[WorkResult] =
      catchTaskmanErrorsN(
        GetMsgAssignWorker(node, worker, m).toIO >>= processAssignment)
  }
}
