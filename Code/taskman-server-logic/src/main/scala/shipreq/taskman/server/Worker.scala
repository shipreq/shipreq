package shipreq.taskman.server

import org.joda.time.DateTime
import scalaz.{-\/, \/-}
import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list.listInstance
import shipreq.base.util.{Logger, ErrorOr, Error}
import Sop._

object Worker extends Logger {

  type FailurePolicy = FailureCtx => FailureResponse

  case class FailureCtx(m: MsgDetail, err: Error, now: DateTime)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[Sop[Unit]])

  type MsgProcessor = MsgDetail => IOE[Unit]

  val nopResult = ErrorOr(())
  val nopTask: IOE[Unit] = IO(nopResult)

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult {
    val toIO = IO(this)
  }

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    case object CouldntAssign extends WorkResult

    /** Work completed successfully. */
    case class Completed(m: MsgDetail) extends WorkResult

    /** The worker business logic failed. */
    case class WorkerFailed(m: MsgDetail, e: Error, f: FailedJobReaction) extends WorkResult

    /** An error occurred in Taskman's generic work management. */
    case class TaskmanFailed(e: Error, m: Option[MsgDetail]) extends WorkResult
  }

  import WorkResult._

  // -------------------------------------------------------------------------------------------------------------------

  case class Reified(
    implicit node: NodeId,
             worker: WorkerId,
             sopToIo: SopReifier,
             clock: IO[DateTime],
             failurePolicy: FailurePolicy,
             msgProcessor: MsgProcessor) {

    private[this] def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t => {
        val e = Error.error(t)
        sopToIo(NotifySupportTaskmanError(e, m)) >> TaskmanFailed(e, m).toIO
      })

    private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

    private[this] def performWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchExceptionM(msgProcessor(m)) >>= {
        case \/-(_) => MarkMsgComplete(m).toIO >> Completed(m).toIO
        case -\/(e) => clock >>= handleTaskFailure(m, e)
      }

    private[this] def handleTaskFailure(m: MsgDetail, err: Error)(now: DateTime): IO[WorkResult] = {
      val f = failurePolicy(FailureCtx(m, err, now))
      f.reaction.toIO >>
        f.additionalOps.traverse_(sopToIo) >>
          WorkerFailed(m, err, f.reaction).toIO
    }

    private[this] val processAssignment: Option[MsgDetail] => IO[WorkResult] = {
      case Some(m) => catchTaskmanErrors(Some(m))(performWork(m))
      case None    => CouldntAssign.toIO
    }

    def process(m: MsgHeader): IO[WorkResult] =
      catchTaskmanErrorsN(
        GetMsgAssignWorker(node, worker, m).toIO >>= processAssignment)

    def logWorkResult(r: WorkResult): IO[WorkResult] = IO{
      r match {
        case CouldntAssign =>
        case Completed(m) =>
          log.info("Work completed: {}", m)
        case WorkerFailed(_, e, f) =>
          // f contains m so no need to print separately
          if (e is Deliberate)
            log.debug("Worker deliberately failed: {} // {}", e.msg, f, null)
          else
            log.warn(s"Worker failed: $f", e.throwable)
        case TaskmanFailed(e, Some(m)) =>
          log.error(s"Taskman error occurred processing $m", e.throwable)
        case TaskmanFailed(e, None) =>
          log.error(s"Taskman error occurred! (no msg)", e.throwable)
      }
      r
    }

    def processL(m: MsgHeader): IO[WorkResult] =
      process(m) >>= logWorkResult
  }
}
