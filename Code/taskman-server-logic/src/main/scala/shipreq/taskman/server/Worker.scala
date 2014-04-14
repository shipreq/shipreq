package shipreq.taskman.server

import org.joda.time.DateTime
import scalaz.effect.IO
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.{-\/, \/-}
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.log.HasLogger
import Sop._

object Worker extends HasLogger {

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
    case class CouldntAssign(m: MsgHeader) extends WorkResult

    /** Work completed successfully. */
    case class Completed(m: MsgDetail) extends WorkResult

    /** The worker business logic failed. */
    case class WorkerFailed(m: MsgDetail, e: Error, f: FailedJobReaction) extends WorkResult

    /** An error occurred in Taskman's generic work management. */
    case class TaskmanFailed(e: Error, m: Option[MsgDetail]) extends WorkResult
  }

  import WorkResult._

  // -------------------------------------------------------------------------------------------------------------------

  final case class Reified(
    implicit node: NodeId,
             worker: WorkerId,
             sopToIo: SopReifier,
             clock: IO[DateTime],
             failurePolicy: FailurePolicy,
             msgProcessor: MsgProcessor) {

    def process(m: MsgHeader): IO[WorkResult] =
      catchTaskmanErrorsN(assign(m)) >>= logWorkResult

    private[this] def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t => {
        val e = Error.error(t)
        val notifySupport = clock >>= (t => sopToIo(NotifySupportTaskmanError(t, e, m)))
        notifySupport >> TaskmanFailed(e, m).toIO
      })

    private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

    private[this] def assign(mh: MsgHeader): IO[WorkResult] =
      GetMsgAssignWorker(node, worker, mh).toIO >>= {
        case Some(m) => catchTaskmanErrors(Some(m))(logWorkStart(m) >> performWork(m))
        case None    => CouldntAssign(mh).toIO
      }

    private[this] def logWorkStart(md: MsgDetail): IO[Unit] =
      IO(log.debug.z(s"Starting work: $md"))

    private[this] def performWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchExceptionM(msgProcessor(m)) >>= {
        case \/-(_) => UpdateMsgSuccess(m).toIO >> Completed(m).toIO
        case -\/(e) => clock >>= handleTaskFailure(m, e)
      }

    private[this] def handleTaskFailure(m: MsgDetail, err: Error)(now: DateTime): IO[WorkResult] = {
      val f = failurePolicy(FailureCtx(m, err, now))
      val addOps: IO[Unit] = f.additionalOps.traverse_(sopToIo)
      f.reaction.toIO >> addOps >> WorkerFailed(m, err, f.reaction).toIO
    }

    private[this] def logWorkResult(r: WorkResult): IO[WorkResult] = IO{
      r match {
        case CouldntAssign(m) =>
          log.debug.z(s"Couldn't assign: $m")
        case Completed(m) =>
          log.info.z(s"Successfully completed: $m")
        case WorkerFailed(_, e, f) =>
          // f contains m so no need to print separately
          if (e is Deliberate)
            log.info.z(s"Worker deliberately failed: ${e.msg} // $f")
          else
            log.error(e, s"Worker failed: $f")
        case TaskmanFailed(e, Some(m)) =>
          log.error(e, s"Taskman error occurred processing $m")
        case TaskmanFailed(e, None) =>
          log.error(e, "Taskman error occurred! (no msg)")
      }
      r
    }
  }
}
