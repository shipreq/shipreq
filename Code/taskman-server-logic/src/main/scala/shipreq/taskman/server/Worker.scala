package shipreq.taskman.server

import org.joda.time.DateTime
import scalaz.effect.IO
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.{-\/, \/, \/-, ~>}
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.log.HasLogger
import Sop._
import Worker._
import WorkResult._

object Worker {

  type MsgProcessor[F[_]] = MsgProcessorIn[F] => MsgProcessorOut[F]

  type AsyncScheduler[F[_]] = IO ~> ({type λ[α] = IO[F[α]]})#λ

  final class MsgProcessorIn[F[_]](val m: MsgDetail, _wrapAsync: => (IOE[Unit] => IO[WorkResult])) {
    private[this] lazy val wrapAsync = _wrapAsync

    @inline def sync(ioe: IOE[Unit])     : MsgProcessorOut[F] = \/-(ioe)
    @inline def sync(r: => ErrorOr[Unit]): MsgProcessorOut[F] = sync(IO(r))
    @inline def syncU(r: => Unit)        : MsgProcessorOut[F] = sync(ErrorOr(r))

    @inline def async(f: AsyncScheduler[F]) = asyncF(f(_))
    @inline def asyncF(f: IO[WorkResult] => IO[F[WorkResult]]): IOE[Unit] => MsgProcessorOut[F] =
      io => -\/(f(wrapAsync(io)))
  }

  final case class AsyncResult[F[_]](f: F[WorkResult], m: MsgDetail)

  type MsgProcessorOut[F[_]] = IO[F[WorkResult]] \/ IOE[Unit]

  // -------------------------------------------------------------------------------------------------------------------

  type FailurePolicy = FailureCtx => FailureResponse

  case class FailureCtx(m: MsgDetail, err: Error, now: DateTime)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[Sop[Unit]])

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult {
    final def toIO = IO(this)
  }

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    case class CouldntAssign(m: MsgHeader) extends WorkResult

    case class CouldntReAssign(m: MsgDetail) extends WorkResult

    /** Work completed successfully. */
    case class Completed(m: MsgDetail) extends WorkResult

    /** The worker business logic failed. */
    case class WorkerFailed(m: MsgDetail, e: Error, f: FailedJobReaction) extends WorkResult

    /** An error occurred in Taskman's generic work management. */
    case class TaskmanFailed(e: Error, m: Option[MsgDetail]) extends WorkResult
  }
}

// ---------------------------------------------------------------------------------------------------------------------

final class Worker[F[_]](msgProcessor: MsgProcessor[F])(
    implicit node: NodeId,
             worker: WorkerId,
             sopToIo: SopReifier,
             trustPeriod: AssignmentTrustPeriod,
             clock: IO[DateTime],
             failurePolicy: FailurePolicy
    ) extends HasLogger {

  // Output type of process()
  private type R = AsyncResult[F] \/ WorkResult

  def process(m: MsgHeader): IO[AsyncResult[F] \/ WorkResult] =
    catchTaskmanErrorsN(assign(m)) <| logWorkResult

  private[this] def catchExecErrors[A]: IO[A] => IO[ErrorOr[A]] =
    io => catchExecErrorsIOE(io map ErrorOr.apply)

  private[this] def catchExecErrorsIOE[A]: IO[ErrorOr[A]] => IO[ErrorOr[A]] =
    _.except(t => IO(ErrorOr error t))

  private[this] def catchTaskmanErrors[T](m: => Option[MsgDetail], ef: TaskmanFailed => IO[T]): IO[T] => IO[T] =
    _.except(t => {
      val e = Error(t)
      val notifySupport = clock >>= (t => sopToIo(NotifySupportTaskmanError(t, e, m)))
      notifySupport >> ef(TaskmanFailed(e, m))
    })

  private[this] def catchTaskmanErrorsR(m: => Option[MsgDetail]) = catchTaskmanErrors[R](m, f => IO(\/-(f)))
  private[this] def catchTaskmanErrorsWR(m: => Option[MsgDetail]) = catchTaskmanErrors[WorkResult](m, f => IO(f))
  private[this] val catchTaskmanErrorsN = catchTaskmanErrorsR(None)

  private[this] def assign(mh: MsgHeader): IO[R] =
    GetMsgAssignWorker(node, worker, mh).toIO >>= {
      case Some(m) => catchTaskmanErrorsR(Some(m))(logWorkStart(m) >> clock >>= performWork(m))
      case None    => IO(\/-(CouldntAssign(mh)))
    }

  private[this] def logWorkStart(md: MsgDetail): IO[Unit] =
    IO(log.debug.z(s"Starting work: $md"))

  private[this] def performWork(m: MsgDetail)(assignedSince: DateTime): IO[R] = {
    val r: MsgProcessorOut[F] =
      try msgProcessor(new MsgProcessorIn[F](m, wrapAsync(m, assignedSince)))
      catch {case t: Throwable => \/-(IO(ErrorOr error t))}
    r match {
      case \/-(io) =>
        catchExecErrorsIOE(io) flatMap taskEnd(m) map \/.right
      case -\/(io) =>
        catchExecErrors(io) >>= {
          case -\/(e) => handleTaskFailure(m, e) map \/.right
          case \/-(f) => IO(-\/(AsyncResult(f, m)))
        }
    }
  }

  private[this] def taskEnd(m: MsgDetail): ErrorOr[Unit] => IO[WorkResult] = {
    case \/-(_) => UpdateMsgSuccess(m).toIO >> Completed(m).toIO
    case -\/(e) => handleTaskFailure(m, e)
  }

  private[this] def handleTaskFailure(m: MsgDetail, e: Error): IO[WorkResult] =
    clock >>= handleTaskFailure2(m, e)

  private[this] def handleTaskFailure2(m: MsgDetail, e: Error)(now: DateTime): IO[WorkResult] = {
    val f = failurePolicy(FailureCtx(m, e, now))
    val addOps: IO[Unit] = f.additionalOps.traverse_(sopToIo)
    f.reaction.toIO >> addOps >> WorkerFailed(m, e, f.reaction).toIO
  }

  private[this] def logWorkResult(r: R): IO[Unit] = r match {
    case \/-(wr) =>
      logWorkResult(wr)
    case -\/(AsyncResult(_, m)) =>
      IO(log.debug.z(s"Scheduled to run asynchronously: $m"))
  }

  private[this] def logWorkResult(r: WorkResult): IO[Unit] =
    IO(r match {
      case CouldntAssign(m) =>
        log.debug.z(s"Couldn't assign: $m")
      case CouldntReAssign(m) =>
        log.warn.z(s"Couldn't reassign: $m")
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
    })

  private[this] def wrapAsync(m: MsgDetail, assignedSince: DateTime): IOE[Unit] => IO[WorkResult] =
    work =>
      catchTaskmanErrorsWR(Some(m))(
        reassignIfNeeded(m, assignedSince) >>= {
          case Some(r) => IO(r)
          case None    => performWorkF(m)(work)
        }
      ) <| logWorkResult

  private[this] val reassignmentOk: IO[Option[WorkResult]] = IO(None)
  private[this] def reassignIfNeeded(m: MsgDetail, assignedSince: DateTime): IO[Option[WorkResult]] =
    clock.flatMap(now =>
      if (now.isBefore(assignedSince plus trustPeriod.value))
        reassignmentOk
      else
        reassign(m)
      )

  private[this] def reassign(m: MsgDetail): IO[Option[WorkResult]] =
    ReAssignWorker(node, worker, m).toIO.map {
      case true  => None
      case false => Some(CouldntReAssign(m))
    }

  private[this] def performWorkF(m: MsgDetail): IOE[Unit] => IO[WorkResult] =
    work => catchExecErrorsIOE(work) >>= taskEnd(m) >>= upcast

  private[this] val upcast: WorkResult => IO[WorkResult] = IO[WorkResult](_)
}