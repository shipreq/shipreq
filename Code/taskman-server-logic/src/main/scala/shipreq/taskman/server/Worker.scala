package shipreq.taskman.server

import org.joda.time.DateTime
import scalaz.{-\/, \/, \/-, ~>}
import scalaz.effect.IO
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.effect.{IoUtils, IOE}
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{Priority => MsgPriority}
import shipreq.taskman.server.business.{Email, Support, BopReifier, Emails}
import shipreq.taskman.server.business.Bop.SupportOp
import ErrorOr.Implicits.MonadExt
import Sop._

object Worker extends HasLogger {

  type MsgProcessor[F[_]] = MsgDetail => MsgProcessorOut[F]

  type MsgProcessorOut[F[_]] = IOE[ProcessorResult[F]]

  type AsyncScheduler[F[_]] = IO ~> ({type λ[α] = IOE[F[α]]})#λ

  /** Legal responses from a MsgProcessor to a Worker when told to process a msg. */
  sealed trait ProcessorResult[+F[_]]

  object ProcessorResult {

    /** Work is complete. Nothing left to do. */
    case object Complete extends ProcessorResult[Nothing]

    /** Schedule for async processing. */
    case class Schedule[F[_]](s: AsyncScheduler[F], w: IOE[ProcessorResult[F]]) extends ProcessorResult[F]
  }

  // -------------------------------------------------------------------------------------------------------------------

  type FailurePolicy = FailureCtx => FailureResponse

  case class FailureCtx(n: NodeId, w: WorkerId, m: MsgDetail, err: Error, now: DateTime)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[Sop[Unit]])

  def priorityForWorkerFailure(mp: MsgPriority): Support.Priority =
    mp.value match {
      case p if p >= MsgPriority.High.value   => Support.Priority.Urgent
      case p if p >= MsgPriority.Medium.value => Support.Priority.High
      case _                                  => Support.Priority.Medium
    }

  final class FailureHandler(emails: Emails, bopReifier: BopReifier) {

    def raise(c: Email.Content, p: Support.Priority): IOE[Unit] = {
      val io1 = bopReifier(SupportOp(Support.API.ReportFailure(c.subject, c.body, p)))
      val io2 = emails archive c map (bopReifier(_)) getOrElse IOE.nop
      io1 execMap io2
    }

    def run(catchIo: Error => IO[Unit])(f: => IOE[Unit]): IO[Unit] =
      try
        IOE.safeExec(catchIo)(f)
      catch {
        case t: Throwable => catchIo(Error(t))
      }

    def handleFailedWorker(f: NotifySupportWorkerFailed): IO[Unit] = {
      val catchIo: Error => IO[Unit] =
        e2 => IO(
          log.error(s"""FAILED TO NOTIFY SUPPORT OF FAILED WORKER.
                Notification error: ${e2.stackTraceStr}
                Worker error: ${f.e.stackTraceStr}
                Msg: ${f.m}""")
        ) >> handleFailedTaskman(NotifySupportTaskmanError(f.t, e2, Some(f.m)))
      run(catchIo)(
        raise(emails.workerFailureEmail(f.t, f.m, f.e), priorityForWorkerFailure(f.m.priority))
      )
    }

    def handleFailedTaskman(f: NotifySupportTaskmanError): IO[Unit] = {
      val catchIo: Error => IO[Unit] =
        e2 => IO(
          log.error(s"""FAILED TO NOTIFY SUPPORT OF TASKMAN FAILURE. FUCK.
              Notification error: ${e2.stackTraceStr}
              Original error: ${f.e.stackTraceStr}
              Msg: ${f.m}""")
        )
      run(catchIo)(
        raise(emails.taskmanErrorEmail(f.t, f.e, f.m), Support.Priority.Urgent)
      )
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult[+F[_]]

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    case class CouldntAssign(m: MsgHeader) extends WorkResult[Nothing]

    case class CouldntReAssign(m: MsgDetail) extends WorkResult[Nothing]

    /** Work completed successfully. */
    case class Completed(m: MsgDetail) extends WorkResult[Nothing]

    /** The worker business logic failed. */
    case class WorkerFailed(m: MsgDetail, e: Error, f: FailedJobReaction) extends WorkResult[Nothing]

    /** An error occurred in Taskman's generic work management. */
    case class TaskmanFailed(e: Error, m: Option[MsgDetail]) extends WorkResult[Nothing]

    case class Scheduled[F[_]](f: F[WorkResult[F]], m: MsgDetail) extends WorkResult[F]
  }
}

// ---------------------------------------------------------------------------------------------------------------------
import Worker._
import WorkResult._

final class Worker[F[_]](msgProcessor: MsgProcessor[F])(
    implicit node: NodeId,
             worker: WorkerId,
             sopToIo: SopReifier,
             trustPeriod: AssignmentTrustPeriod,
             clock: IO[DateTime],
             failurePolicy: FailurePolicy
    ) extends HasLogger {

  def process(m: MsgHeader): IO[WorkResult[F]] =
    IoUtils.time_(catchTaskmanErrorsN(assign(m)))(logWorkResult)

  private[this] def catchExecErrorsIOE[A]: IOE[A] => IOE[A] =
    _.except(IOE error _)

  private[this] def catchTaskmanErrorsG[T](m: => Option[MsgDetail], ef: TaskmanFailed => IO[T]): IO[T] => IO[T] =
    _.except(t => {
      val e = Error(t)
      val notifySupport = clock >>= (t => sopToIo(NotifySupportTaskmanError(t, e, m)))
      notifySupport >> ef(TaskmanFailed(e, m))
    })

  private[this] def catchTaskmanErrors(m: => Option[MsgDetail]) = catchTaskmanErrorsG[WorkResult[F]](m, f => IO(f))
  private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

  private[this] def assign(mh: MsgHeader): IO[WorkResult[F]] =
    GetMsgAssignWorker(node, worker, mh).toIO >>= {
      case Some(m) => catchTaskmanErrors(Some(m))(logWorkStart(m) >> clock >>= performWork(m))
      case None    => IO(CouldntAssign(mh))
    }

  private[this] def logWorkStart(md: MsgDetail): IO[Unit] =
    IO(log.debug.z(s"Starting work: $md"))

  private[this] def performWork(m: MsgDetail)(assignedSince: DateTime): IO[WorkResult[F]] = {
    val io: MsgProcessorOut[F] =
      try catchExecErrorsIOE(msgProcessor(m)) catch {case t: Throwable => IOE error t}
    io flatMap taskEnd(m, assignedSince)
  }

  private[this] def taskEnd(m: MsgDetail, assignedSince: DateTime): ErrorOr[ProcessorResult[F]] => IO[WorkResult[F]] = {
    case \/-(ProcessorResult.Complete) =>
      UpdateMsgSuccess(node, worker, m).toIO >> IO(Completed(m))

    case \/-(ProcessorResult.Schedule(schedule, w)) =>
      schedule(wrapAsync(m, assignedSince)(w)).cmapE(f => IO(Scheduled(f, m)), handleTaskFailure(m))

    case -\/(e) =>
      handleTaskFailure(m)(e)
  }

  private[this] def handleTaskFailure(m: MsgDetail)(e: Error): IO[WorkResult[F]] =
    clock >>= handleTaskFailure2(m, e)

  private[this] def handleTaskFailure2(m: MsgDetail, e: Error)(now: DateTime): IO[WorkResult[F]] = {
    val f = failurePolicy(FailureCtx(node, worker, m, e, now))
    val addOps: IO[Unit] = f.additionalOps.traverse_(sopToIo)
    f.reaction.toIO >> addOps >> IO(WorkerFailed(m, e, f.reaction))
  }

  private[this] def logWorkResult(r: WorkResult[F])(time: Long): IO[Unit] =
    IO(r match {
      case CouldntAssign(m) =>
        log.debug.z(s"Couldn't assign: $m")
      case CouldntReAssign(m) =>
        log.warn.z(s"Couldn't reassign: $m")
      case Completed(m) =>
        log.info.z(s"Successfully completed in ${time}ms: $m")
      case Scheduled(_, m) =>
        log.debug.z(s"Scheduled to run asynchronously: $m")
      case WorkerFailed(_, e, f) =>
        // f contains m so no need to print separately
        if (e is Deliberate)
          log.info.z(s"Worker deliberately failed: ${e.msg} // $f")
        else
          log.error(e, s"Worker failed after ${time}ms: $f")
      case TaskmanFailed(e, Some(m)) =>
        log.error(e, s"Taskman error occurred processing $m")
      case TaskmanFailed(e, None) =>
        log.error(e, "Taskman error occurred! (no msg)")
    })

  private[this] def wrapAsync(m: MsgDetail, assignedSince: DateTime): IOE[ProcessorResult[F]] => IO[WorkResult[F]] =
    work =>
      IoUtils.time_(
        catchTaskmanErrors(Some(m))(
          reassignIfNeeded(m, assignedSince) flatMap {
            case Some(r) => IO(r)
            case None    => catchExecErrorsIOE(work) flatMap taskEnd(m, assignedSince)
          }
        )
      )(logWorkResult)

  private[this] val reassignmentOk: IO[Option[WorkResult[F]]] = IO(None)

  private[this] def reassignIfNeeded(m: MsgDetail, assignedSince: DateTime): IO[Option[WorkResult[F]]] =
    clock.flatMap(now =>
      if (now.isBefore(assignedSince plus trustPeriod.value))
        reassignmentOk
      else
        reassign(m)
      )

  private[this] def reassign(m: MsgDetail): IO[Option[WorkResult[F]]] =
    ReAssignWorker(node, worker, m).toIO.map {
      case true  => None
      case false => Some(CouldntReAssign(m))
    }

}