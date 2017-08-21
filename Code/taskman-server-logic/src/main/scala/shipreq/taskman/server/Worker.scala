package shipreq.taskman.server

import java.time.{Duration, Instant}
import scalaz.{-\/, \/, \/-, ~>}
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import shipreq.base.util.FxModule._
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.effect._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{Priority => MsgPriority}
import shipreq.taskman.server.business.{BopReifier, Email, Emails, Support}
import shipreq.taskman.server.business.Bop.SupportOp
import ErrorOr.Implicits.MonadExt
import ServerOp._

object Worker extends HasLogger {

  type MsgProcessor[F[_]] = MsgDetail => MsgProcessorOut[F]

  type MsgProcessorOut[F[_]] = FxE[ProcessorResult[F]]

  type AsyncScheduler[F[_]] = Fx ~> ({type λ[α] = FxE[F[α]]})#λ

  /** Legal responses from a MsgProcessor to a Worker when told to process a msg. */
  sealed trait ProcessorResult[+F[_]]

  object ProcessorResult {

    /** Work is complete. Nothing left to do. */
    case object Complete extends ProcessorResult[Nothing]

    /** Schedule for async processing. */
    case class Schedule[F[_]](s: AsyncScheduler[F], w: FxE[ProcessorResult[F]]) extends ProcessorResult[F]
  }

  // -------------------------------------------------------------------------------------------------------------------

  type FailurePolicy = FailureCtx => FailureResponse

  case class FailureCtx(n: NodeId, w: WorkerId, m: MsgDetail, err: Error, now: Instant)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[ServerOp[Unit]])

  def priorityForWorkerFailure(mp: MsgPriority): Support.Priority =
    mp.value match {
      case p if p >= MsgPriority.High.value   => Support.Priority.Urgent
      case p if p >= MsgPriority.Medium.value => Support.Priority.High
      case _                                  => Support.Priority.Medium
    }

  final class FailureHandler(emails: Emails, bopReifier: BopReifier) {

    def raise(c: Email.Content, p: Support.Priority): FxE[Unit] = {
      val io1 = bopReifier(SupportOp(Support.API.ReportFailure(c.subject, c.body, p)))
      val io2 = emails.archive(c).fold(FxE.nop)(bopReifier.apply)
      io1 execMap io2
    }

    def run(catchIo: Error => Fx[Unit])(f: => FxE[Unit]): Fx[Unit] =
      try
        FxE.safeExec(catchIo)(f)
      catch {
        case t: Throwable => catchIo(Error(t))
      }

    def handleFailedWorker(f: NotifySupportWorkerFailed): Fx[Unit] = {
      val catchIo: Error => Fx[Unit] =
        e2 => Fx(
          log.error(s"""FAILED TO NOTIFY SUPPORT OF FAILED WORKER.
                Notification error: ${e2.stackTraceStr}
                Worker error: ${f.err.stackTraceStr}
                Msg: ${f.md}""")
        ) >> handleFailedTaskman(NotifySupportTaskmanError(f.when, e2, Some(f.md)))
      run(catchIo)(
        raise(emails.workerFailureEmail(f.when, f.md, f.err), priorityForWorkerFailure(f.md.priority))
      )
    }

    def handleFailedTaskman(f: NotifySupportTaskmanError): Fx[Unit] = {
      val catchIo: Error => Fx[Unit] =
        e2 => Fx(
          log.error(s"""FAILED TO NOTIFY SUPPORT OF TASKMAN FAILURE. FUCK.
              Notification error: ${e2.stackTraceStr}
              Original error: ${f.err.stackTraceStr}
              Msg: ${f.md}""")
        )
      run(catchIo)(
        raise(emails.taskmanErrorEmail(f.when, f.err, f.md), Support.Priority.Urgent)
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
             clock: Fx[Instant],
             failurePolicy: FailurePolicy
    ) extends HasLogger {

  def process(m: MsgHeader): Fx[WorkResult[F]] =
    logWorkResult(catchTaskmanErrorsN(assign(m)))

  private[this] def catchExecErrorsFxE[A]: FxE[A] => FxE[A] =
    _.except(FxE error _)

  private[this] def catchTaskmanErrorsG[T](m: => Option[MsgDetail], ef: TaskmanFailed => Fx[T]): Fx[T] => Fx[T] =
    _.except(t => {
      val e = Error(t)
      val notifySupport = clock >>= (t => sopToIo(NotifySupportTaskmanError(t, e, m)))
      notifySupport >> ef(TaskmanFailed(e, m))
    })

  private[this] def catchTaskmanErrors(m: => Option[MsgDetail]) = catchTaskmanErrorsG[WorkResult[F]](m, f => Fx(f))
  private[this] val catchTaskmanErrorsN = catchTaskmanErrors(None)

  private[this] def assign(mh: MsgHeader): Fx[WorkResult[F]] =
    GetMsgAssignWorker(node, worker, mh).toFx >>= {
      case Some(m) => catchTaskmanErrors(Some(m))(logWorkStart(m) >> clock >>= performWork(m))
      case None    => Fx(CouldntAssign(mh))
    }

  private[this] def logWorkStart(md: MsgDetail): Fx[Unit] =
    Fx(log.debug.z(s"Starting work: $md"))

  private[this] def performWork(m: MsgDetail)(assignedSince: Instant): Fx[WorkResult[F]] = {
    val io: MsgProcessorOut[F] =
      try catchExecErrorsFxE(msgProcessor(m)) catch {case t: Throwable => FxE error t}
    io flatMap taskEnd(m, assignedSince)
  }

  private[this] def taskEnd(m: MsgDetail, assignedSince: Instant): ErrorOr[ProcessorResult[F]] => Fx[WorkResult[F]] = {
    case \/-(ProcessorResult.Complete) =>
      UpdateMsgSuccess(node, worker, m).toFx >> Fx(Completed(m))

    case \/-(ProcessorResult.Schedule(schedule, w)) =>
      schedule(wrapAsync(m, assignedSince)(w)).cmapE(f => Fx(Scheduled(f, m)), handleTaskFailure(m))

    case -\/(e) =>
      handleTaskFailure(m)(e)
  }

  private[this] def handleTaskFailure(m: MsgDetail)(e: Error): Fx[WorkResult[F]] =
    clock >>= handleTaskFailure2(m, e)

  private[this] def handleTaskFailure2(m: MsgDetail, e: Error)(now: Instant): Fx[WorkResult[F]] = {
    val f = failurePolicy(FailureCtx(node, worker, m, e, now))
    val addOps: Fx[Unit] = f.additionalOps.traverse_(sopToIo)
    f.reaction.toFx >> addOps >> Fx(WorkerFailed(m, e, f.reaction))
  }

  private[this] def logWorkResult(task: Fx[WorkResult[F]]): Fx[WorkResult[F]] =
    for {
      x <- task.measureDuration
      (r, dur) = x
      _ <- logWorkResult(r, dur)
    } yield r

  private[this] def logWorkResult(r: WorkResult[F], dur: Duration): Fx[Unit] =
    Fx(r match {
      case CouldntAssign(m) =>
        log.debug.z(s"Couldn't assign: $m")
      case CouldntReAssign(m) =>
        log.warn.z(s"Couldn't reassign: $m")
      case Completed(m) =>
        log.info.z(s"Successfully completed in ${dur.toMillis}ms: $m")
      case Scheduled(_, m) =>
        log.debug.z(s"Scheduled to run asynchronously: $m")
      case WorkerFailed(_, e, f) =>
        // f contains m so no need to print separately
        if (e is Deliberate)
          log.info.z(s"Worker deliberately failed: ${e.msg} // $f")
        else
          log.error(e, s"Worker failed after ${dur.toMillis}ms: $f")
      case TaskmanFailed(e, Some(m)) =>
        log.error(e, s"Taskman error occurred processing $m")
      case TaskmanFailed(e, None) =>
        log.error(e, "Taskman error occurred! (no msg)")
    })

  private[this] def wrapAsync(m: MsgDetail, assignedSince: Instant): FxE[ProcessorResult[F]] => Fx[WorkResult[F]] =
    work =>
      logWorkResult(
        catchTaskmanErrors(Some(m))(
          reassignIfNeeded(m, assignedSince) flatMap {
            case Some(r) => Fx(r)
            case None    => catchExecErrorsFxE(work) flatMap taskEnd(m, assignedSince)
          }
        )
      )

  private[this] val reassignmentOk: Fx[Option[WorkResult[F]]] = Fx(None)

  private[this] def reassignIfNeeded(m: MsgDetail, assignedSince: Instant): Fx[Option[WorkResult[F]]] =
    clock.flatMap(now =>
      if (now.isBefore(assignedSince plus trustPeriod.value))
        reassignmentOk
      else
        reassign(m)
      )

  private[this] def reassign(m: MsgDetail): Fx[Option[WorkResult[F]]] =
    ReassignWorker(node, worker, m).toFx.map {
      case true  => None
      case false => Some(CouldntReAssign(m))
    }

}