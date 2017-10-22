package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.catchable._
import scalaz.syntax.foldable._
import scalaz.{-\/, \/, \/-, ~>}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{Priority => MsgPriority}
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.business.{BusinessOp, Email, Emails, Support}

object Worker extends HasLogger {

  type MsgProcessor[F[_]] = MsgDetail => MsgProcessorOut[F]

  type MsgProcessorOut[F[_]] = Fx[ProcessorResult[F]]

  type AsyncScheduler[F[_]] = Fx ~> λ[α => Fx[F[α]]]

  /** Legal responses from a MsgProcessor to a Worker when told to process a msg. */
  sealed trait ProcessorResult[+F[_]]

  object ProcessorResult {

    /** Work is complete. Nothing left to do. */
    case object Complete extends ProcessorResult[Nothing]

    /** Schedule for async processing. */
    final case class Schedule[F[_]](scheduler: AsyncScheduler[F], work: Fx[ProcessorResult[F]]) extends ProcessorResult[F]
  }

  // -------------------------------------------------------------------------------------------------------------------

  type FailurePolicy = FailureCtx => FailureResponse

  final case class FailureCtx(node  : NodeId,
                              worker: WorkerId,
                              msg   : MsgDetail,
                              err   : ArticulateError,
                              now   : Instant)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  final case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[ServerOp[Unit]])

  def priorityForWorkerFailure(mp: MsgPriority): Support.Priority =
    mp.value match {
      case p if p >= MsgPriority.High.value   => Support.Priority.Urgent
      case p if p >= MsgPriority.Medium.value => Support.Priority.High
      case _                                  => Support.Priority.Medium
    }

  final class FailureHandler(emails: Emails)(implicit businessOpFx: BusinessOp ~> Fx) {

    def raise(c: Email.Content, p: Support.Priority): Fx[Unit] = {
      val reportFailure = businessOpFx(BusinessOp.SupportOp(Support.API.ReportFailure(c.subject, c.body, p)))
      val emailArchive = emails.archive(c).fold(Fx.unit)(businessOpFx.apply)
      for {
        ea <- reportFailure.attempt
        eb <- emailArchive.attempt
      } yield (ea *> eb).valueOr(throw _)
    }

    private val indent = "  "

    def handleFailedWorker(f: NotifySupportWorkerFailed): Fx[Unit] = {
      def logError(e: ArticulateError): Fx[Unit] =
        Fx(log.error(
          s"""
             |FAILED TO NOTIFY SUPPORT OF FAILED WORKER.
             |
             |Msg:
             |${f.md.toString.indent(indent)}
             |
             |Notification error:
             |${e.show.indent(indent)}
             |
             |Worker error:
             |${f.err.show.indent(indent)}
           """.stripMargin.trim))

      def notifySupport(e: ArticulateError): Fx[Unit] =
        handleFailedTaskman(NotifySupportTaskmanError(f.when, e, Some(f.md)))

      Fx.safe(raise(emails.workerFailureEmail(f.when, f.md, f.err), priorityForWorkerFailure(f.md.priority)))
        .recoverArticulateError(e => logError(e) andFinally notifySupport(e))
    }

    def handleFailedTaskman(f: NotifySupportTaskmanError): Fx[Unit] = {
      def logError(e: ArticulateError): Fx[Unit] =
        Fx(log.error(
          s"""
             |FAILED TO NOTIFY SUPPORT OF TASKMAN FAILURE. FUCK.
             |
             |Msg:
             |${f.md.toString.indent(indent)}
             |
             |Notification error:
             |${e.show.indent(indent)}
             |
             |Original error:
             |${f.err.show.indent(indent)}
           """.stripMargin.trim))

      Fx.safe(raise(emails.taskmanErrorEmail(f.when, f.err, f.md), Support.Priority.Urgent))
        .recoverArticulateError(logError)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult[+F[_]]

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    final case class CouldntAssign(m: MsgHeader) extends WorkResult[Nothing]

    final case class CouldntReassign(m: MsgDetail) extends WorkResult[Nothing]

    /** Work completed successfully. */
    final case class Completed(m: MsgDetail) extends WorkResult[Nothing]

    /** The worker business logic failed. */
    final case class WorkerFailed(m: MsgDetail, err: ArticulateError, f: FailedJobReaction) extends WorkResult[Nothing]

    /** An error occurred in Taskman's generic work management. */
    final case class TaskmanFailed(err: ArticulateError, m: Option[MsgDetail]) extends WorkResult[Nothing]

    final case class Scheduled[F[_]](f: F[WorkResult[F]], m: MsgDetail) extends WorkResult[F]
  }
}

// ---------------------------------------------------------------------------------------------------------------------
import shipreq.taskman.server.logic.Worker.WorkResult._
import shipreq.taskman.server.logic.Worker._

final class Worker[F[_]](msgProcessor : MsgProcessor[F])
                        (implicit node: NodeId,
                         worker       : WorkerId,
                         serverOpFx   : ServerOp ~> Fx,
                         trustPeriod  : AssignmentTrustPeriod,
                         clock        : Fx[Instant],
                         failurePolicy: FailurePolicy) extends HasLogger {

  def process(m: MsgHeader): Fx[WorkResult[F]] =
    logWorkResult(recoverTaskmanErrorsNoMsg(assign(m)))

  private def handleAndRecoverTaskmanErrors[T](m: => Option[MsgDetail], recover: TaskmanFailed => Fx[T]): Fx[T] => Fx[T] =
    _.recoverArticulateError { e =>
      val notifySupport = clock.flatMap(t => serverOpFx(NotifySupportTaskmanError(t, e, m)))
      notifySupport >> recover(TaskmanFailed(e, m))
    }

  private def recoverTaskmanErrors(m: => Option[MsgDetail]): Fx[WorkResult[F]] => Fx[WorkResult[F]] =
    handleAndRecoverTaskmanErrors(m, Fx.pure)

  private val recoverTaskmanErrorsNoMsg: Fx[WorkResult[F]] => Fx[WorkResult[F]] =
    recoverTaskmanErrors(None)

  private def handleTaskFailure(m: MsgDetail, e: ArticulateError): Fx[WorkResult[F]] =
    clock >>= (handleTaskFailure(m, e, _))

  private def handleTaskFailure(m: MsgDetail, e: ArticulateError, now: Instant): Fx[WorkResult[F]] = {
    val f = failurePolicy(FailureCtx(node, worker, m, e, now))
    val addOps: Fx[Unit] = f.additionalOps.traverse_(serverOpFx)
    serverOpFx(f.reaction) >> addOps >> Fx.pure(WorkerFailed(m, e, f.reaction))
  }

  private def assign(mh: MsgHeader): Fx[WorkResult[F]] =
    serverOpFx(GetMsgAssignWorker(node, worker, mh)).flatMap {
      case s@ Some(m) => recoverTaskmanErrors(s)(logWorkStart(m) >> clock >>= performWork(m))
      case None       => Fx(CouldntAssign(mh))
    }

  private def logWorkStart(md: MsgDetail): Fx[Unit] =
    Fx(log.debug.z(s"Starting work: $md"))

  private def performWork(m: MsgDetail)(assignedSince: Instant): Fx[WorkResult[F]] =
    Fx.safe(msgProcessor(m)).attemptArticulateError flatMap taskEnd(m, assignedSince)

  private def taskEnd(m: MsgDetail, assignedSince: Instant): ArticulateError \/ ProcessorResult[F] => Fx[WorkResult[F]] = {
    case \/-(ProcessorResult.Complete) =>
      serverOpFx(UpdateMsgSuccess(node, worker, m)).map(_ => Completed(m))

    case \/-(ProcessorResult.Schedule(scheduler, work)) =>
      scheduler(wrapAsync(m, assignedSince)(work))
        .map(Scheduled(_, m))
        .recoverArticulateError(handleTaskFailure(m, _))

    case -\/(err) =>
      handleTaskFailure(m, err)
  }

  private def logWorkResult(task: Fx[WorkResult[F]]): Fx[WorkResult[F]] =
    for {
      x <- task.measureDuration
      (r, dur) = x
      _ <- logWorkResult(r, dur)
    } yield r

  private def logWorkResult(r: WorkResult[F], dur: Duration): Fx[Unit] =
    Fx(r match {
      case CouldntAssign(m) =>
        log.debug.z(s"Couldn't assign: $m")
      case CouldntReassign(m) =>
        log.warn.z(s"Couldn't reassign: $m")
      case Completed(m) =>
        log.info.z(s"Successfully completed in ${dur.toMillis}ms: $m")
      case Scheduled(_, m) =>
        log.debug.z(s"Scheduled to run asynchronously: $m")
      case WorkerFailed(_, e, f) =>
        // f contains m so no need to print separately
        if (e is Deliberate)
          log.info.z(s"Worker deliberately failed: ${e.getMessage} // $f")
        else
          log.error(e, s"Worker failed after ${dur.toMillis}ms: $f")
      case TaskmanFailed(e, Some(m)) =>
        log.error(e, s"Taskman error occurred processing $m")
      case TaskmanFailed(e, None) =>
        log.error(e, "Taskman error occurred! (no msg)")
    })

  private def wrapAsync(m: MsgDetail, assignedSince: Instant)(work: Fx[ProcessorResult[F]]): Fx[WorkResult[F]] =
    logWorkResult(
      recoverTaskmanErrors(Some(m))(
        reassignIfNeeded(m, assignedSince).flatMap {
          case Some(r) => Fx.pure(r)
          case None    => work.attemptArticulateError flatMap taskEnd(m, assignedSince)
        }))

  private val reassignmentOk: Fx[Option[WorkResult[F]]] =
    Fx(None)

  private def reassignIfNeeded(m: MsgDetail, assignedSince: Instant): Fx[Option[WorkResult[F]]] =
    clock.flatMap(now =>
      if (now.isBefore(assignedSince plus trustPeriod.value))
        reassignmentOk
      else
        reassign(m))

  private def reassign(m: MsgDetail): Fx[Option[WorkResult[F]]] =
    serverOpFx(ReassignWorker(node, worker, m)).map {
      case true  => None
      case false => Some(CouldntReassign(m))
    }
}