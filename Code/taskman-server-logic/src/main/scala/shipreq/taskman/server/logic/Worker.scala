package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scalaz.std.either._
import scalaz.std.list.listInstance
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.~>
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{Priority => TaskPriority}
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.business.{BusinessOp, Email, Emails, Support}

object Worker extends HasLogger {

  type Processor[F[_]] = TaskDetail => ProcessorOut[F]

  type ProcessorOut[F[_]] = Fx[ProcessorResult[F]]

  type AsyncScheduler[F[_]] = Fx ~> λ[α => Fx[F[α]]]

  /** Legal responses from a [[Processor]] to a Worker when told to process a task. */
  sealed trait ProcessorResult[+F[_]]

  object ProcessorResult {

    /** Work is complete. Nothing left to do. */
    case object Complete extends ProcessorResult[Nothing]

    /** Schedule for async processing. */
    final case class Schedule[F[_]](scheduler: AsyncScheduler[F], work: Fx[ProcessorResult[F]]) extends ProcessorResult[F]
  }

  // -------------------------------------------------------------------------------------------------------------------

  type FailurePolicy = FailureCtx => FailureResponse

  final case class FailureCtx(node      : NodeId,
                              worker    : WorkerId,
                              taskDetail: TaskDetail,
                              err       : ArticulateError,
                              now       : Instant)

  /**
   * What to do when a job fails.
   *
   * @param reaction What to do with the job itself.
   * @param additionalOps Optional additional operations to perform (esp. notifying support).
   */
  final case class FailureResponse(reaction: FailedJobReaction, additionalOps: List[ServerOp[Unit]])

  def priorityForWorkerFailure(mp: TaskPriority): Support.Priority =
    mp.value match {
      case p if p >= TaskPriority.High.value   => Support.Priority.Urgent
      case p if p >= TaskPriority.Medium.value => Support.Priority.High
      case _                                   => Support.Priority.Medium
    }

  final class FailureHandler(emails: Emails)(implicit businessOpFx: BusinessOp ~> Fx) {

    def raise(c: Email.Content, p: Support.Priority): Fx[Unit] = {
      val reportFailure = businessOpFx(BusinessOp.SupportOp(Support.API.ReportFailure(c, p)))
      val emailArchive = emails.archive(c).fold(Fx.unit)(businessOpFx.apply)
      for {
        ea <- reportFailure.attempt
        eb <- emailArchive.attempt
      } yield (ea *> eb).swap.foreach(throw _)
    }

    private val indent = "  "

    def handleFailedWorker(f: NotifySupportWorkerFailed): Fx[Unit] = {
      def logError(e: ArticulateError): Fx[Unit] =
        Fx(logger.error(
          s"""
             |FAILED TO NOTIFY SUPPORT OF FAILED WORKER.
             |
             |Task:
             |${f.td.toString.indent(indent)}
             |
             |Notification error:
             |${e.show.indent(indent)}
             |
             |Worker error:
             |${f.err.show.indent(indent)}
           """.stripMargin.trim))

      def notifySupport(e: ArticulateError): Fx[Unit] =
        handleFailedTaskman(NotifySupportTaskmanError(f.when, e, Some(f.td)))

      Fx.safe(raise(emails.workerFailureEmail(f.when, f.td, f.err), priorityForWorkerFailure(f.td.priority)))
        .recoverArticulateError(e => logError(e) andFinally notifySupport(e))
    }

    def handleFailedTaskman(f: NotifySupportTaskmanError): Fx[Unit] = {
      def logError(e: ArticulateError): Fx[Unit] =
        Fx(logger.error(
          s"""
             |FAILED TO NOTIFY SUPPORT OF TASKMAN FAILURE. FUCK.
             |
             |Task:
             |${f.td.toString.indent(indent)}
             |
             |Notification error:
             |${e.show.indent(indent)}
             |
             |Original error:
             |${f.err.show.indent(indent)}
           """.stripMargin.trim))

      Fx.safe(raise(emails.taskmanErrorEmail(f.when, f.err, f.td), Support.Priority.Urgent))
        .recoverArticulateError(logError)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  /** Represents the final outcome of attempting to perform a job. */
  sealed trait WorkResult[+F[_]]

  object WorkResult {

    /** Unable to assign the worker to the job. Someone else must've taken it. */
    final case class CouldntAssign(taskHeader: TaskHeader) extends WorkResult[Nothing]

    final case class CouldntReassign(taskDetail: TaskDetail) extends WorkResult[Nothing]

    /** Work completed successfully. */
    final case class Completed(taskDetail: TaskDetail) extends WorkResult[Nothing]

    /** The worker business logic failed. */
    final case class WorkerFailed(taskDetail: TaskDetail, err: ArticulateError, f: FailedJobReaction) extends WorkResult[Nothing]

    /** An error occurred in Taskman's generic work management. */
    final case class TaskmanFailed(err: ArticulateError, taskDetail: Option[TaskDetail]) extends WorkResult[Nothing]

    final case class Scheduled[F[_]](fResult: F[WorkResult[F]], taskDetail: TaskDetail) extends WorkResult[F]
  }
}

// ---------------------------------------------------------------------------------------------------------------------
import shipreq.taskman.server.logic.Worker.WorkResult._
import shipreq.taskman.server.logic.Worker._

final class Worker[F[_]](processor    : Processor[F])
                        (implicit node: NodeId,
                         worker       : WorkerId,
                         serverOpFx   : ServerOp ~> Fx,
                         trustPeriod  : AssignmentTrustPeriod,
                         clock        : Fx[Instant],
                         failurePolicy: FailurePolicy) extends HasLogger {

  def process(m: TaskHeader): Fx[WorkResult[F]] =
    logWorkResult(recoverTaskmanErrorsNoTask(assign(m)))

  private def handleAndRecoverTaskmanErrors[T](m: => Option[TaskDetail], recover: TaskmanFailed => Fx[T]): Fx[T] => Fx[T] =
    _.recoverArticulateError { e =>
      val notifySupport = clock.flatMap(t => serverOpFx(NotifySupportTaskmanError(t, e, m)))
      notifySupport >> recover(TaskmanFailed(e, m))
    }

  private def recoverTaskmanErrors(t: => Option[TaskDetail]): Fx[WorkResult[F]] => Fx[WorkResult[F]] =
    handleAndRecoverTaskmanErrors(t, Fx.pure)

  private val recoverTaskmanErrorsNoTask: Fx[WorkResult[F]] => Fx[WorkResult[F]] =
    recoverTaskmanErrors(None)

  private def handleTaskFailure(t: TaskDetail, e: ArticulateError): Fx[WorkResult[F]] =
    clock >>= (handleTaskFailure(t, e, _))

  private def handleTaskFailure(t: TaskDetail, e: ArticulateError, now: Instant): Fx[WorkResult[F]] = {
    val f                = failurePolicy(FailureCtx(node, worker, t, e, now))
    val addOps: Fx[Unit] = f.additionalOps.traverse_(serverOpFx)
    val log              = Fx(logger.warn(s"Task failure on $t", e))
    log >> serverOpFx(f.reaction) >> addOps >> Fx.pure(WorkerFailed(t, e, f.reaction))
  }

  private def assign(th: TaskHeader): Fx[WorkResult[F]] =
    serverOpFx(GetTaskAssignWorker(node, worker, th)).flatMap {
      case s@ Some(m) => recoverTaskmanErrors(s)(logWorkStart(m) >> clock >>= performWork(m))
      case None       => Fx(CouldntAssign(th))
    }

  private def logWorkStart(td: TaskDetail): Fx[Unit] =
    Fx(logger.info(s"Starting work: $td"))

  private def performWork(td: TaskDetail)(assignedSince: Instant): Fx[WorkResult[F]] =
    Fx.safe(processor(td)).attemptArticulateError flatMap taskEnd(td, assignedSince)

  private def taskEnd(td: TaskDetail, assignedSince: Instant): ArticulateError \/ ProcessorResult[F] => Fx[WorkResult[F]] = {
    case \/-(ProcessorResult.Complete) =>
      for {
        _ <- Fx(logger.info(s"Successfully completed work #${td.id.value}"))
        _ <- serverOpFx(UpdateTaskSuccess(node, worker, td))
      } yield Completed(td)

    case \/-(ProcessorResult.Schedule(scheduler, work)) =>
      scheduler(wrapAsync(td, assignedSince)(work))
        .map(Scheduled(_, td))
        .recoverArticulateError(handleTaskFailure(td, _))

    case -\/(err) =>
      handleTaskFailure(td, err)
  }

  private def logWorkResult(task: Fx[WorkResult[F]]): Fx[WorkResult[F]] =
    for {
      (r, dur) <- task.measureDuration
      _        <- logWorkResult(r, dur)
    } yield r

  private def logWorkResult(r: WorkResult[F], dur: Duration): Fx[Unit] =
    Fx(r match {
      case CouldntAssign(t) =>
        logger.info(s"Couldn't assign: $t")

      case CouldntReassign(t) =>
        logger.warn(s"Couldn't reassign: $t")

      case Completed(t) =>
        logger.info(s"Successfully completed in ${dur.toMillis}ms: $t")

      case Scheduled(_, t) =>
        logger.info(s"Scheduled to run asynchronously: $t")

      case WorkerFailed(_, e, f) =>
        // f contains m so no need to print separately
        if (e is Deliberate)
          logger.warn(s"Worker deliberately failed: ${e.getMessage} // $f")
        else
          logger.error(s"Worker failed after ${dur.toMillis}ms: $f", e)

      case TaskmanFailed(e, Some(t)) =>
        logger.error(s"Taskman error occurred processing $t", e)

      case TaskmanFailed(e, None) =>
        logger.error("Taskman error occurred! (no task)", e)
    })

  private def wrapAsync(m: TaskDetail, assignedSince: Instant)(work: Fx[ProcessorResult[F]]): Fx[WorkResult[F]] =
    logWorkResult(
      recoverTaskmanErrors(Some(m))(
        reassignIfNeeded(m, assignedSince).flatMap {
          case Some(r) => Fx.pure(r)
          case None    => work.attemptArticulateError flatMap taskEnd(m, assignedSince)
        }))

  private val reassignmentOk: Fx[Option[WorkResult[F]]] =
    Fx(None)

  private def reassignIfNeeded(t: TaskDetail, assignedSince: Instant): Fx[Option[WorkResult[F]]] =
    clock.flatMap(now =>
      if (now.isBefore(assignedSince plus trustPeriod.value))
        reassignmentOk
      else
        reassign(t))

  private def reassign(t: TaskDetail): Fx[Option[WorkResult[F]]] =
    serverOpFx(ReassignWorker(node, worker, t)).map {
      case true  => None
      case false => Some(CouldntReassign(t))
    }
}