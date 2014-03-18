package shpireq.taskman

import org.joda.time.Period
import scala.collection.immutable.TreeSet
import shipreq.base.util.{ErrorOr, Error}
import shipreq.taskman.api.{Msg, Priority}

import scalaz.{-\/, \/-, ~>, NonEmptyList, State, StateT}
import scalaz.effect.{MonadIO, IO}
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance

object MsgManagement {

  // Effects: should probably separated from the JobQueue stuff

  sealed trait Op[A]

  /**
   * Assigns msgs to the given node id, and retrieves them.
   *
   * @param limit The maximum number of msgs to assign and return.
   * @param minPriority All msgs will be at least this priority or higher.
   * @param assignmentTrustPeriod Period of time for which another node's assignment is respected.
   */
  case class GetMsgsAssignNode(n: NodeId, limit: Int, minPriority: Option[Priority], assignmentTrustPeriod: Period)
    extends Op[Seq[MsgHeader]]

  case class GetMsgAssignWorker(n: NodeId, w: WorkerId, m: MsgHeader)
    extends Op[Option[MsgDetail]]
  
  case class MarkMsgComplete(m: MsgDetail) extends Op[Unit]
  case class MsgFailedAbort(m: MsgDetail) extends Op[Unit]
  case class MsgFailedRetry(m: MsgDetail, p: Period) extends Op[Unit]

  case class NotifySupportWorkerFailed(m: MsgDetail, e: Error) extends Op[Unit]
  case class NotifySupportTaskmanError(e: Error, m: Option[MsgDetail]) extends Op[Unit]

  // -------------------------------------------------------------------------------------------------------------------

  type JobQueue = TreeSet[MsgHeader]
  type JobQueueS[A] = State[JobQueue, A]
  type JobQueueSIO[A] = StateT[IO, JobQueue, A]

  object PrioritisationOrder extends Ordering[MsgHeader] {
    override def compare(x: MsgHeader, y: MsgHeader): Int = {
      val a = y.p.value - x.p.value
      if (a != 0) a else {
        val b = x.created.compareTo(y.created)
        if (b != 0) b else
          Ordering.Long.compare(x.id.value, y.id.value)
      }

    }
  }

  def empty = TreeSet.empty[MsgHeader](PrioritisationOrder)

  def addToQueue(ms: Seq[MsgHeader]): JobQueueS[Unit] =
    State.modify(q => q ++ ms)

  val getHighestPriority: JobQueueS[Option[Priority]] =
    State.gets(_.headOption.map(_.p))

  val popJob: JobQueueS[Option[MsgHeader]] =
    State(q =>
      if (q.isEmpty)
        (q, None)
      else
        (q.tail, q.headOption)
    )

  implicit class OpExt[A](val op: Op[A]) extends AnyVal {
    def toIo(implicit opToIo: Op ~> IO): IO[A] = opToIo(op)
    def toMIo[M[_]](implicit opToIo: Op ~> IO, m: MonadIO[M]): M[A] = toIo.liftIO[M]
  }

  sealed trait JobFailureReaction
  case class Retry(p: Period) extends JobFailureReaction
  case object Abort extends JobFailureReaction
  type FailurePolicy = MsgDetail => Error => (JobFailureReaction, List[Op[Unit]])

  sealed trait WorkResult {
    val io = IO(this)
  }
  case object CouldntAssign extends WorkResult
  case object Completed extends WorkResult
  case object WorkerFailed extends WorkResult
  case object TaskmanFailed extends WorkResult
}

// ------------------------------------------------------------------------------------------------
import MsgManagement._

case class Blah(n: NodeId,
                limit: Int,
                assignmentTrustPeriod: Period)(implicit opToIo: Op ~> IO) {

  val pollTask: JobQueueSIO[Int] =
    for {
      curHighPri <- getHighestPriority.lift[IO]
      minPri     =  curHighPri.map(_.inc)
      jobs       <- GetMsgsAssignNode(n, limit, minPri, assignmentTrustPeriod).toMIo[JobQueueSIO]
      _          <- addToQueue(jobs).lift[IO]
    } yield jobs.length

  def EXAMPLE_USAGE_poll(): Unit = {
    var queue: JobQueue = empty
    queue = pollTask.run(queue).unsafePerformIO()._1
  }
}

case class Blah2(n: NodeId, w: WorkerId, mh: MsgHeader)(implicit opToIo: Op ~> IO) {

  def EXAMPLE_USAGE_worker(): Unit = {

    val failurePolicy: FailurePolicy = ???
    val jfToIo: JobFailureReaction => IO[Unit] = ???
    val doWork: MsgDetail => ErrorOr[Unit] = ???


    // what about error handling for GetMsgAssignWorker?
    // what about error handling for the post-task actions?

    def processWork(m: MsgDetail): IO[WorkResult] =
      ErrorOr.catchException(doWork(m)) match {
        case \/-(_) =>
          MarkMsgComplete(m).toIo >> Completed.io
        case -\/(err) =>
          val (jf, extra) = failurePolicy(m)(err)
          jfToIo(jf) >> extra.traverse_(opToIo) >> WorkerFailed.io
      }

    def catchTaskmanErrors(m: => Option[MsgDetail]): IO[WorkResult] => IO[WorkResult] =
      _.except(t =>
        opToIo(NotifySupportTaskmanError(Error.error(t), m)) >> WorkerFailed.io
      )

    val task: IO[WorkResult] =
      GetMsgAssignWorker(n, w, mh).toIo.flatMap(_ match {
        case Some(m) => catchTaskmanErrors(Some(m))(processWork(m))
        case None    => CouldntAssign.io
      })

    val task2 = catchTaskmanErrors(None)(task)
    
    task2.unsafePerformIO()
  }

}
