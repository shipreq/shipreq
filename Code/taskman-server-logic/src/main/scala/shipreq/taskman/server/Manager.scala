package shipreq.taskman.server

import org.joda.time.Period
import scala.collection.immutable.TreeSet
import scalaz.{~>, State, StateT}
import scalaz.effect.IO
import shipreq.taskman.api.Priority
import Sop._

object Manager {

  type JobQueue       = TreeSet[MsgHeader]
  type JobQueueS[A]   = State[JobQueue, A]
  type JobQueueSIO[A] = StateT[IO, JobQueue, A]

  object PrioritisationOrder extends Ordering[MsgHeader] {
    override def compare(x: MsgHeader, y: MsgHeader): Int = {
      val a = y.priority.value - x.priority.value
      if (a != 0) a else {
        val b = x.created.compareTo(y.created)
        if (b != 0) b else
          Ordering.Long.compare(x.id.value, y.id.value)
      }
    }
  }

  def emptyQueue = TreeSet.empty[MsgHeader](PrioritisationOrder)

  def addToQueue(ms: Seq[MsgHeader]): JobQueueS[Unit] =
    State.modify(_ ++ ms)

  val getQueueStatus: JobQueueS[Option[(Priority, Int)]] = // TODO cache?
    State.gets(q =>
      if (q.isEmpty)
        None
      else
        Some((q.head.priority, q.size))
    )

  val popJob: JobQueueS[Option[MsgHeader]] =
    State(q =>
      if (q.isEmpty)
        (q, None)
      else
        (q.tail, q.headOption)
    )

  case class Reified(limit: Int, assignmentTrustPeriod: Period)(implicit node: NodeId, opToIo: SopReifier) {

    val pollTask: JobQueueSIO[Int] =
      for {
        queueStatus <- getQueueStatus.lift[IO]
        jobs        <- GetMsgsAssignNode(node, limit, assignmentTrustPeriod, queueStatus).liftIOM[JobQueueSIO]
        _           <- addToQueue(jobs).lift[IO]
      } yield jobs.length
  }
}
