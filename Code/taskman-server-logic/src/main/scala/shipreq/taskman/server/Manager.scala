package shipreq.taskman.server

import org.joda.time.Period
import scalaz.{Heap, State, StateT}
import scalaz.effect.IO
import shipreq.taskman.api.Priority
import Sop._

object Manager {

  type JobQueue       = Heap[MsgHeader]
  type JobQueueS[A]   = State[JobQueue, A]
  type JobQueueSIO[A] = StateT[IO, JobQueue, A]

  implicit object PrioritisationOrder extends Ordering[MsgHeader] {
    override def compare(x: MsgHeader, y: MsgHeader): Int = {
      val a = y.priority.value - x.priority.value
      if (a != 0) a else {
        val b = x.created.compareTo(y.created)
        if (b != 0) b else
          Ordering.Long.compare(x.id.value, y.id.value)
      }
    }
  }

  implicit val PrioritisationOrderZ = scalaz.Order.fromScalaOrdering[MsgHeader]

  def emptyQueue = Heap.Empty[MsgHeader]

  def addToQueue(ms: Seq[MsgHeader]): JobQueueS[Unit] =
    State.modify(s =>
      (s /: ms)((q, m) => q insert m))

  val getQueueStatus: JobQueueS[Option[(Priority, Int)]] = // TODO cache?
    State.gets(q =>
      if (q.isEmpty)
        None
      else
        Some((q.minimum.priority, q.size))
    )

  val popJob: JobQueueS[Option[MsgHeader]] =
    State(q =>
      if (q.isEmpty)
        (q, None)
      else
        (q.deleteMin, Some(q.minimum))
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
