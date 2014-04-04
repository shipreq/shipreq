package shipreq.taskman.server

import scalaz.{Heap, State, StateT}
import scalaz.effect.IO

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

  def emptyQueue: JobQueue = Heap.Empty[MsgHeader]

  def addToQueue(ms: Seq[MsgHeader]): JobQueueS[Unit] =
    State.modify(s =>
      (s /: ms)((q, m) => q insert m))

  val getQueueStatus: JobQueueS[Source.QueueStatus] = // TODO cache?
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
}
