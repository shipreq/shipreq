package shipreq.taskman.server.logic

import scalaz.{Heap, State}

object Manager {

  final case class JobQueue(q: Heap[MsgHeader]) {
    def size: Int =
      q.size

    lazy val status: Source.QueueStatus =
      q.minimumO.map(m => (m.priority, q.size))
  }

  type JobQueueS[A] = State[JobQueue, A]

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

  implicit val PrioritisationOrderZ: scalaz.Order[MsgHeader] =
    scalaz.Order.fromScalaOrdering[MsgHeader]

  def empty: JobQueue =
    JobQueue(Heap.Empty[MsgHeader])

  def add(ms: Seq[MsgHeader]): JobQueueS[Unit] =
    State.modify(j => JobQueue(j.q insertAll ms))

  val pop: JobQueueS[Option[MsgHeader]] =
    State(j =>
      j.q.minimumO match {
        case None      => (j, None)
        case m@Some(_) => (JobQueue(j.q.deleteMin), m)
      }
    )
}
