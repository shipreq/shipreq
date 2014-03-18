package shpireq.taskman

import org.joda.time.Period
import scala.collection.immutable.TreeSet
import scalaz.{StateT, Order, State}
import shipreq.taskman.api.{Msg, Priority}
import scalaz.effect.{MonadIO, LiftIO, IO}

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
  case class GetMsgs(n: NodeId, limit: Int, minPriority: Option[Priority], assignmentTrustPeriod: Period)
    extends Op[Seq[MsgHeader]]

  case class GetMsg(n: NodeId, w: WorkerId, m: MsgHeader)
    extends Op[Option[MsgDetail]]

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

  // -------------------------------------------------------------------------------------------------------------------

  import scalaz._, Scalaz._

  // All defs below could be vals, just avoiding ??? instantiation

  def OpToIo: Op ~> IO = ???
  implicit class OpExt[A](val op: Op[A]) extends AnyVal {
    def toIo: IO[A] = OpToIo(op)
    def toMIo[M[_]](implicit m: MonadIO[M]): M[A] = toIo.liftIO[M]
  }

  def n: NodeId = ???
  def limit: Int = 10
  def assignmentTrustPeriod: Period = ???

  def pollTask: JobQueueSIO[Int] =
    for {
      curHighPri <- getHighestPriority.lift[IO]
      minPri     =  curHighPri.map(_.inc)
      jobs       <- GetMsgs(n, limit, minPri, assignmentTrustPeriod).toMIo[JobQueueSIO]
      _          <- addToQueue(jobs).lift[IO]
    } yield jobs.length

  def EXAMPLE_USAGE_poll(): Unit = {
    var queue: JobQueue = empty
    queue = pollTask.run(queue).unsafePerformIO()._1
  }
}
