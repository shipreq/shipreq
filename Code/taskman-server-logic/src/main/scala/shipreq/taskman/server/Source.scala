package shipreq.taskman.server

import org.joda.time.{Period, DateTime}
import shipreq.taskman.api.Priority
import shipreq.taskman.server.Sop.GetMsgsAssignNode
import scalaz.{StateT, State}
import scalaz.effect.IO

object Source {

  type S = DateTime
  type ST[A] = State[S, A]
  type STIO[A] = StateT[IO, S, A]
  type QueueStatus = Option[(Priority, Int)]

  case class Reified(pollGap: Period, batchSize: Int, assignmentTrustPeriod: Period)(
    implicit node: NodeId,
             clock: IO[DateTime],
             sopReifier: SopReifier) {

    def empty: IO[S] = clock.map(_ minus pollGap)

    val outsidePollGap: STIO[Boolean] =
      StateT(s => clock.map(now => (s, now.isAfter(s plus pollGap))))

    val updateTime: STIO[Unit] =
      StateT(_ => clock.map(n => (n, ())))

    val noResults: STIO[Seq[MsgHeader]] =
      StateT.stateT(Seq.empty)

    def poll(qs: QueueStatus): STIO[Seq[MsgHeader]] =
      outsidePollGap flatMap (ok =>
        if (ok)
          for {
            ms <- GetMsgsAssignNode(node, batchSize, assignmentTrustPeriod, qs).liftIOM[STIO]
            _  <- updateTime
          } yield ms
        else
          noResults
      )
  }
}
