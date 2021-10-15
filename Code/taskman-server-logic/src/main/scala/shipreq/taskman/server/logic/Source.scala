package shipreq.taskman.server.logic

import cats.data.{State, StateT}
import cats.~>
import java.time.{Duration, Instant}
import shipreq.base.util.FxModule._
import shipreq.taskman.api.Priority
import shipreq.taskman.server.logic.Source._

object Source {
  type S = Instant
  type ST[A] = State[S, A]
  type STFx[A] = StateT[Fx, S, A]
  type QueueStatus = Option[(Priority, Int)]
}

final class Source(pollGap      : Duration,
                   batchSize    : Int)
                  (implicit node: NodeId,
                   clock        : Fx[Instant],
                   trustPeriod  : AssignmentTrustPeriod,
                   serverOpFx   : ServerOp ~> Fx) {

  def empty: Fx[S] =
    clock.map(_ minus pollGap)

  val outsidePollGap: STFx[Boolean] =
    StateT(s => clock.map(now => (s, now.isAfter(s plus pollGap))))

  val updateTime: STFx[Unit] =
    StateT(_ => clock.map(n => (n, ())))

  val noResults: STFx[Seq[TaskHeader]] =
    StateT.pure(Seq.empty)

  def runOp[A](op: ServerOp[A]): STFx[A] =
    StateT(s => serverOpFx(op).map((s, _)))

  def poll(qs: QueueStatus): STFx[Seq[TaskHeader]] =
    outsidePollGap.flatMap(ok =>
      if (ok)
        for {
          ms <- runOp(ServerOp.GetTasksAssignNode(node, batchSize, trustPeriod.value, qs))
          _  <- updateTime
        } yield ms
      else
        noResults
    )
}