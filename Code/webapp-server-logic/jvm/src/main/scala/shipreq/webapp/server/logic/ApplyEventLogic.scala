package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import scalaz.{-\/, Applicative, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.ops.Trace
import shipreq.base.util.ErrorMsg
import shipreq.base.util.JavaTimeHelpers._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event.{ApplyEvent, ProjectAndOrd, VerifiedEvent}

trait ApplyEventLogic[F[_]] { self =>
  import ApplyEventLogic.{AppendFn, Result}

  def F: Applicative[F]
  def trusted: Boolean
  val appendFn: AppendFn[F]

  final def create(pid: ProjectId, events: VerifiedEvent.Seq): F[Result] =
    append(pid, ProjectAndOrd.empty, events)

  final def append(pid: ProjectId,
                   pao: ProjectAndOrd,
                   events: VerifiedEvent.Seq): F[Result] =
    if (events.isEmpty)
      F.point(\/-(pao))
    else
      appendFn(pid, pao, VerifiedEvent.NonEmptySeq.force(events))
}

object ApplyEventLogic extends StrictLogging {

  type Result = ErrorMsg \/ ProjectAndOrd

  type AppendFn[F[_]] = (ProjectId, ProjectAndOrd, VerifiedEvent.NonEmptySeq) => F[Result]

  def apply[F[_]](_trusted: Boolean)
                 (_appendFn: AppendFn[F])
                 (implicit _F: Applicative[F]): ApplyEventLogic[F] =
    new ApplyEventLogic[F] {
      override def F = _F
      override def trusted = _trusted
      override val appendFn = _appendFn
    }

  def trusted[F[_]](implicit _F: Applicative[F]): ApplyEventLogic[F] =
    apply(true)((pid, pao, events) => _F.point {
      ApplyEvent.trusted.applyVerified(events)(pao.project) match {
        case \/-(p2) =>
          \/-(ProjectAndOrd(p2, Some(events.lastKey.ord.asLatest)))
        case -\/(e) =>
          logger.error(s"Failed to apply events [${events.head.ord},${events.last.ord}] on project #${pid.value}: $e")
          -\/(ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}: Event application failure."))
      }
    })

  def traced[F[_]](underlying: ApplyEventLogic[F], trace: Trace.Algebra[F])
                  (implicit F: Monad[F]): ApplyEventLogic[F] =
    apply(underlying.trusted)((a, b, c) =>
      trace.newSpan("ApplyEvents")(_ => underlying.appendFn(a, b, c)))

  def withMetricsAndLogging[F[_]](underlying: ApplyEventLogic[F], warnIfDurExceedsMs: Int)
                                 (implicit F: Monad[F],
                                  metrics: MetricsLogic.ForEvents[F],
                                  svr: Server.Time[F]): ApplyEventLogic[F] = {

    val trusted            = underlying.trusted
    val warnIfDurExceedsNs = warnIfDurExceedsMs * 1000 * 1000

    apply(trusted) { (pid, pao1, events) =>
      for {
        (res, dur) ← svr.measureDuration(underlying.appendFn(pid, pao1, events))
        eventCount = res match {
                       case \/-(pao2) => pao2.ordAsInt - pao1.ordAsInt
                       case -\/(_)    => events.size
                     }
        _          ← metrics.appliedEvents(eventCount, dur, trusted = trusted)
      } yield {
        if (dur.getSeconds == 0 && dur.getNano < warnIfDurExceedsNs)
          logger.debug(s"Applied $eventCount events to project #${pid.value} v${pao1.ordAsInt} in ${dur.conciseDesc}")
        else
          logger.warn(s"Applied $eventCount events to project #${pid.value} v${pao1.ordAsInt} in ${dur.conciseDesc}")
        res
      }
    }
  }

}