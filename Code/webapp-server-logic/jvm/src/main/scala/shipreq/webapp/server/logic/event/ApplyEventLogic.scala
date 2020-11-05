package shipreq.webapp.server.logic.event

import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.syntax.monad._
import scalaz.{Applicative, Monad}
import shipreq.base.ops.Trace
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.member.event._
import shipreq.webapp.server.logic.effect.{MetricsLogic, Server}

trait ApplyEventLogic[F[_]] { self =>
  import ApplyEventLogic.{AppendFn, Result}

  def F: Applicative[F]
  def trust: Trust
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

  def apply[F[_]](_trust: Trust)
                 (_appendFn: AppendFn[F])
                 (implicit _F: Applicative[F]): ApplyEventLogic[F] =
    new ApplyEventLogic[F] {
      override def F = _F
      override def trust = _trust
      override val appendFn = _appendFn
    }

  def trusted[F[_]](implicit _F: Applicative[F]): ApplyEventLogic[F] =
    apply(Trusted)((pid, pao, events) => _F.point {
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
    apply(underlying.trust)((a, b, c) =>
      trace.newSpan("ApplyEvents")(_ => underlying.appendFn(a, b, c)))

  def withMetricsAndLogging[F[_]](underlying: ApplyEventLogic[F], warnIfDurExceedsMs: Int)
                                 (implicit F: Monad[F],
                                  metrics: MetricsLogic.ForEvents[F],
                                  svr: Server.Time[F]): ApplyEventLogic[F] = {

    val trust              = underlying.trust
    val warnIfDurExceedsNs = warnIfDurExceedsMs * 1000 * 1000

    apply(trust) { (pid, pao1, events) =>
      for {
        (res, dur) <- svr.measureDuration(underlying.appendFn(pid, pao1, events))
        eventCount = res match {
                       case \/-(pao2) => pao2.ordAsInt - pao1.ordAsInt
                       case -\/(_)    => events.size
                     }
        _          <- metrics.appliedEvents(eventCount, dur, trust = trust)
      } yield {
        if (dur.getSeconds == 0 && dur.getNano < warnIfDurExceedsNs)
          logger.info(s"Applied $eventCount events to project #${pid.value} v${pao1.ordAsInt} in ${dur.conciseDesc}")
        else
          logger.warn(s"Applied $eventCount events to project #${pid.value} v${pao1.ordAsInt} in ${dur.conciseDesc}")
        res
      }
    }
  }

}