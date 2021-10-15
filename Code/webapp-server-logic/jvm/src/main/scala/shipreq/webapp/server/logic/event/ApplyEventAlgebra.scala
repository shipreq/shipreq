package shipreq.webapp.server.logic.event

import cats.syntax.all._
import cats.{Applicative, Monad}
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.ops.Trace
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event._
import shipreq.webapp.server.logic.algebra.{MetricsAlgebra, Server}

trait ApplyEventAlgebra[F[_]] { self =>
  import ApplyEventAlgebra.{AppendFn, Result}

  def F: Applicative[F]
  def trust: Trust
  val appendFn: AppendFn[F]

  final def create(pid: ProjectId, events: VerifiedEvent.Seq): F[Result] =
    append(pid, Project.empty, events)

  final def append(pid: ProjectId,
                   p: Project,
                   events: VerifiedEvent.Seq): F[Result] =
    if (events.isEmpty)
      F.point(\/-(p))
    else
      appendFn(pid, p, VerifiedEvent.NonEmptySeq.force(events))
}

object ApplyEventAlgebra extends StrictLogging {

  type Result = ErrorMsg \/ Project

  type AppendFn[F[_]] = (ProjectId, Project, VerifiedEvent.NonEmptySeq) => F[Result]

  def apply[F[_]](_trust: Trust)
                 (_appendFn: AppendFn[F])
                 (implicit _F: Applicative[F]): ApplyEventAlgebra[F] =
    new ApplyEventAlgebra[F] {
      override def F = _F
      override def trust = _trust
      override val appendFn = _appendFn
    }

  def trusted[F[_]](implicit _F: Applicative[F]): ApplyEventAlgebra[F] =
    apply(Trusted)((pid, p, events) => _F.unit.map { _ =>
      ApplyEvent.trusted(events)(p).leftMap { e =>
        logger.error(s"Failed to apply events [${events.head.ord},${events.last.ord}] on project #${pid.value}: $e")
        ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}: Event application failure.")
      }
    })

  def traced[F[_]](underlying: ApplyEventAlgebra[F], trace: Trace.Algebra[F])
                  (implicit F: Monad[F]): ApplyEventAlgebra[F] =
    apply(underlying.trust)((a, b, c) =>
      trace.newSpan("ApplyEvents")(_ => underlying.appendFn(a, b, c)))

  def withMetricsAndLogging[F[_]](underlying: ApplyEventAlgebra[F], warnIfDurExceedsMs: Int)
                                 (implicit F: Monad[F],
                                  metrics: MetricsAlgebra.ForEvents[F],
                                  svr: Server.Time[F]): ApplyEventAlgebra[F] = {

    val trust              = underlying.trust
    val warnIfDurExceedsNs = warnIfDurExceedsMs * 1000 * 1000

    apply(trust) { (pid, p1, events) =>
      for {
        (res, dur) <- svr.measureDuration(underlying.appendFn(pid, p1, events))
        eventCount = res match {
                       case \/-(p2) => p2.ordAsInt - p1.ordAsInt
                       case -\/(_)  => events.size
                     }
        _          <- metrics.appliedEvents(eventCount, dur, trust = trust)
      } yield {
        if (dur.getSeconds == 0 && dur.getNano < warnIfDurExceedsNs)
          logger.info(s"Applied $eventCount events to project #${pid.value} v${p1.ordAsInt} in ${dur.conciseDesc}")
        else
          logger.warn(s"Applied $eventCount events to project #${pid.value} v${p1.ordAsInt} in ${dur.conciseDesc}")
        res
      }
    }
  }

}