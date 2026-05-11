package shipreq.webapp.server.logic.algebra

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.ops.Trace
import shipreq.webapp.base.data.{ProjectCreator, ProjectId}
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.event.ApplyEventAlgebra

/** Why is this called Redis and not Cache?
  * Because our architecture relies on Redis for both caching and pub/sub.
  * I don't know of any other service apart from Redis that can satisfactorily, and atomically, fulfil both roles.
  */
object Redis extends StrictLogging {

  // This class seems redundant now that Project includes its history but I'm keeping it around for two reasons:
  //
  // 1) It represents a snapshot stored in Redis. In Redis we store the ord explicitly for use by the lua fns.
  //    It's important to model the state as seen from Redis's point of view.
  //
  // 2) It provides type-level proof that we never store empty projects. ord is not an Option here.
  final case class ProjectSnapshot(project: Project, ord: EventOrd.Latest) {
    assert(
      project.ordAsInt == ord.value,
      s"""
         |--------------------------------------------------------------------------------------------------------------
         |Project v${project.ordAsInt} saved in Redis as v${ord.value}
         |
         |Latest 3 events:
         |${project.history.events.to(Vector).reverseIterator.take(3).mkString("\n")}
         |--------------------------------------------------------------------------------------------------------------
         |""".stripMargin)

    override def toString = s"ProjectSnapshot(${ord.value})"
    def min(b: ProjectSnapshot): ProjectSnapshot = if (ord < b.ord) this else b
    def max(b: ProjectSnapshot): ProjectSnapshot = if (ord > b.ord) this else b
  }

  final case class ProjectCache(snapshot: Option[ProjectSnapshot], events: VerifiedEvent.Seq) {

    /** [TLA+] This is RedisTotalVer */
    val ord: Option[EventOrd.Latest] =
      if (events.nonEmpty)
        Some(events.last.ord.asLatest)
      else
        snapshot.map(_.ord)

    @inline def isEmpty = ord.isEmpty
    @inline def nonEmpty = !isEmpty

    def isCompleteTo(latestOrd: EventOrd.Latest): Boolean =
      ord.exists(_ ==* latestOrd)

    def isCompleteTo(latestOrd: Option[EventOrd.Latest]): Boolean =
      latestOrd match {
        case Some(l) => isCompleteTo(l)
        case None    => isEmpty
      }

    def build[F[_]](pid: ProjectId, pc: ProjectCreator)(implicit ae: ApplyEventAlgebra[F]) =
      snapshot match {
        case Some(ss) => ae.append(pid, ss.project, events)
        case None     => ae.create(pid, pc, events)
      }

    def buildNonEmpty[F[_]](pid: ProjectId, pc: ProjectCreator)(implicit ae: ApplyEventAlgebra[F]): F[Option[Project]] =
      if (nonEmpty)
        ae.F.map(build(pid, pc))(_.toOption)
      else
        ae.F.pure(None)
  }

  object ProjectCache {
    val empty = apply(None, VerifiedEvent.Seq.empty)
  }

  final case class Subscription[F[_]](unsubscribe: F[Unit])

  type Listener[F[_]] = ListenerError \/ VerifiedEvent => F[Unit]

  sealed trait ListenerError
  object ListenerError {
    final case class RedisLibraryException(value: Throwable) extends ListenerError
    final case class DecodingFailure(value: SafePickler.DecodingFailure) extends ListenerError
  }

  private[logic] val ensureComplete: ProjectCache => ProjectCache = pc => {
    pc.events.headOption match {
      case Some(e) =>
        if (e.ord.immediatelyFollows(pc.snapshot.map(_.ord.asEventOrd)))
          pc
        else
          pc.snapshot match {
            case s@ Some(_) => ProjectCache(s, VerifiedEvent.Seq.empty)
            case None       => ProjectCache.empty
          }

      case None =>
        pc
    }
  }

  trait ProjectAlgebra[F[_]] {
    protected def F: Monad[F]

    /** [TLA+] Used by:
      *          - Load_Subscribe
      *          - Reload_Subscribe
      */
    def subscribe(id      : ProjectId,
                  listener: Listener[F]): F[Subscription[F]]

    /** [TLA+] Used by:
      *          - Load_ReadRedis
      *          - Update_ReadRedis
      *
      * @return Result is always complete. No gap due to an evicted snapshot.
      */
    final def read(id: ProjectId): F[SafePickler.Result[ProjectCache]] =
      F.map(_read(id))(_.map(ensureComplete))

    /** [TLA+] Used by:
      *          - Load_ReadRedis
      *          - Update_ReadRedis
      */
    protected def _read(id: ProjectId): F[SafePickler.Result[ProjectCache]]

    /** Read events only.
      *
      * @param beyond Only events that exceed this are to be returned.
      */
    def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]): F[SafePickler.Result[VerifiedEvent.Seq]]

    /** [TLA+] Used by:
      *          - RedisWriteSnapshot
      *            - Load_WriteRedis
      *            - Update_WriteRedis1
      *            - Update_WriteRedis2
      *
      * @param publishOnly Events to publish to the project's topic, but not save.
      *                    These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the write was accepted (stale data is rejected), or there was nothing to write.
      */
    def writeSnapshot(id         : ProjectId,
                      snapshot   : ProjectSnapshot,
                      publishOnly: VerifiedEvent.Seq): F[Boolean]

    /** [TLA+] Used by:
      *          - RedisWriteEvents
      *            - Update_WriteRedis1
      *            - Update_WriteRedis2
      *            - SyncPush
      *
      * @param cacheOnly       Events to save, and not publish.
      * @param cacheAndPublish Events to save, and publish to the project's topic.
      *                        These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the write was accepted (stale data is rejected, and empty set is rejected too).
      */
    def writeEvents(id             : ProjectId,
                    cacheOnly      : VerifiedEvent.Seq,
                    cacheAndPublish: VerifiedEvent.Seq): F[Boolean]

    def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq): F[Unit]

    // Helpers

    final def publishEvents(id: ProjectId, events: VerifiedEvent.Seq): F[Unit] =
      VerifiedEvent.NonEmptySeq.maybe(events) match {
        case Some(s) => publishEvents(id, s)
        case None    => F.unit
      }

    /**
     * @return Whether the write was accepted (stale data is rejected), or there was nothing to write.
     */
    final def writeSnapshot(id         : ProjectId,
                            project    : Project,
                            publishOnly: VerifiedEvent.Seq): F[Boolean] =
      project.ord match {
        case Some(ord) => writeSnapshot(id, ProjectSnapshot(project, ord), publishOnly)
        case None      =>
          VerifiedEvent.NonEmptySeq.maybe(publishOnly) match {
            case Some(s) => F.map(publishEvents(id, s))(_ => true)
            case None    => fTrue
          }
      }

    protected final val fTrue = F.pure(true)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def traced[F[_]](underlying: ProjectAlgebra[F], trace: Trace.Algebra[F])(implicit monadF: Monad[F]): ProjectAlgebra[F] =
    new ProjectAlgebra[F] {
      override protected def F = monadF

      private def traced[A](name: String, id: ProjectId, f: F[A]): F[A] =
        trace.newSpan("Redis: " + name) { span =>
          val addAttrs = trace.addAttrs(Trace.Attr.ShipReqProjectId(id) :: Nil)(span)
          F.flatMap(addAttrs)(_ => f)
        }

      override def subscribe(id: ProjectId, listener: Listener[F]) =
        traced("subscribe", id, underlying.subscribe(id, listener))

      override protected def _read(id: ProjectId) =
        traced("read", id, underlying.read(id))

      override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) =
        traced("readEvents", id, underlying.readEvents(id, beyond))

      override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) =
        traced("writeSnapshot", id, underlying.writeSnapshot(id, snapshot, publishOnly))

      override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) =
        traced("writeEvents", id, underlying.writeEvents(id, cacheOnly, cacheAndPublish))

      override def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq) =
        traced("publishEvents", id, underlying.publishEvents(id, events))
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def timed[F[_]](underlying: ProjectAlgebra[F], report: (String, ProjectId, Duration) => F[Unit])
                 (implicit monadF: Monad[F], svr: Server.Time[F]): ProjectAlgebra[F] =
    new ProjectAlgebra[F] {
      override protected def F = monadF

      private def wrap[A](name: String, id: ProjectId, f: F[A]): F[A] =
        for {
          (a, dur) <- svr.measureDuration(f)
          _        <- report(name, id, dur)
        } yield a

      override def subscribe(id: ProjectId, listener: Listener[F]) =
        wrap("subscribe", id, underlying.subscribe(id, listener))

      override protected def _read(id: ProjectId) =
        wrap("read", id, underlying.read(id))

      override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) =
        wrap("readEvents", id, underlying.readEvents(id, beyond))

      override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) =
        wrap("writeSnapshot", id, underlying.writeSnapshot(id, snapshot, publishOnly))

      override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) =
        wrap("writeEvents", id, underlying.writeEvents(id, cacheOnly, cacheAndPublish))

      override def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq) =
        wrap("publishEvents", id, underlying.publishEvents(id, events))
    }

  def withMetricsAndLogging[F[_]](underlying: ProjectAlgebra[F], metrics: MetricsAlgebra.ForRedis[F])
                                 (implicit F: Sync[F], svr: Server.Time[F]): ProjectAlgebra[F] = {

    var report: (String, ProjectId, Duration) => F[Unit] =
      (op, _, dur) => metrics.redis(op, dur)

    logger.whenInfoEnabled {
      report = (op, id, dur) => {
        val log = F.delay(logger.info(s"Redis $op for project #${id.value} completed in ${dur.conciseDesc}."))
        val write = metrics.redis(op, dur)
        write *> log
      }
    }

    timed(underlying, report)
  }
}
