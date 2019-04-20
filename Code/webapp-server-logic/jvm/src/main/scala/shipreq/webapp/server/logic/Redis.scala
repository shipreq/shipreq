package shipreq.webapp.server.logic

import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.event.{EventOrd, VerifiedEvent}

/** Why is this called Redis and not Cache?
  * Because our architecture relies on Redis for both caching and pub/sub.
  * I don't know of any other service apart from Redis that can satisfactorily, and atomically, fulfil both roles.
  */
object Redis {

  final case class ProjectSnapshot(value: Project, ord: EventOrd.Latest)

  final case class ProjectCache(snapshot: Option[ProjectSnapshot], events: VerifiedEvent.Seq) {

    /** [TLA+] This is RedisTotalVer */
    val ord: Option[EventOrd] =
      if (events.nonEmpty)
        Some(events.last.ord)
      else
        snapshot.map(_.ord)

    @inline def isEmpty = ord.isEmpty
    @inline def nonEmpty = !isEmpty
  }

  trait ProjectAlgebra[F[_]] {

    /** [TLA+] Used by:
      *          - Load_ReadRedis
      *          - Update_ReadRedis
      */
    def read(id: ProjectId): F[ProjectCache]

    /** [TLA+] Used by:
      *          - RedisWriteSnapshot
      *            - Load_WriteRedis
      *            - Update_WriteRedis1
      *            - Update_WriteRedis2
      *
      * @param publishOnly Events to publish to the project's topic, but not save.
      *                    These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the cache was updated. (Stale data is rejected.)
      */
    def writeSnapshot(id         : ProjectId,
                      snapshot   : ProjectSnapshot,
                      publishOnly: VerifiedEvent.Seq): F[Boolean]

    /** [TLA+] Used by:
      *          - RedisWriteEvents
      *            - Update_WriteRedis1
      *            - Update_WriteRedis2
      *
      * @param cacheOnly       Events to save, and not publish.
      * @param cacheAndPublish Events to save, and publish to the project's topic.
      *                        These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the cache was updated. (Stale data is rejected.)
      */
    def writeEvents(id             : ProjectId,
                    cacheOnly      : VerifiedEvent.Seq,
                    cacheAndPublish: VerifiedEvent.Seq): F[Boolean]

    /** [TLA+] Used by:
      *          - SyncPush
      */
    def publish(id    : ProjectId,
                events: VerifiedEvent.NonEmptySeq): F[Unit]
  }

}
