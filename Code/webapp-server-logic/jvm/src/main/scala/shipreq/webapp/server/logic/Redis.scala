package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import scalaz.{-\/, BindRec, Monad, \/-}
import scalaz.syntax.monad._
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}

/** Why is this called Redis and not Cache?
  * Because our architecture relies on Redis for both caching and pub/sub.
  * I don't know of any other service apart from Redis that can satisfactorily, and atomically, fulfil both roles.
  */
object Redis {

  final case class ProjectSnapshot(value: Project, ord: EventOrd.Latest) {
    def toProjectAndOrd = ProjectAndOrd(value, Some(ord))
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

    def isComplete: Boolean =
      events.headOption match {
        case Some(e) => e.ord.immediatelyFollows(snapshot.map(_.ord.asEventOrd))
        case None    => true
      }

    def isCompleteTo(latestOrd: EventOrd.Latest): Boolean =
      ord.exists(_ ==* latestOrd) && isComplete

    def isCompleteTo(latestOrd: Option[EventOrd.Latest]): Boolean =
      latestOrd match {
        case Some(l) => isCompleteTo(l)
        case None    => isEmpty
      }

    def filterComplete: ProjectCache =
      if (isComplete) this else ProjectCache.empty

    def build(pid: ProjectId) =
      snapshot match {
        case Some(ss) => ApplyEvents.append(pid, ss.toProjectAndOrd, events)
        case None     => ApplyEvents.create(pid, events)
      }

    def nonEmptyCompleteBuild(pid: ProjectId): Option[ProjectAndOrd] =
      if (nonEmpty && isComplete)
        build(pid).toOption
      else
        None
  }

  object ProjectCache {
    val empty = apply(None, VerifiedEvent.Seq.empty)
  }

  final case class Subscription[F[_]](unsubscribe: F[Unit])

  trait ProjectAlgebra[F[_]] {
    protected def F: Monad[F]

    /** [TLA+] Used by:
      *          - Load_Subscribe
      *          - Reload_Subscribe
      */
    def subscribe(id      : ProjectId,
                  listener: VerifiedEvent => F[Unit]): F[Subscription[F]]

    /** [TLA+] Used by:
      *          - Load_ReadRedis
      *          - Update_ReadRedis
      */
    def read(id: ProjectId): F[ProjectCache]

    /** Read events only.
      *
      * @param beyond Only events that exceed this are to be returned.
      */
    def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]): F[VerifiedEvent.Seq]

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

    final def writeSnapshot(id         : ProjectId,
                            snapshot   : ProjectAndOrd,
                            publishOnly: VerifiedEvent.Seq): F[Boolean] =
      snapshot.ord match {
        case Some(ord) => writeSnapshot(id, ProjectSnapshot(snapshot.project, ord), publishOnly)
        case None      => fTrue
      }

    /** [TLA+] Used by:
      *          - RedisWriteEvents
      *            - Update_WriteRedis1
      *            - Update_WriteRedis2
      *            - SyncPush
      *
      * @param cacheOnly       Events to save, and not publish.
      * @param cacheAndPublish Events to save, and publish to the project's topic.
      *                        These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the write was accepted (stale data is rejected), or there was nothing to write.
      */
    def writeEvents(id             : ProjectId,
                    cacheOnly      : VerifiedEvent.Seq,
                    cacheAndPublish: VerifiedEvent.Seq): F[Boolean]

    protected final val fTrue = F pure true
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object InMemory {
    private[InMemory] final case class PubSub[F[_]](pub: VerifiedEvent => F[Unit],
                                                    sub: Subscription[F],
                                                    key: AnyRef)

    private[InMemory] final class Queue[F[_]] {
      private val queue = new collection.mutable.Queue[(PubSub[F], VerifiedEvent)]
      def unsafeAdd(events: Traversable[VerifiedEvent], pubSubs: List[PubSub[F]]): Unit =
        synchronized {
          for {
            p <- pubSubs
            e <- events
          } queue.enqueue((p, e))
        }
      def unsafeDequeue() =
        synchronized(Option.when(queue.nonEmpty)(queue.dequeue))
    }
  }

  final class InMemory[F[_]](implicit FF: Monad[F] with BindRec[F]) extends ProjectAlgebra[F] {
    import com.github.blemale.scaffeine._
    import InMemory.PubSub

    override protected def F = FF

    private type PubSubs = List[PubSub[F]]

    private[this] val globalCache  = Scaffeine().softValues().build[ProjectId, ProjectCache]()
    private[this] val globalPubSub = new ConcurrentHashMap[ProjectId, PubSubs]()
    private[this] val globalQueue  = new InMemory.Queue[F]
    private[this] val writeCounter = new AtomicInteger(0)
    private[this] val fUnit        = F.pure(())

    private def modPubSub[A](id: ProjectId)(f: PubSubs => (PubSubs, A)): F[A] =
      F.point(unsafeModPubSubNow(id)(f))

    private def unsafeModPubSubNow[A](id: ProjectId)(f: PubSubs => (PubSubs, A)): A = {
      @volatile var result: Any = null
      globalPubSub.compute(id, (_, ostate) => {
        val state = Option(ostate).getOrElse(Nil)
        val (state2, a) = f(state)
        result = a
        state2
      })
      result.asInstanceOf[A]
    }

    override def subscribe(id: ProjectId, pub: VerifiedEvent => F[Unit]) =
      modPubSub(id) { pubSubs =>
        val key    = new AnyRef
        val unsub  = modPubSub(id)(s => (s.filter(_.key ne key), ()))
        val sub    = Subscription(unsub)
        val pubSub = PubSub(pub, sub, key)
        (pubSub :: pubSubs, sub)
      }

    private def readPubSubsNow(id: ProjectId): PubSubs =
      Option(globalPubSub.get(id)).getOrElse(Nil)

    private def readCacheNow(id: ProjectId) =
      globalCache.getIfPresent(id).getOrElse(ProjectCache.empty)

    override def read(id: ProjectId) = F.point {
      readCacheNow(id)
    }

    override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) = F.point {
      readCacheNow(id).events.filter(_.ord > beyond)
    }

    override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) = F.point {
      writeCounter.getAndIncrement()

      val cache = readCacheNow(id)

      val result =
        if (cache.snapshot.forall(snapshot.ord > _.ord)) {
          val cache2 = ProjectCache(Some(snapshot), cache.events.filter(_.ord > snapshot.ord))
          globalCache.put(id, cache2)
          true
        } else
          false

      val pubSubs = readPubSubsNow(id)
      globalQueue.unsafeAdd(publishOnly, pubSubs)

      result
    }

    override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) = F.point {
      writeCounter.getAndIncrement()

      val cache = readCacheNow(id)

      val newEvents = cache.ord match {
        case Some(o) => VerifiedEvent.Seq.empty ++ (cacheOnly.iterator ++ cacheAndPublish).filter(_.ord > o)
        case None    => cacheOnly ++ cacheAndPublish
      }

      val result =
        if (newEvents.isEmpty || cache.ord.exists(newEvents.min.ord.value > _.value + 1))
          false
        else {
          val cache2 = cache.copy(events = cache.events ++ newEvents)
          globalCache.put(id, cache2)
          true
        }

      val pubSubs = readPubSubsNow(id)
      globalQueue.unsafeAdd(cacheAndPublish, pubSubs)

      result
    }

    // =================================================================================================================
    // Test/utility additions

    def writeCount() = writeCounter.get()

    val publishOne: F[Boolean] =
      F.point(globalQueue.unsafeDequeue()).flatMap {
        case Some((p, e)) => p.pub(e) >| true
        case None         => F.pure(false)
      }

    /** Simulates Redis publishing events to listeners */
    val publishAll: F[Unit] =
      fUnit.whileM_(publishOne)
  }
}
