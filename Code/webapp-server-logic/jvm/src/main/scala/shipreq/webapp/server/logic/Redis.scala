package shipreq.webapp.server.logic

import japgolly.univeq._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import scalaz.{-\/, BindRec, Monad, \/-}
import scalaz.syntax.monad._
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
    val ord: Option[EventOrd.Latest] =
      if (events.nonEmpty)
        Some(events.last.ord.asLatest)
      else
        snapshot.map(_.ord)

    @inline def isEmpty = ord.isEmpty
    @inline def nonEmpty = !isEmpty

    def isComplete: Boolean =
      (snapshot, events.headOption) match {
        case (Some(_), None)    => true
        case (Some(s), Some(e)) => e.ord.immediatelyFollows(s.ord)
        case (None   , Some(e)) => e.ord ==* EventOrd.first
        case (None   , None)    => true
      }

    def isCompleteTo(latestOrd: EventOrd.Latest): Boolean =
      ord.exists(_ ==* latestOrd) && isComplete

    def isCompleteTo(latestOrd: Option[EventOrd.Latest]): Boolean =
      latestOrd match {
        case Some(l) => isCompleteTo(l)
        case None    => isEmpty
      }

    def build(pid: ProjectId) =
      snapshot match {
        case Some(ss) => ApplyEvents.append(pid, ss.value, Some(ss.ord), events)
        case None     => ApplyEvents.append(pid, Project.empty, None, events)
      }

    def nonEmptyCompleteBuild(pid: ProjectId): Option[(Project, Option[EventOrd.Latest])] =
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
      *            - SyncPush
      *
      * @param cacheOnly       Events to save, and not publish.
      * @param cacheAndPublish Events to save, and publish to the project's topic.
      *                        These events are published unconditionally, even if the cache isn't updated.
      * @return Whether the cache was updated. (Stale data is rejected.)
      */
    def writeEvents(id             : ProjectId,
                    cacheOnly      : VerifiedEvent.Seq,
                    cacheAndPublish: VerifiedEvent.Seq): F[Boolean]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object InMemory {
    private[InMemory] final case class State[F[_]](cache: ProjectCache,
                                                   subs: List[(VerifiedEvent => F[Unit], Subscription[F])],
                                                   pub: List[F[Unit]]) {

      def modSubs(f: List[(VerifiedEvent => F[Unit], Subscription[F])] => List[(VerifiedEvent => F[Unit], Subscription[F])]) =
        copy(subs = f(subs))

      def publish(es: Traversable[VerifiedEvent]) = {
        var pub2 = pub
        for {
          e <- es
          s <- subs
        } pub2 ::= s._1(e)
        pub2
      }
    }
  }

  final class InMemory[F[_]](implicit F: Monad[F] with BindRec[F]) extends ProjectAlgebra[F] {
    import InMemory.State

    private[this] val globalState  = new ConcurrentHashMap[ProjectId, State[F]]()
    private[this] val emptyState   = State[F](ProjectCache.empty, Nil, Nil)
    private[this] val writeCounter = new AtomicInteger(0)
    private[this] val fUnit        = F.pure(())

    private def mod[A](id: ProjectId)(f: State[F] => (State[F], A)): F[A] =
      F.point(unsafeModNow(id)(f))

    private def unsafeModNow[A](id: ProjectId)(f: State[F] => (State[F], A)): A = {
      @volatile var result: Any = null
      globalState.compute(id, (_, ostate) => {
        val state = Option(ostate).getOrElse(emptyState)
        val (state2, a) = f(state)
        result = a
        state2
      })
      result.asInstanceOf[A]
    }

    override def subscribe(id: ProjectId, f: VerifiedEvent => F[Unit]) =
      mod(id) { state =>
        lazy val sub: Subscription[F] = Subscription[F](mod(id)(s => (s.modSubs(_.filter(_._2 ne sub)), ())))
        val state2 = state.modSubs((f, sub) :: _)
        (state2, sub)
      }

    override def read(id: ProjectId) = F.point {
      globalState.getOrDefault(id, emptyState).cache
    }

    override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) =
      mod(id) { state =>
        writeCounter.getAndIncrement()

        val cache = state.cache

        val (result, cache2) =
          if (cache.snapshot.forall(snapshot.ord > _.ord))
            (true, ProjectCache(Some(snapshot), cache.events.filter(_.ord > snapshot.ord)))
          else
            (false, cache)

        val pub2 = state.publish(publishOnly)

        (State(cache2, state.subs, pub2), result)
      }

    override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) =
      mod(id) { state =>
        writeCounter.getAndIncrement()

        val cache = state.cache

        val newEvents = cache.ord match {
          case Some(o) => VerifiedEvent.Seq.empty ++ (cacheOnly.iterator ++ cacheAndPublish).filter(_.ord > o)
          case None    => cacheOnly ++ cacheAndPublish
        }

        val (result, cache2) =
          if (newEvents.isEmpty || cache.ord.exists(newEvents.min.ord.value > _.value + 1))
            (false, cache)
          else
            (true, cache.copy(events = cache.events ++ newEvents))


        val pub2 = state.publish(cacheAndPublish)

        (State(cache2, state.subs, pub2), result)
      }

    // =================================================================================================================
    // Test/utility additions

    def writeCount() = writeCounter.get()

    /** Simulates Redis publishing events to listeners */
    val publishAll: F[Unit] = {
      type L = List[ProjectId]

      def getKeys: F[L] =
        F.point(globalState.keySet().asScala.toList)

      def run(l: L): F[Unit] =
        F.tailrecM[L, Unit]({
          case ids @ id :: nextIds =>
            for {
              callback <- mod(id)(s => s.pub match {
                            case h :: t => (s.copy(pub = t), Some(h))
                            case Nil    => (s, None)
                          })
              _ <- callback.getOrElse(fUnit)
            } yield callback match {
              case Some(_) => -\/(ids)
              case None    => -\/(nextIds)
            }
          case Nil => F.pure(\/-(()))
        })(l)

      for {
        keys <- getKeys
        _    <- run(keys)
      } yield ()
    }
  }
}
