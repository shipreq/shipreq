package shipreq.webapp.server.logic.inmem

import cats.effect.Sync
import cats.syntax.all._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.algebra.Redis._

object InMemoryRedis {
  private[InMemoryRedis] final case class PubSub[F[_]](pub: Listener[F],
                                                       sub: Subscription[F],
                                                       key: AnyRef)

  private[InMemoryRedis] final class Queue[F[_]] {
    private val queue = new collection.mutable.Queue[(PubSub[F], VerifiedEvent)]
    def unsafeAdd(events: VerifiedEvent.NonEmptySeq, pubSubs: List[PubSub[F]]): Unit =
      synchronized {
        for {
          p <- pubSubs
          e <- events
        } queue.enqueue((p, e))
      }
    def unsafeDequeue() =
      synchronized(Option.when(queue.nonEmpty)(queue.dequeue()))
  }
}

final class InMemoryRedis[F[_]](implicit FF: Sync[F]) extends ProjectAlgebra[F] {
  import com.github.blemale.scaffeine._
  import InMemoryRedis.PubSub

  override protected def F = FF

  private type PubSubs = List[PubSub[F]]

  private[this] val globalCache  = Scaffeine().softValues().build[ProjectId, ProjectCache]()
  private[this] val globalPubSub = new ConcurrentHashMap[ProjectId, PubSubs]()
  private[this] val globalQueue  = new InMemoryRedis.Queue[F]
  private[this] val writeCounter = new AtomicInteger(0)

  private def modPubSub[A](id: ProjectId)(f: PubSubs => (PubSubs, A)): F[A] =
    F.delay(unsafeModPubSubNow(id)(f))

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

  override def subscribe(id: ProjectId, pub: Listener[F]) =
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

  override protected def _read(id: ProjectId) = F.delay {
    \/-(readCacheNow(id))
  }

  override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) = F.delay {
    \/-(readCacheNow(id).events.filter(_.ord > beyond))
  }

  override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) =
    F.delay {
      writeCounter.getAndIncrement()
      val cache = readCacheNow(id)
      if (cache.snapshot.forall(snapshot.ord > _.ord)) {
        val cache2 = ProjectCache(Some(snapshot), cache.events.filter(_.ord > snapshot.ord))
        globalCache.put(id, cache2)
        true
      } else
        false
    } <* publishEvents(id, publishOnly)

  override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) =
    F.delay {
      val cache = ensureComplete(readCacheNow(id))
      writeCounter.getAndIncrement()
      val newEvents = cache.ord match {
        case Some(o) => VerifiedEvent.Seq.empty ++ (cacheOnly.iterator ++ cacheAndPublish).filter(_.ord > o)
        case None    => cacheOnly ++ cacheAndPublish
      }
      def containsGaps = cache.ord match {
        case Some(o) => newEvents.min.ord.value != o.value + 1
        case None    => newEvents.min.ord.value > 1
      }
      if (newEvents.isEmpty || containsGaps)
        false
      else {
        val it = newEvents.iterator
        val first = it.next()
        var events2 = cache.events + first
        @tailrec def go(prev: Int): Unit =
          if (it.hasNext) {
            val e = it.next()
            val o = e.ord.value
            if (o == prev + 1) {
              events2 += e
              go(o)
            }
          }
        go(first.ord.value)
        val cache2 = cache.copy(events = events2)
        globalCache.put(id, cache2)
        true
      }
    } <* publishEvents(id, cacheAndPublish)

  override def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq) = F.delay {
    val pubSubs = readPubSubsNow(id)
    globalQueue.unsafeAdd(events, pubSubs)
  }

  // =================================================================================================================
  // Test/utility additions

  def writeCount() = writeCounter.get()

  private def _publishOne[A](ok: A, ko: F[A]): F[A] =
    F.delay(globalQueue.unsafeDequeue()).flatMap {
      case Some((p, e)) => p.pub(\/-(e)) as ok
      case None         => ko
    }

  val publishOne: F[Boolean] =
    _publishOne(true, F.pure(false))

  /** Simulates Redis publishing events to listeners */
  val publishAll: F[Unit] = {
    type T       = Unit \/ Unit
    val stop : T = \/-(())
    val again: T = -\/(())
    val stopF    = F.pure(stop)
    val pub1     = _publishOne(again, stopF)
    def unsafe   = FF.tailRecM[Unit, Unit](())(_ => pub1)
    stopF.flatMap(_ => unsafe)
  }

  def unsafeEvictSnapshot(id: ProjectId): Unit = {
    val c1 = readCacheNow(id)
    val c2 = c1.copy(snapshot = None)
    globalCache.put(id, c2)
  }
}
