package shipreq.webapp.base.event

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.hash.HashRecs

/**
 * A verified event is an event that has been validated by the server, proven applicable, and retains hashes expected
 * of the Project after application.
 */
final case class VerifiedEvent(event: Event, hashRecs: HashRecs)

object VerifiedEvent {

  /** 0 or more consecutive verified events */
  sealed trait Seq {
    def eventVector: Vector[VerifiedEvent]
    def iterator: Iterator[(EventOrd, VerifiedEvent)]
  }

  object Seq {
    // For type inference
    @inline def empty: Seq = EmptySeq

    // For tests
    def apply(firstOrd: EventOrd, events: TraversableOnce[VerifiedEvent]): Seq =
      NonEmptyVector.maybe(events.toVector, empty)(NonEmptySeq(firstOrd, _))
  }

  case object EmptySeq extends Seq {
    override def eventVector = Vector.empty[VerifiedEvent]
    override def iterator = Iterator.empty
  }

  /** 1 or more consecutive verified events */
  final case class NonEmptySeq(firstOrd: EventOrd, events: NonEmptyVector[VerifiedEvent]) extends Seq {
    override def toString = s"NonEmptySeq[${firstOrd.value},${lastOrd.value}]"

    override def eventVector = events.whole

    override def iterator: Iterator[(EventOrd, VerifiedEvent)] = {
      val v = events.whole
      v.indices.iterator.map(i => (firstOrd + i, v(i)))
    }

    def lastOrd: EventOrd =
      firstOrd + events.tail.length
  }

  object NonEmptySeq {
    def one(ord: EventOrd, event: VerifiedEvent): NonEmptySeq =
      NonEmptySeq(ord, NonEmptyVector one event)
  }
}
