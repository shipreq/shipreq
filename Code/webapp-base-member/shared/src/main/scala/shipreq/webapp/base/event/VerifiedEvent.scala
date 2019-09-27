package shipreq.webapp.base.event

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.ConciseIntSetFormat
import java.time.Instant
import scala.collection.immutable.TreeSet

/**
 * A verified event is an event that has been validated by the server, and proven applicable.
 */
final case class VerifiedEvent(ord      : EventOrd,
                               event    : Event,
                               createdAt: Instant) {

  override def toString = s"VerifiedEvent(${ord.value}, $event, $createdAt)"
}

object VerifiedEvent {

  implicit val ordering: Ordering[VerifiedEvent] =
    (x: VerifiedEvent, y: VerifiedEvent) => x.ord.value - y.ord.value

  /** 0 or more consecutive verified events */
  type Seq = TreeSet[VerifiedEvent]

  object Seq {
    val empty: Seq =
      TreeSet.empty

    def one(e: VerifiedEvent): Seq =
      empty + e
  }

  final case class NonEmptySeq(head: VerifiedEvent, tail: Seq) {
    override def toString =
      s"VerifiedEvent.NonEmptySeq($describeEvents)"

    def describeEvents: String =
      ConciseIntSetFormat(values.toIterator.map(_.ord.value).toSet)

    def values: Seq =
      tail + head
  }

  object NonEmptySeq {

    @inline implicit def nonEmptySeqAsSeq(n: NonEmptySeq): Seq =
      n.values

    def one(ve: VerifiedEvent): NonEmptySeq =
      NonEmptySeq(ve, Seq.empty)

    def maybe(s: Seq): Option[NonEmptySeq] =
      Option.when(s.nonEmpty)(force(s))

    def force(s: Seq): NonEmptySeq =
      apply(s.head, s.tail)
  }
}
