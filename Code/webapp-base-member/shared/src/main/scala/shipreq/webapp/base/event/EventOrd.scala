package shipreq.webapp.base.event

import japgolly.univeq._

/** Event ordinal.
  *
  * The order of an event in an event stream.
  */
final case class EventOrd(value: Int) extends AnyVal {

  def next: EventOrd = EventOrd(value + 1)

  def +(n: Int): EventOrd = EventOrd(value + n)
  def -(n: Int): EventOrd = EventOrd(value - n)

  def < (o: EventOrd): Boolean = value <  o.value
  def > (o: EventOrd): Boolean = value >  o.value
  def <=(o: EventOrd): Boolean = value <= o.value
  def >=(o: EventOrd): Boolean = value >= o.value

  def immediatelyFollows(prev: EventOrd): Boolean =
    (prev.value + 1) ==* this.value

  def immediatelyFollows(prev: Option[EventOrd]): Boolean =
    prev match {
      case Some(p) => immediatelyFollows(p)
      case None    => this ==* EventOrd.first
    }

  def immediatelyFollowsLatest(prev: Option[EventOrd.Latest]): Boolean =
    immediatelyFollows(prev.map(_.asEventOrd))

  def asLatest = EventOrd.Latest(value)
}

object EventOrd {
  implicit def univEq: UnivEq[EventOrd] = UnivEq.derive

  def first = apply(1)

  final case class Latest(value: Int) {
    def asEventOrd = EventOrd(value)
  }

  @inline implicit def latestExtendsEventOrd(l: Latest): EventOrd = l.asEventOrd

  implicit def univEqLatest: UnivEq[Latest] = UnivEq.derive
}