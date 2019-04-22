package shipreq.webapp.base.event

import japgolly.univeq._

/**
  * Event ordinal.
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

  def asLatest = EventOrd.Latest(value)
}

object EventOrd {
  implicit def univEq: UnivEq[EventOrd] = UnivEq.derive

  def first = apply(1)

  final case class Latest(value: Int) extends AnyVal

  implicit def latestExtendsEventOrd(l: Latest): EventOrd = new EventOrd(l.value)
  implicit def univEqLatest: UnivEq[Latest] = UnivEq.derive
}