package shipreq.webapp.base.event

import japgolly.univeq._

/**
  * Event ordinal.
  *
  * The order of an event in an event stream.
  */
final case class EventOrd(value: Int) extends AnyVal {
  def +(n: Int): EventOrd = EventOrd(value + n)
  def -(n: Int): EventOrd = EventOrd(value - n)

  def < (o: EventOrd): Boolean = value <  o.value
  def > (o: EventOrd): Boolean = value >  o.value
  def <=(o: EventOrd): Boolean = value <= o.value
  def >=(o: EventOrd): Boolean = value >= o.value

  def immediatelyFollows(o: EventOrd): Boolean =
    (o.value + 1) ==* this.value
}

object EventOrd {
  implicit def univEq: UnivEq[EventOrd] =
    UnivEq.derive
}