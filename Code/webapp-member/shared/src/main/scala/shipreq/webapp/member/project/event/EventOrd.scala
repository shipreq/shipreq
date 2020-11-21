package shipreq.webapp.member.project.event

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

  def < (o: Option[EventOrd.Latest]): Boolean = if (o.isEmpty) false else value <  o.get.value
  def <=(o: Option[EventOrd.Latest]): Boolean = if (o.isEmpty) false else value <= o.get.value
  def > (o: Option[EventOrd.Latest]): Boolean = if (o.isEmpty) true  else value >  o.get.value
  def >=(o: Option[EventOrd.Latest]): Boolean = if (o.isEmpty) true  else value >= o.get.value

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

  def fromIndex(idx: Int) = apply(1 + idx)

  final case class Latest(value: Int) {
    def asEventOrd = EventOrd(value)
  }

  @inline implicit def latestExtendsEventOrd(l: Latest): EventOrd = l.asEventOrd

  implicit def univEqLatest: UnivEq[Latest] = UnivEq.derive

  object Implicits {
    implicit final class EventOrdLatestExt(private val x: Option[EventOrd.Latest]) extends AnyVal {

      def >(y: Option[EventOrd.Latest]): Boolean =
        (x, y) match {
          case (Some(a), Some(b)) => a > b
          case (Some(_), None   ) => true
          case (None   , Some(_))
             | (None   , None   ) => false
        }

      @inline def <=(y: Option[EventOrd.Latest]): Boolean =
        !this.>(y)

      def min(y: Option[EventOrd.Latest]): Option[EventOrd.Latest] =
        if (x > y) y else x
    }

  }
}