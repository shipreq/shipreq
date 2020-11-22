package shipreq.webapp.member.project.event

/** Event ordinal.
  *
  * The order of an event in an event stream.
  */
final case class EventOrd(value: Int) extends AnyVal with EventOrd.CmpOps {

  override protected def ordAsInt = value

  def next: EventOrd = EventOrd(value + 1)

  def +(n: Int): EventOrd = EventOrd(value + n)
  def -(n: Int): EventOrd = EventOrd(value - n)

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

  implicit def univEq: UnivEq[EventOrd] =
    UnivEq.derive

  def first = apply(1)

  def fromIndex(idx: Int) = apply(1 + idx)

  // ===================================================================================================================

  final case class Latest(value: Int) {
    def asEventOrd = EventOrd(value)
  }

  @inline implicit def latestExtendsEventOrd(l: Latest): EventOrd =
    l.asEventOrd

  implicit def univEqLatest: UnivEq[Latest] =
    UnivEq.derive

  // ===================================================================================================================

  trait CmpOps extends Any {
    protected def ordAsInt: Int

    final def < [A](a: A)(implicit cmp: CanCmp[A]): Boolean = ordAsInt <  cmp.ordAsInt(a)
    final def > [A](a: A)(implicit cmp: CanCmp[A]): Boolean = ordAsInt >  cmp.ordAsInt(a)
    final def <=[A](a: A)(implicit cmp: CanCmp[A]): Boolean = ordAsInt <= cmp.ordAsInt(a)
    final def >=[A](a: A)(implicit cmp: CanCmp[A]): Boolean = ordAsInt >= cmp.ordAsInt(a)
  }

  final case class CanCmp[-A](ordAsInt: A => Int) extends AnyVal {
    def option: CanCmp[Option[A]] =
      CanCmp(o => if (o.isEmpty) 0 else ordAsInt(o.get))
  }

  object CanCmp {
    implicit val eventOrd             = CanCmp[EventOrd](_.value)
    implicit val eventOrdLatest       = CanCmp[Latest](_.value)
    implicit val optionEventOrd       = eventOrd.option
    implicit val optionEventOrdLatest = eventOrdLatest.option
  }

  // ===================================================================================================================

  object Implicits {
    implicit final class EventOrdLatestExt(private val x: Option[EventOrd.Latest]) extends AnyVal with CmpOps {

      override protected def ordAsInt = if (x.isEmpty) 0 else x.get.value

      def min(y: Option[EventOrd.Latest]): Option[EventOrd.Latest] =
        if (x > y) y else x
    }

  }
}