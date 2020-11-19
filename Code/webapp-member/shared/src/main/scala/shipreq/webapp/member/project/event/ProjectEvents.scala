package shipreq.webapp.member.project.event

import scalaz.Equal

final case class ProjectEvents(events: VerifiedEvent.Seq) extends AnyVal {

  def +(e: VerifiedEvent): ProjectEvents =
    ProjectEvents(events + e)

  def ++(es: VerifiedEvent.Seq): ProjectEvents =
    ProjectEvents(events ++ es)

  def ord: Option[EventOrd.Latest] =
    Option.when(events.nonEmpty)(events.last.ord.asLatest)

  def ordAsInt: Int =
    if (events.isEmpty)
      0
    else
      events.last.ord.value

  def nextOrd: EventOrd =
    if (events.isEmpty)
      EventOrd.first
    else
      events.last.ord + 1

  def >(x: ProjectEvents): Boolean =
    (ord, x.ord) match {
      case (Some(a), Some(b)) => a > b
      case (Some(_), None   ) => true
      case (None   , Some(_))
         | (None   , None   ) => false
    }

  @inline def <(x: ProjectEvents): Boolean =
    x > this

  @inline def <=(x: ProjectEvents): Boolean =
    !(this > x)

  @inline def >=(x: ProjectEvents): Boolean =
    !(this < x)

  def max(p: ProjectEvents): ProjectEvents =
    if (this > p) this else p

  def max(o: Option[ProjectEvents]): ProjectEvents =
    o.fold(this)(max)
}

object ProjectEvents {

  val empty: ProjectEvents =
    apply(VerifiedEvent.Seq.empty)

  // Not universally safe/desirable so opt-in only
  object ImplicitEqualityByOrd {
    implicit val equalProjectEvents: Equal[ProjectEvents] =
      (x, y) => x.ordAsInt == y.ordAsInt
  }
}
