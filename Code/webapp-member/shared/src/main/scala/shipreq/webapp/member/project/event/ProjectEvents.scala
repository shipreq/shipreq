package shipreq.webapp.member.project.event

import cats.Eq

final case class ProjectEvents(events: VerifiedEvent.Seq) extends AnyVal with EventOrd.CmpOps {

  def +(e: VerifiedEvent): ProjectEvents =
    ProjectEvents(events + e)

  def ++(es: VerifiedEvent.Seq): ProjectEvents =
    ProjectEvents(events ++ es)

  def ord: Option[EventOrd.Latest] =
    Option.when(events.nonEmpty)(events.last.ord.asLatest)

  override def ordAsInt: Int =
    if (events.isEmpty)
      0
    else
      events.last.ord.value

  def nextOrd: EventOrd =
    if (events.isEmpty)
      EventOrd.first
    else
      events.last.ord + 1

  def max(p: ProjectEvents): ProjectEvents =
    if (this > p) this else p

  def max(o: Option[ProjectEvents]): ProjectEvents =
    o.fold(this)(max)
}

object ProjectEvents {

  val empty: ProjectEvents =
    apply(VerifiedEvent.Seq.empty)

  implicit val canCmp: EventOrd.CanCmp[ProjectEvents] =
    EventOrd.CanCmp(_.ordAsInt)

  // Not universally safe/desirable so opt-in only
  object ImplicitEqualityByOrd {
    implicit val equalProjectEvents: Eq[ProjectEvents] =
      (x, y) => x.ordAsInt == y.ordAsInt
  }
}
