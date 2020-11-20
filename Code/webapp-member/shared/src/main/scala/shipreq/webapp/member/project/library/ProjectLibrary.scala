package shipreq.webapp.member.project.library

import japgolly.microlibs.utils.ConciseIntSetFormat
import shipreq.webapp.member.project.data.{Project, ProjectMetaData}
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}

/** A library of revisions of a project.
  * Includes up-to-date [[ProjectMetaData]], and additional info needed to continue applying events.
  *
  * Event application is idempotent and commutative.
  *
  * @param futureEvents Events that can't be applied yet because there are events missing between the earliest here and
  *                     the latest project.
  */
final case class ProjectLibrary(latest        : Project,
                                latestMetaData: ProjectMetaData,
                                futureEvents  : VerifiedEvent.Seq) {
  import ProjectLibrary._

  assert(
    futureEvents.isEmpty || futureEvents.head.ord.value > latest.history.ordAsInt + 1,
    s"Project is v${latest.history.ordAsInt} but youngest future event is v${futureEvents.head.ord.value}")

  @inline def ord =
    latest.ord

  def descState: String =
    s"ord = $ord, future = $futureEventRange"

  def futureEventRange: String =
    "[" + ConciseIntSetFormat(futureEvents.iterator.map(_.ord.value).toSet) + "]"

  override def toString: String =
    s"ProjectLibrary($descState)"

  def update(events: VerifiedEvent.Seq): Option[ProjectLibrary.Update] = {
    val newEvents = ord.fold(events)(o => events.filter(_.ord > o))
    val pendingEvents = futureEvents ++ newEvents
    removeConsecutive(pendingEvents, _.immediatelyFollowsLatest(ord)) match {

      case Some((ves, remainingFutureEvents)) =>
        val p2  = latest.updateOrThrow(ves)
        val md2 = latestMetaData.applyEvents(ves, p2, ves.last.createdAt)
        val s2  = ProjectLibrary(p2, md2, remainingFutureEvents)
        Some(Update(s2, ves.values))

      case None =>
        if (newEvents.isEmpty)
          None
        else {
          val s2 = copy(futureEvents = pendingEvents)
          Some(Update(s2, VerifiedEvent.Seq.empty))
        }
    }
  }

  def addEvents(events: VerifiedEvent.Seq): ProjectLibrary =
    update(events).fold(this)(_.newLibrary)
}

object ProjectLibrary {

  def init(p: Project, md: ProjectMetaData): ProjectLibrary =
    ProjectLibrary(p, md, VerifiedEvent.Seq.empty)

  final case class Update(newLibrary        : ProjectLibrary,
                          newlyAppliedEvents: VerifiedEvent.Seq) {

    /** Adding future events is still considered an update to the library. The latest project isn't always updated. */
    def latestUpdated: Boolean =
      newlyAppliedEvents.nonEmpty

    val newEvents: NewEvents =
      NewEvents(newlyAppliedEvents, newLibrary.latest)
  }

  private[ProjectLibrary] def removeConsecutive(events: VerifiedEvent.Seq, headFilter: EventOrd => Boolean): Option[(VerifiedEvent.NonEmptySeq, VerifiedEvent.Seq)] =
    events.headOption
      .filter(ve => headFilter(ve.ord))
      .map { ve1 =>
        var ves = VerifiedEvent.Seq.empty
        @tailrec def go(prev: EventOrd, remainder: VerifiedEvent.Seq): VerifiedEvent.Seq =
          remainder.headOption match {
            case Some(ve) if ve.ord.immediatelyFollows(prev) =>
              ves += ve
              go(ve.ord, remainder.tail)
            case _ => remainder
          }
        val remainder = go(ve1.ord, events.tail)
        (VerifiedEvent.NonEmptySeq(ve1, ves), remainder)
      }

}
