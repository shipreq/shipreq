package shipreq.webapp.member.project.library

import japgolly.microlibs.utils.ConciseIntSetFormat
import java.time.Instant
import shipreq.webapp.member.project.data.{Project, ProjectMetaData}
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}

/** A library of revisions of a project.
  *
  * Event application is idempotent and commutative.
  */
trait ProjectLibrary extends EventOrd.CmpOps {
  type This <: ProjectLibrary

  def self: This

  val cache: Cache

  val latest: Project

  /** Events that can't be applied yet because there are events missing between the earliest here and
    * the latest project.
    */
  val futureEvents: VerifiedEvent.Seq

  val staleSince: Option[Instant]

  def update(events: VerifiedEvent.Seq, now: Instant): Option[Update]

  def projectAt(ord: EventOrd): Option[Project]

  def withoutFutureEvents: This

  final type Update = ProjectLibrary.UpdateFor[This]

  final def addEvents(events: VerifiedEvent.Seq, now: Instant): This =
    update(events, now).fold(self)(_.newLibrary)

  final def update(newProject: Project, now: Instant): Option[Update] =
    if (newProject > latest)
      update(newProject.history.events, now)
    else
      None

  final def update(u: Project \/ VerifiedEvent.Seq, now: Instant): Option[Update] =
    u.fold(update(_, now), update(_, now))

  final def updated(u: Project \/ VerifiedEvent.Seq, now: Instant): This =
    update(u, now).fold(self)(_.newLibrary)

  @inline final def ord =
    latest.ord

  final override def ordAsInt =
    latest.ordAsInt

  final def descState: String =
    s"ord = $ordAsInt, future = $futureEventRange"

  final def futureEventRange: String =
    "[" + ConciseIntSetFormat(futureEvents.iterator.map(_.ord.value).toSet) + "]"

  assert(futureEvents.isEmpty == staleSince.isEmpty)
}

object ProjectLibrary {

  final case class UpdateFor[+PL <: ProjectLibrary](newLibrary        : PL,
                                                    newlyAppliedEvents: VerifiedEvent.Seq) {

    /** Adding future events is still considered an update to the library. The latest project isn't always updated. */
    def latestUpdated: Boolean =
      newlyAppliedEvents.nonEmpty

    val newEvents: NewEvents =
      NewEvents(newlyAppliedEvents, newLibrary.latest)
  }

  // ===================================================================================================================

  type Update = UpdateFor[ProjectLibrary]

  def empty(cache: Cache): ProjectLibrary =
    init(Project.empty, cache)

  def init(p: Project, cache: Cache): ProjectLibrary =
    new Basic(p, VerifiedEvent.Seq.empty, None, cache)

  def load(ps: Iterable[Project], cache: Cache): Option[ProjectLibrary] =
    Option.when(ps.nonEmpty) {
      val latest = ps.maxBy(_.ordAsInt)
      new Basic(latest, VerifiedEvent.Seq.empty, None, cache.update(ps))
    }

  private final class Basic(val latest        : Project,
                            val futureEvents  : VerifiedEvent.Seq,
                            val staleSince    : Option[Instant],
                            prevCache         : Cache) extends Shared(latest, prevCache) {

    override type This = Basic

    override def self: This =
      this

    override def toString: String =
      s"ProjectLibrary($descState)"

    private def copy(latest        : Project           = latest,
                     futureEvents  : VerifiedEvent.Seq,
                     staleSince    : Option[Instant],
                    ): This =
      new Basic(
        latest         = latest,
        futureEvents   = futureEvents,
        staleSince     = staleSince,
        prevCache      = cache,
      )

    override def update(events: VerifiedEvent.Seq, now: Instant): Option[Update] =
      _update(
        events       = events,
        now          = now,
        staleSince   = staleSince,
        updateLatest = (p2, _, fe2, ss) => copy(p2, fe2, ss),
        updateFuture = (fe2, ss) => copy(futureEvents = fe2, staleSince = ss)
      )

    override def withoutFutureEvents: This =
      copy(futureEvents = VerifiedEvent.Seq.empty, staleSince = None)
  }

  // ===================================================================================================================

  object WithMetaData {
    type Update = UpdateFor[WithMetaData]

    def apply(pl: ProjectLibrary, md: ProjectMetaData): WithMetaData =
      new WithMetaData(
        pl.latest,
        md,
        pl.futureEvents,
        pl.staleSince,
        pl.cache)

    def init(p: Project, md: ProjectMetaData, cache: Cache): WithMetaData =
      new WithMetaData(p, md, VerifiedEvent.Seq.empty, None, cache)
  }

  final class WithMetaData(val latest        : Project,
                           val latestMetaData: ProjectMetaData,
                           val futureEvents  : VerifiedEvent.Seq,
                           val staleSince    : Option[Instant],
                           prevCache         : Cache) extends Shared(latest, prevCache) {

    override type This = WithMetaData

    override def self: This =
      this

    override def toString: String =
      s"ProjectLibrary.WithMetaData($descState)"

    private def copy(latest        : Project           = latest,
                     latestMetaData: ProjectMetaData   = latestMetaData,
                     futureEvents  : VerifiedEvent.Seq,
                     staleSince    : Option[Instant],
                    ): This =
      new WithMetaData(
        latest         = latest,
        latestMetaData = latestMetaData,
        futureEvents   = futureEvents,
        staleSince     = staleSince,
        prevCache      = cache,
      )

    override def update(events: VerifiedEvent.Seq, now: Instant): Option[Update] =
      _update(
        events       = events,
        now          = now,
        staleSince   = staleSince,
        updateLatest = (p2, ves, fe2, ss) => copy(p2, latestMetaData.applyEvents(ves, p2, ves.last.createdAt), fe2, ss),
        updateFuture = (fe2, ss) => copy(futureEvents = fe2, staleSince = ss)
      )

    override def withoutFutureEvents: This =
      new WithMetaData(latest, latestMetaData, VerifiedEvent.Seq.empty, None, cache)

    def withoutMetaData: ProjectLibrary =
      new Basic(latest, futureEvents, staleSince, cache)
  }

  // ===================================================================================================================

  sealed abstract class Shared(latest: Project, prevCache: Cache) extends ProjectLibrary {

    assert(
      futureEvents.isEmpty || futureEvents.head.ord.value > latest.ordAsInt + 1,
      s"Project is v${latest.ordAsInt} but youngest future event is v${futureEvents.head.ord.value}")

    protected def _update(events      : VerifiedEvent.Seq,
                          now         : Instant,
                          staleSince  : Option[Instant],
                          updateLatest: (Project, VerifiedEvent.NonEmptySeq, VerifiedEvent.Seq, Option[Instant]) => This,
                          updateFuture: (VerifiedEvent.Seq, Option[Instant]) => This
                         ): Option[Update] = {

      val newEvents     = ord.fold(events)(o => events.filter(_.ord > o))
      val pendingEvents = futureEvents ++ newEvents

      removeConsecutive(pendingEvents, _.immediatelyFollowsLatest(ord)) match {

        case Some((ves, remainingFutureEvents)) =>
          val p2  = latest.updateOrThrow(ves)
          val ss  = Option.when(remainingFutureEvents.nonEmpty)(now)
          val pl2 = updateLatest(p2, ves, remainingFutureEvents, ss)
          Some(UpdateFor(pl2, ves.values))

        case None =>
          if (newEvents.isEmpty)
            None
          else {
            val ss  = Option.when(pendingEvents.nonEmpty)(staleSince.getOrElse(now))
            val pl2 = updateFuture(pendingEvents, ss)
            Some(UpdateFor(pl2, VerifiedEvent.Seq.empty))
          }
      }
    }

    override final val cache =
      prevCache.update(latest)

    private val latestOrd =
      latest.ordAsInt

    override final def projectAt(ord: EventOrd): Option[Project] =
      if (ord.value == latestOrd)
        Some(latest)
      else if (ord.value > latestOrd)
        None
      else
        cache(ord)
  }

  private def removeConsecutive(events: VerifiedEvent.Seq, headFilter: EventOrd => Boolean): Option[(VerifiedEvent.NonEmptySeq, VerifiedEvent.Seq)] =
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

  implicit val canCmp: EventOrd.CanCmp[ProjectLibrary] =
    EventOrd.CanCmp(_.latest.ordAsInt)
}
