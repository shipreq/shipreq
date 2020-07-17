package shipreq.webapp.client.project.app.state

import japgolly.microlibs.utils.ConciseIntSetFormat
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{Callback, CallbackTo}
import shipreq.webapp.base.data.{Project, ProjectMetaData, TCB}
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.client.project.lib.DataReusability.reusabilityProject

/**
  * A [[Project]] built from an event stream, up-to-date [[ProjectMetaData]],
  * and additional info needed to continue applying events.
  *
  * Event application is idempotent and commutative.
  *
  * @param futureEvents Events that been received but not applied yet.
  */
final case class ProjectState(projectAndOrd  : ProjectAndOrd,
                              projectMetaData: ProjectMetaData,
                              futureEvents   : VerifiedEvent.Seq) {

  def project = projectAndOrd.project
  def ord = projectAndOrd.ord

  override def toString = s"ProjectState($descState)"

  def descState = s"ord = $ord, future = $futureEventRange"

  def futureEventRange = "[" + ConciseIntSetFormat(futureEvents.iterator.map(_.ord.value).toSet) + "]"

  assert(
    ord match {
      case Some(o) => futureEvents.forall(_.ord > o)
      case None    => true
    }, s"Error: old events found in futureEvents. $descState")

  assert(
    !futureEvents.iterator.map(_.ord).contains(projectAndOrd.nextOrd),
    s"Error: applicable event found in futureEvents. $descState")

  def addEvents(events: VerifiedEvent.Seq): Option[ProjectState.Update] = {
    val newEvents = ord.fold(events)(o => events.filter(_.ord > o))
    val pendingEvents = futureEvents ++ newEvents
    ProjectState.removeConsecutive(pendingEvents, _.immediatelyFollowsLatest(ord)) match {

      case Some((ves, remainingFutureEvents)) =>
        val pao2 = projectAndOrd.mustApplyVerified(ves)
        val md2 = projectMetaData.applyEvents(ves, pao2.project, ves.last.createdAt)
        val s2  = ProjectState(pao2, md2, remainingFutureEvents)
        Some(ProjectState.Update(s2, ves.values))

      case None =>
        if (newEvents.isEmpty)
          None
        else {
          val s2 = copy(futureEvents = pendingEvents)
          Some(ProjectState.Update(s2, VerifiedEvent.Seq.empty))
        }
    }
  }

  def addEventsSimple(events: VerifiedEvent.Seq): ProjectState =
    addEvents(events).fold(this)(_.newState)
}

object ProjectState {

  def init(p: ProjectAndOrd, md: ProjectMetaData): ProjectState =
    ProjectState(p, md, VerifiedEvent.Seq.empty)

  final case class Update(newState: ProjectState, newlyAppliedEvents: VerifiedEvent.Seq) {
    val newEvents = NewEvents(newlyAppliedEvents, newState.project)
  }

  private[ProjectState] def removeConsecutive(events: VerifiedEvent.Seq, headFilter: EventOrd => Boolean): Option[(VerifiedEvent.NonEmptySeq, VerifiedEvent.Seq)] =
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Mutable(initState: ProjectState) {

    private var _state: ProjectState =
      initState

    def state(): ProjectState =
      _state

    val stateCB: CallbackTo[ProjectState] =
      CallbackTo(_state)

    private val _pxProject: Px.ThunkM[Project] =
      Px.apply(_state.project).withReuse.manualRefresh

    val pxProject: Px[Project] =
      _pxProject

    private def updateState(u: Update): Callback =
      Callback {
//        if (s2.futureEvents.nonEmpty)
//          console.warn(s"Not all events applied: stuck at #${s2.latestEventOrd.value} pending ${s2.futureEventRange}")
        _state = u.newState
        if (u.newlyAppliedEvents.nonEmpty)
          _pxProject.refresh()
      }

    def applyEventSeqCB(ves: VerifiedEvent.Seq): Callback =
      Callback.unless(ves.isEmpty)(
        stateCB.flatMap { s1 =>
          Callback.traverseOption(s1.addEvents(ves))(updateState)
        }
      )

    def applyEventSeqSCB(ves: VerifiedEvent.Seq): TCB.Success =
      TCB.Success(applyEventSeqCB(ves))
  }

}