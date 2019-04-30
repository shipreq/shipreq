package shipreq.webapp.client.project.app.state

import japgolly.microlibs.utils.ConciseIntSetFormat
import japgolly.scalajs.react.{Callback, CallbackTo}
import japgolly.scalajs.react.extra.Px
import java.time.Instant
import scala.annotation.tailrec
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.data.TCB
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

  def addEvents(events: VerifiedEvent.Seq): Option[(ProjectState, VerifiedEvent.NonEmptySeq)] = {
    val newEvents = ord.fold(events)(o => events.filter(_.ord > o))
    val pendingEvents = futureEvents ++ newEvents
    ProjectState.removeConsecutive(pendingEvents, _.immediatelyFollowsLatest(ord))
      .map { case (ves, remainingFutureEvents) =>
        ApplyEvent.trusted.applyVerified(ves)(project) match {
          case \/-(p2) =>
            val pao2 = ProjectAndOrd(p2, Some(ves.lastKey.ord.asLatest))
            val md2  = projectMetaData.applyEvents(ves, Instant.now())
            val s2   = ProjectState(pao2, md2, remainingFutureEvents)
            (s2, ves)
          case -\/(err) =>
            // TODO Do more when VerifiedEvent application fails
            throw new RuntimeException(s"Update failed. $err")
        }
      }
  }
}

object ProjectState {

  def init(p: ProjectAndOrd, md: ProjectMetaData): ProjectState =
    ProjectState(p, md, VerifiedEvent.Seq.empty)

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

  final case class Change(oldState: ProjectState,
                          newState: ProjectState,
                          events  : VerifiedEvent.NonEmptySeq)

  type Listener = Change => Callback

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

    private var _listeners: List[Listener] =
      Nil

    def addListener(l: Listener): Unit =
      _listeners ::= l

    private def updateState(s2: ProjectState, newEvents: VerifiedEvent.NonEmptySeq): Callback =
      Callback {
//        if (s2.futureEvents.nonEmpty)
//          console.warn(s"Not all events applied: stuck at #${s2.latestEventOrd.value} pending ${s2.futureEventRange}")
        val s1 = _state
        _state = s2
        _pxProject.refresh()
        if (_listeners.nonEmpty) {
          val c = Change(oldState = s1, newState = s2, events = newEvents)
          _listeners.foreach(_(c).runNow())
        }
      }

    def applyEventSeqCB(ves: VerifiedEvent.Seq): Callback =
      Callback.unless(ves.isEmpty)(
        stateCB.flatMap { s1 =>
          s1.addEvents(ves) match {
            case Some((s2, ves2)) => updateState(s2, ves2)
            case None             => Callback.empty
          }
        }
      )

    def applyEventSeqSCB(ves: VerifiedEvent.Seq): TCB.Success =
      TCB.Success(applyEventSeqCB(ves))
  }

}