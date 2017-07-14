package shipreq.webapp.client.project.app.state

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.scalajs.react.{Callback, CallbackTo}
import japgolly.scalajs.react.extra.Px
import java.time.Instant
import org.scalajs.dom.console
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scalaz.{-\/, \/-}
import shipreq.base.util.ConciseIntSetFormat
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
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
final case class ProjectState(project        : Project,
                              projectMetaData: ProjectMetaData,
                              latestEventOrd : EventOrd,
                              futureEvents   : ProjectState.EventMap) {

  assert(futureEvents.keysIterator.forall(_ > latestEventOrd))

  def addFutureEvents(es: Iterator[(EventOrd, VerifiedEvent)]): ProjectState =
    copy(futureEvents = futureEvents ++ es.dropWhile(_._1 <= latestEventOrd))

  def futureEventRange: String =
    NonEmptySet.maybe(futureEvents.keysIterator.map(_.value).toSet, "[]")(
      "[" + ConciseIntSetFormat.short(_) + "]")
}

object ProjectState {
  type EventMap = SortedMap[EventOrd, VerifiedEvent]

  def init(p: Project, md: ProjectMetaData, o: EventOrd): ProjectState =
    ProjectState(p, md, o, SortedMap.empty)

  def removeConsecutive(events: EventMap, headFilter: EventOrd => Boolean): Option[(VerifiedEvent.NonEmptySeq, EventMap)] =
    events.headOption
      .filter(x => headFilter(x._1))
      .map { case (o1, e1) =>
        val b = Vector.newBuilder[VerifiedEvent]
        @tailrec def go(prev: EventOrd, remainder: EventMap): EventMap =
          remainder.headOption match {
            case Some((o, e)) if o.immediatelyFollows(prev) =>
              b += e
              go(o, remainder.tail)
            case _ => remainder
          }
        val remainder = go(o1, events.tail)
        val ves = NonEmptyVector(e1, b.result())
        (VerifiedEvent.NonEmptySeq(o1, ves), remainder)
      }

  def applyFutureEvents(s: ProjectState): Option[(VerifiedEvent.NonEmptySeq, ProjectState)] =
    removeConsecutive(s.futureEvents, _.immediatelyFollows(s.latestEventOrd))
      .map { case ((ves, futureEvents2)) =>
        ApplyEvent.trusted.applyVerified(ves.eventVector)(s.project) match {
          case \/-(p2) =>
            val md2 = s.projectMetaData.applyEvents(ves.eventVector, Instant.now())
            val s2 = ProjectState(p2, md2, ves.lastOrd, futureEvents2)
            (ves, s2)
          case -\/(err) =>
            // TODO Do more when VerifiedEvent application fails
            throw new RuntimeException(s"Update failed. $err")
        }
      }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Listener = (VerifiedEvent.Seq, ProjectState, ProjectState) => Callback

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

    private def setState(ves: VerifiedEvent.Seq, s2: ProjectState): Callback =
      Callback {
//        if (s2.futureEvents.nonEmpty)
//          console.warn(s"Not all events applied: stuck at #${s2.latestEventOrd.value} pending ${s2.futureEventRange}")
        val s1 = _state
        _state = s2
        _pxProject.refresh()
        for (l <- _listeners)
          l(ves, s1, s2).runNow()
      }

    def applyEventSeqCB(ves: VerifiedEvent.Seq): Callback =
      ves match {
        case ves2: VerifiedEvent.NonEmptySeq =>
          stateCB.flatMap { s1 =>
            val s2 = s1.addFutureEvents(ves2.iterator)
            applyFutureEvents(s2) match {
              case Some((ves3, s3)) => setState(ves3, s3)
              case None             => setState(VerifiedEvent.EmptySeq, s2)
            }
          }
        case VerifiedEvent.EmptySeq => Callback.empty
      }

    def applyEventSeqSCB(ves: VerifiedEvent.Seq): TCB.Success =
      TCB.Success(applyEventSeqCB(ves))
  }

}