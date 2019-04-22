package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}

object ApplyEvents extends StrictLogging {

  val emptyStartingPoint: (Project, Option[EventOrd.Latest]) =
    (Project.empty, None)

  def create(pid: ProjectId, events: VerifiedEvent.Seq): ErrorMsg \/ (Project, Option[EventOrd.Latest]) =
    append(pid, emptyStartingPoint, events)

  @inline def append(pid: ProjectId,
                     startingPoint: (Project, Option[EventOrd.Latest]),
                     events: VerifiedEvent.Seq): ErrorMsg \/ (Project, Option[EventOrd.Latest]) =
    append(pid, startingPoint._1, startingPoint._2, events)

  def append(pid: ProjectId,
             p: Project,
             latest: Option[EventOrd.Latest],
             events: VerifiedEvent.Seq): ErrorMsg \/ (Project, Option[EventOrd.Latest]) =
    if (events.isEmpty)
      \/-((p, latest))
    else
      ApplyEvent.trusted.applyVerified(events)(p) match {
        case \/-(p2) => \/-((p2, Some(events.lastKey.ord.asLatest)))
        case -\/(e) =>
          logger.error(s"Failed to apply events [${events.head.ord},${events.last.ord}] on project #${pid.value}: $e")
          -\/(ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}\n\nEvent application failure.\n$e"))
      }

}
