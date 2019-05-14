package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event.{ApplyEvent, ProjectAndOrd, VerifiedEvent}

object ApplyEvents extends StrictLogging {

  def create(pid: ProjectId, events: VerifiedEvent.Seq): ErrorMsg \/ ProjectAndOrd =
    append(pid, ProjectAndOrd.empty, events)

  def append(pid: ProjectId,
             pao: ProjectAndOrd,
             events: VerifiedEvent.Seq): ErrorMsg \/ ProjectAndOrd =
    if (events.isEmpty)
      \/-(pao)
    else
      ApplyEvent.trusted.applyVerified(events)(pao.project) match {
        case \/-(p2) =>
          \/-(ProjectAndOrd(p2, Some(events.lastKey.ord.asLatest)))

        case -\/(e) =>
          logger.error(s"Failed to apply events [${events.head.ord},${events.last.ord}] on project #${pid.value}: $e")
          -\/(ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}\n\nEvent application failure.\n$e"))
      }

}
