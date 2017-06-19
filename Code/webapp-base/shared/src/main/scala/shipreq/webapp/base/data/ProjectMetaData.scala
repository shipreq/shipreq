package shipreq.webapp.base.data

import java.time.Instant
import nyaya.prop.Prop
import scala.annotation.elidable
import shipreq.base.util.univeq._

final case class ProjectMetaData(id           : Project.XId,
                                 name         : Project.Name,
                                 eventCount   : Int,
                                 reqCount     : Int,
                                 createdAt    : Instant,
                                 lastUpdatedAt: Option[Instant]) {

  def lastUpdatedOrCreatedAt: Instant =
    lastUpdatedAt.getOrElse(createdAt)

  @elidable(elidable.ASSERTION)
  def assertInSyncWith(p: => Project): Unit =
    ProjectMetaData.props(p) assert this

  import shipreq.webapp.base.event._

  def applyEvent(ve: VerifiedEvent, when: Instant): ProjectMetaData = {
    var newName     = name
    var newReqCount = reqCount
    ve.event match {
      case e if Event.reqCreationEventFilter(e) => newReqCount += 1
      case ProjectNameSet(n)                    => newName = n
      case _                                    => ()
    }
    copy(
      name          = newName,
      reqCount      = newReqCount,
      eventCount    = eventCount + 1,
      lastUpdatedAt = Some(when))
  }

  def applyEvents(ve: VerifiedEvents, when: Instant): ProjectMetaData =
    ve.foldLeft(this)(_.applyEvent(_, when))
}

object ProjectMetaData {
  implicit def equality: UnivEq[ProjectMetaData] = UnivEq.derive

  def props(project: Project): Prop[ProjectMetaData] = {
    type P = ProjectMetaData
    Prop.equal[P]("Project name")(_.name, _ => project.name) &
      Prop.equal[P]("Req count")(_.reqCount, _ => project.reqs.size)
  }
}