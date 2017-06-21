package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import scala.collection.immutable.SortedMap
import scalaz.Scalaz.Id
import shipreq.base.util.IMap
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashRec.Collection
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.base.test.WebappTestUtil._

object MockDb {
  final case class Entry(projectId    : ProjectId,
                         userId       : UserId,
                         events       : VerifiedEvent.Seq,
                         createdAt    : Instant,
                         lastUpdatedAt: Option[Instant]) {

    lazy val project: Project =
      ApplyEvent.trusted.applyVerified(events.eventVector)(Project.empty).needRight

    lazy val projectMetaData: ProjectMetaData =
      ProjectMetaData(id            = ProjectId.Extern(projectId),
                      name          = project.name,
                      eventCount    = events.eventVector.length,
                      reqCount      = project.reqs.size,
                      createdAt     = createdAt,
                      lastUpdatedAt = lastUpdatedAt)

    lazy val projectLoad: DB.ProjectLoad =
      (SortedMap.empty: DB.ProjectLoad) ++ events.iterator
  }

}

final class MockDb extends DB.Algebra[Id] {
  private var projects: IMap[ProjectId, MockDb.Entry] =
    IMap.empty(_.projectId)

  def addProject(projectId: ProjectId, userId: UserId)(events: Event*): Unit = {
    val ves = VerifiedEvent.Seq(EventOrd(1), verifyEvents(Project.empty)(events: _*))
    val now = Instant.now()
    val mde = MockDb.Entry(projectId, userId, ves, now, Some(now))
    projects = projects.add(mde)
  }

  var loadProjectMetaDataAndUserLog = Vector.empty[ProjectId]
  override def loadProjectMetaDataAndUser(id: ProjectId): Option[(ProjectMetaData, UserId)] = {
    loadProjectMetaDataAndUserLog :+= id
    projects.get(id).map(e => (e.projectMetaData, e.userId))
  }

  var loadProjectLog = Vector.empty[ProjectId]
  override def loadProject(id: ProjectId): DB.ProjectLoad = {
    loadProjectLog :+= id
    projects.need(id).projectLoad
  }

  def assertLoadCounts(expectMD: Int, expectEv: Int): Unit =
    assertEq("Load counts", (loadProjectMetaDataAndUserLog.length, loadProjectLog.length), (expectMD, expectEv))

  override def saveProjectEvent(id: ProjectId, ord: EventOrd, e: ActiveEvent, hrs: Collection): Option[Throwable] = {
    val entry = projects.need(id)
    def update(events: VerifiedEvent.Seq): Unit =
      projects = projects + entry.copy(events = events, lastUpdatedAt = Some(Instant.now()))
    val ve = verifyEvent(entry.project, e)
    entry.events match {
      case ves: VerifiedEvent.NonEmptySeq =>
        if (ord.immediatelyFollows(ves.lastOrd)) {
          update(ves.copy(events = ves.events :+ ve))
          None
        } else
          Some(new RuntimeException(s"$ord doesn't follow ${ves.lastOrd}"))
      case VerifiedEvent.EmptySeq =>
        update(VerifiedEvent.NonEmptySeq.one(ord, ve))
        None
    }
  }

  override def inDbTransaction[A](f: A): A =
    f
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockSvr extends Server.Algebra[Id] {
//    private var fns: Map[ServerSideProc.Protocol, String] =
//      UnivEq.emptyMap

  override def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => p.Response): p.Instance =
    ServerSideProc(p.toString, p)

  override def now: Instant =
    Instant.now()

  override def delay[A](f: A, d: Duration): A =
    f
}
