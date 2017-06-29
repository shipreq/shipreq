package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import scala.collection.immutable.SortedMap
import scalaz.Name
import shipreq.base.util.IMap
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.user._
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

    lazy val projectLoad: DB.ProjectEvents =
      (SortedMap.empty: DB.ProjectEvents) ++ events.iterator
  }

}

final class MockDb extends DB.Algebra[Name] {
  private var projects: IMap[ProjectId, MockDb.Entry] =
    IMap.empty(_.projectId)

  def addProject(projectId: ProjectId, userId: UserId)(events: Event*): Unit = {
    val ves = VerifiedEvent.Seq(EventOrd(1), verifyEvents(Project.empty)(events: _*))
    val now = Instant.now()
    val mde = MockDb.Entry(projectId, userId, ves, now, Some(now))
    projects = projects.add(mde)
  }

  override def createEmptyProject(id: UserId) = Name[ProjectId] {
    val pid = ProjectId(1 + projects.underlyingMap.keysIterator.map(_.value).foldLeft(0L)(_ max _))
    addProject(pid, id)()
    pid
  }

  override def getAllProjectMetaDataForUser(id: UserId) = Name[List[ProjectMetaData]] {
    projects.valuesIterator
      .filter(_.userId ==* id)
      .map(_.projectMetaData)
      .toList
  }

  var loadProjectHeaderLog = Vector.empty[ProjectId]
  override def getProjectHeader(id: ProjectId) = Name[Option[ProjectHeader]] {
    loadProjectHeaderLog :+= id
    projects.get(id).map(e => ProjectHeader(e.userId, e.project.name))
  }

  var loadProjectMetaDataLog = Vector.empty[ProjectId]
  override def getProjectMetaData(id: ProjectId) = Name[Option[ProjectMetaData]] {
    loadProjectMetaDataLog :+= id
    projects.get(id).map(_.projectMetaData)
  }

  var loadProjectLog = Vector.empty[ProjectId]
  override def getAllProjectEvents(id: ProjectId) = Name[DB.ProjectEvents] {
    loadProjectLog :+= id
    projects.need(id).projectLoad
  }

  override def saveProjectEvent(id: ProjectId)(ord: EventOrd, e: ActiveEvent, hrs: Collection) = Name[Option[Throwable]] {
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

  override def inDbTransaction[A](f: Name[A]) =
    f
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockServer extends Server.Algebra[Name] {
  private var prevFn = 0
  private var fns: Map[String, Any] =
    UnivEq.emptyMap

  override def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => Name[p.Response]) = Name[p.Instance] {
    prevFn += 1
    val key = prevFn.toString
    fns = fns.updated(key, localFn)
    ServerSideProc(key, p)
  }

  def run(p: ServerSideProc)(i: p.protocol.Input): p.protocol.Response = {
    val f = fns(p.key).asInstanceOf[p.protocol.Input => Name[p.protocol.Response]]
    f(i).value
  }

  var clock = Instant.now()
  override val now = Name(clock)

  var onDelay = List.empty[() => Unit]
  override def delay[A](f: Name[A], d: Duration) = Name[A] {
    clock = clock plus d
    onDelay match {
      case Nil    => ()
      case h :: t => onDelay = t; h()
    }
    f.value
  }

  var forked = Vector.empty[Name[Any]]
  override def fork[A](f: Name[A]) = Name[Unit] {
    forked :+= f
  }
  def runForked(): Unit = {
    forked.foreach(_.value)
    forked = Vector.empty
  }
}
