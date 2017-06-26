package shipreq.webapp.server.logic

import scala.collection.immutable.SortedMap
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec

object DB {

  trait Base[F[_]] {
    def inDbTransaction[A](f: F[A]): F[A]
  }

  trait SaveProjectEvent[F[_]] {
    def saveProjectEvent(id    : ProjectId,
                         ord   : EventOrd,
                         event : ActiveEvent,
                         hashes: HashRec.Collection): F[Option[Throwable]]
  }

  trait ForHomeSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def createProject(id: UserId): F[ProjectId]
    def findAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  trait ForProjectSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def loadProjectHeader  (id: ProjectId): F[Option[ProjectHeader]]
    def loadProjectMetaData(id: ProjectId): F[Option[ProjectMetaData]]
    def loadProject        (id: ProjectId): F[ProjectLoad]
  }
  type ProjectLoad = SortedMap[EventOrd, VerifiedEvent]

  trait Algebra[F[_]] extends ForHomeSpa[F] with ForProjectSpa[F]
}
