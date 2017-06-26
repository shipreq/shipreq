package shipreq.webapp.server.logic

import scala.collection.immutable.SortedMap
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec

/**
  * Naming conventions:
  *
  * =======
  * SELECT:
  * =======
  *
  * Prefixes:
  * - `get`    = SELECT [0,1]
  * - `getAll` = SELECT [0,n]
  * - `need`   = SELECT [1,1]
  *
  * Suffixes:
  * - `for<Criteria>`
  *
  * =======
  * INSERT:
  * =======
  *
  * - `save`   = A -> (Error \/)? Unit
  * - `create` = A -> (Error \/)? B
  */
object DB {

  trait Base[F[_]] {
    def inDbTransaction[A](f: F[A]): F[A]
  }

  trait SaveProjectEvent[F[_]] {
    def saveProjectEvent(id    : ProjectId)
                        (ord   : EventOrd,
                         event : ActiveEvent,
                         hashes: HashRec.Collection): F[Option[Throwable]]
  }

  trait ForHomeSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def createEmptyProject          (id: UserId): F[ProjectId]
    def getAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  trait ForProjectSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def getProjectHeader   (id: ProjectId): F[Option[ProjectHeader]]
    def getProjectMetaData (id: ProjectId): F[Option[ProjectMetaData]]
    def getAllProjectEvents(id: ProjectId): F[ProjectEvents]
  }
  type ProjectEvents = SortedMap[EventOrd, VerifiedEvent]

  trait Algebra[F[_]] extends ForHomeSpa[F] with ForProjectSpa[F]
}
