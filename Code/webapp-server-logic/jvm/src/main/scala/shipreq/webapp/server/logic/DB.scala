package shipreq.webapp.server.logic

import scala.collection.immutable.SortedMap
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec

object DB {
  type ProjectLoad = SortedMap[EventOrd, VerifiedEvent]

  trait Algebra[F[_]] {

    def loadProjectMetaDataAndUser(id: ProjectId): F[Option[(ProjectMetaData, UserId)]]

    def loadProject(id: ProjectId): F[ProjectLoad]

    def saveProjectEvent(id: ProjectId, ord: EventOrd, e: ActiveEvent, hrs: HashRec.Collection): F[Option[Throwable]]

    def inDbTransaction[A](f: F[A]): F[A]
  }
}
