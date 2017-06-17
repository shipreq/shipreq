package shipreq.webapp.server.logic

import scala.collection.immutable.SortedMap
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.ProjectCatalogue
import shipreq.webapp.base.event.{ActiveEvent, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec

object DB {
  type ProjectLoad = SortedMap[EventSeq, VerifiedEvent]

  trait Algebra[F[_]] {

    def loadProjectSummary(id: ProjectId): F[Option[(ProjectCatalogue.Item, UserId)]]

    def loadProject(id: ProjectId): F[ProjectLoad]

    def saveProjectEvent(id: ProjectId, seq: EventSeq, e: ActiveEvent, hrs: HashRec.Collection): F[Option[Throwable]]

    def inDbTransaction[A](f: F[A]): F[A]
  }
}
