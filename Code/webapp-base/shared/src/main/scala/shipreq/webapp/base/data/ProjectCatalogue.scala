package shipreq.webapp.base.data

import java.time.Instant
import shipreq.base.util.univeq._
import ProjectCatalogue._

case class ProjectCatalogue(items: List[Item]) extends AnyVal

object ProjectCatalogue {

  case class Item(id           : Project.XId,
                  name         : Project.Name,
                  eventCount   : Int,
                  reqCount     : Int,
                  createdAt    : Instant,
                  lastUpdatedAt: Option[Instant]) {

    def lastUpdatedOrCreatedAt: Instant =
      lastUpdatedAt.getOrElse(createdAt)
  }

  implicit def univEqItem: UnivEq[Item] = UnivEq.derive

  implicit def univEqPC: UnivEq[ProjectCatalogue] = UnivEq.derive
}