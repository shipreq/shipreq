package shipreq.webapp.server.logic.config

import io.circe._
import io.circe.parser.decode
import japgolly.clearconfig._
import shipreq.base.util.{Allow, Permission}
import shipreq.webapp.base.data._

// Temporary measure until phase 3 is complete
final case class ProjectAccessHacks(additionalAccess: Multimap[UserId, Set, ProjectId]) {
  def apply(requester: User, projectId: ProjectId): Permission =
    Allow.when(additionalAccess(requester.id).contains(projectId))
}

object ProjectAccessHacks {

  private implicit def configValueCodec: Decoder[ProjectAccessHacks] =
    Decoder[Map[Long, List[Long]]].map { untyped =>
      val mm = untyped.foldLeft(Multimap.empty[UserId, Set, ProjectId]) { case (m, (k, vs)) =>
        m.addvs(UserId(k), vs.map(ProjectId.apply).toSet)
      }
      ProjectAccessHacks(mm)
    }

  implicit def config: ConfigValueParser[ProjectAccessHacks] =
    ConfigValueParser(decode[ProjectAccessHacks](_).leftMap(_.getMessage))

  def empty: ProjectAccessHacks =
    apply(Multimap.empty)
}