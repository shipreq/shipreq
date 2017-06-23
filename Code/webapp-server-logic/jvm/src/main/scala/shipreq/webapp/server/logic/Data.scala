package shipreq.webapp.server.logic

import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.Project

final case class ProjectId(value: Long) extends TaggedLong // not AnyVal, it gets boxed

object ProjectId {

  val Extern = ExternalId.scheme[Project, ProjectId](
    ProjectId.apply, _.value,
    "F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")

  final case class AndOwner(id: ProjectId, owner: UserId)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
  * @param userId The only user with access to the project.
  *               This will change in Phase 3 when collaborative features are added.
  */
final case class ProjectHeader(userId: UserId, name: Project.Name)
