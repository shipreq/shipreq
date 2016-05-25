package shipreq.webapp.server.data

import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.taskman.api.UserId
import shipreq.webapp.base
import shipreq.webapp.server.util.ExternalId

final case class ProjectId(value: Long) extends TaggedLong

object ProjectId {

  val Extern = ExternalId.scheme[base.data.Project, ProjectId](
    ProjectId.apply, _.value,
    "F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")

  case class AndOwner(id: ProjectId, owner: UserId)
}
