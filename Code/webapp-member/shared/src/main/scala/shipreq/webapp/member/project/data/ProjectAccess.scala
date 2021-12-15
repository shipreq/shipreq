package shipreq.webapp.member.project.data

import shipreq.webapp.base.data.{ProjectPerm, UserId}

final case class ProjectAccess(value: Map[UserId.Public, ProjectPerm])

object ProjectAccess {
  implicit def univEq: UnivEq[ProjectAccess] = UnivEq.derive

  val empty = apply(Map.empty)
}
