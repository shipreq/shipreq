package shipreq.webapp.member.project.data

import shipreq.webapp.base.data.{ProjectPerm, Username}

final case class ProjectAccess(value: Map[Username, ProjectPerm])

object ProjectAccess {
  implicit def univEq: UnivEq[ProjectAccess] = UnivEq.derive

  val empty = apply(Map.empty)
}
