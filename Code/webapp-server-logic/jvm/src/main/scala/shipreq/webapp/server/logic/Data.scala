package shipreq.webapp.server.logic

import japgolly.univeq.UnivEq
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.base.data.{Project, Username}

final case class ProjectId(value: Long) extends TaggedLong // not AnyVal, it gets boxed

object ProjectId {

  val Extern = ExternalId.scheme[Project, ProjectId](
    ProjectId.apply, _.value,
    "F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")

  final case class AndOwner(id: ProjectId, owner: UserId)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class User(id      : UserId,
                      username: Username,
                      email   : EmailAddr,
                      roles   : Set[String]) {

  // I hope it's obvious that this is a temporarily measure.. phase 3!
  def hasRole(role: String): Boolean =
    roles.contains(role)
}

object User {
  implicit def univEq: UnivEq[User] = UnivEq.derive

  def roleStr(roles: Set[String]): Option[String] =
    if (roles.isEmpty)
      None
    else
      Some(roles.mkString(","))
}


// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
  * @param userId The only user with access to the project.
  *               This will change in Phase 3 when collaborative features are added.
  */
final case class ProjectHeader(userId: UserId, name: Project.Name)
