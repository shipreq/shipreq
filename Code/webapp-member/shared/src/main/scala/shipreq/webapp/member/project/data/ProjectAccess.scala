package shipreq.webapp.member.project.data

import shipreq.base.util.Permission
import shipreq.webapp.base.data.{ProjectCreator, ProjectPerm, UserId}

final case class ProjectAccess(asMap: Map[UserId.Public, ProjectPerm]) {

  def apply(user: UserId.Public): Option[ProjectPerm] =
    asMap.get(user)

  def need(user: UserId.Public): ProjectPerm =
    apply(user).get

  def adminIterator(): Iterator[UserId.Public] =
    asMap.iterator.filter(_._2 ==* ProjectPerm.Admin).map(_._1)

  def hasAdmin: Boolean =
    adminIterator().nonEmpty

  def update(updates: Map[UserId.Public, Option[ProjectPerm]]): ProjectAccess = {
    var m = asMap
    updates.foreach {
      case (u, None)    => m -= u
      case (u, Some(p)) => m = m.updated(u, p)
    }
    ProjectAccess(m)
  }

  /** Checks if the given user has the required permission. */
  def require(requiredPerm: ProjectPerm, user: UserId.Public): Permission =
    requiredPerm.isSatisfiedBy(apply(user))
}

object ProjectAccess {
  implicit def univEq: UnivEq[ProjectAccess] = UnivEq.derive

  def empty: ProjectAccess =
    apply(Map.empty)

  def init(c: ProjectCreator): ProjectAccess =
    apply(Map.empty[UserId.Public, ProjectPerm].updated(c.userId, ProjectPerm.Admin))
}
