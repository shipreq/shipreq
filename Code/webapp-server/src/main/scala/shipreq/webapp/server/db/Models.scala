package shipreq.webapp.server
package db

import org.joda.time.DateTime
import shipreq.taskman.api.{EmailAddr, UserId}
import lib.Types._
import security.PasswordAndSalt

// ===================================================================================================================
// User

case class UserDescriptor(id: UserId, username: Username, email: EmailAddr, roles: Set[String]) {
  final def hasRole(role: String): Boolean = roles.contains(role)
}

object UserDescriptor {
  def roleStr(roles: Set[String]): Option[String] =
    if (roles.isEmpty)
      None
    else
      Some(roles.mkString(","))
}

case class UserDetail(name: String, newsletter: Boolean)

case class UserSupplementalInfo(ps: PasswordAndSalt, registeredAt: ISO8601)

case class UserRegistrationInfo(
  id: UserId,
  confirmationToken: Option[String],
  confirmationSentAt: Option[DateTime],
  confirmedAt: Option[DateTime])

case class ResetPasswordInfo(token: Option[String], sentAt: Option[DateTime])

case class UsrCount(registered: Long, total: Long) {
  def pending = total - registered
}

