package shipreq.webapp.server.data

import org.joda.time.DateTime
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.server.security.PasswordAndSalt

case class Username(value: String) extends AnyVal

case class UserDescriptor(id      : UserId,
                          username: Username,
                          email   : EmailAddr,
                          roles   : Set[String]) {

  final def hasRole(role: String): Boolean =
    roles.contains(role)
}

object UserDescriptor {
  def roleStr(roles: Set[String]): Option[String] =
    if (roles.isEmpty)
      None
    else
      Some(roles.mkString(","))
}

case class UserDetail(name: String, newsletter: Boolean)

/** Marks a string as being an ISO-8601 representation of a datetime. */
case class ISO8601(value: String) extends AnyVal

case class UserSupplementalInfo(ps: PasswordAndSalt, registeredAt: ISO8601)

case class UserRegistrationInfo(id                : UserId,
                                confirmationToken : Option[String],
                                confirmationSentAt: Option[DateTime],
                                confirmedAt       : Option[DateTime])

