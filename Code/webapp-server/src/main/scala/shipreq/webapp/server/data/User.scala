package shipreq.webapp.server.data

import java.time.Instant
import shipreq.webapp.base.user._

//case class UserDetail(name: String, newsletter: Boolean)

///** Marks a string as being an ISO-8601 representation of a datetime. */
//case class ISO8601(value: String) extends AnyVal

//case class UserSupplementalInfo(ps: PasswordAndSalt, registeredAt: ISO8601)

case class UserRegistrationInfo(id                : UserId,
                                confirmationToken : Option[String],
                                confirmationSentAt: Option[Instant],
                                confirmedAt       : Option[Instant])

case class ResetPasswordInfo(token: Option[String], sentAt: Option[Instant])

