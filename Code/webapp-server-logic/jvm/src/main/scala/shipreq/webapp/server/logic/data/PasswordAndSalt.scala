package shipreq.webapp.server.logic.data

/** A hashed password and the salt used to generate the hash. */
final case class PasswordAndSalt(passwordHash: PasswordHash, salt: Salt)

object PasswordAndSalt {
  implicit def univEq: UnivEq[PasswordAndSalt] = UnivEq.derive
}
