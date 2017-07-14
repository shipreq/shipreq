package shipreq.webapp.server.logic

import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._

/**
  * @param userId The only user with access to the project.
  *               This will change in Phase 3 when collaborative features are added.
  */
final case class ProjectHeader(userId: UserId, name: Project.Name)

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class PasswordHash(value: String) extends AnyVal
object PasswordHash {
  implicit def univEq: UnivEq[PasswordHash] = UnivEq.derive
}

final case class Salt(base64: String) extends AnyVal
object Salt {
  implicit def univEq: UnivEq[Salt] = UnivEq.derive
}

/** A hashed password and the salt used to generate the hash. */
final case class PasswordAndSalt(passwordHash: PasswordHash, salt: Salt)
object PasswordAndSalt {
  implicit def univEq: UnivEq[PasswordAndSalt] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class IP(value: String)
object IP {
  implicit def univEq: UnivEq[IP] = UnivEq.derive
}
