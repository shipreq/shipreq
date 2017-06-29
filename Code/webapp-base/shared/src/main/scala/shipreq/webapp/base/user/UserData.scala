package shipreq.webapp.base.user

import japgolly.univeq.UnivEq

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class EmailAddr(value: String)
object EmailAddr {
  implicit def univEq: UnivEq[EmailAddr] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class UserId(value: Long)
object UserId {
  implicit def univEq: UnivEq[UserId] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class Username(value: String) {
  def with_@ : String =
    "@" + value
}
object Username {
  implicit def univEq: UnivEq[Username] = UnivEq.derive
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
}
