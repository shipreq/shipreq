package shipreq.webapp.base.user

import japgolly.univeq.UnivEq
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.data.Obfuscated

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class PlainTextPassword(value: String) {
  def hashStr: String = "%08X".format(value.##)
}
object PlainTextPassword {
  implicit def univEq: UnivEq[PlainTextPassword] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class EmailAddr(value: String) {
  def mailto: String =
    "mailto:" + value
}
object EmailAddr {
  implicit def univEq: UnivEq[EmailAddr] = UnivEq.derive

  def isEmailAddr(s: String): Boolean =
    // >0 instead of !=-1 because @golly will be interpreted as a username and email addresses can't start with @
    s.indexOf('@') > 0
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class UserId(value: Long)
object UserId {
  implicit def univEq: UnivEq[UserId] = UnivEq.derive

  /** The real UserId is never directly exposed to users. Publicly it has a different ID. */
  type Public = Obfuscated[UserId]
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class Username(value: String) {
  def with_@ : String =
    "@" + value
}
object Username {
  implicit def univEq: UnivEq[Username] = UnivEq.derive

  def orEmail(usernameOrEmail: String): Username \/ EmailAddr =
    if (EmailAddr.isEmailAddr(usernameOrEmail))
      \/-(EmailAddr(usernameOrEmail))
    else
      -\/(Username(usernameOrEmail))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class User(id      : UserId,
                      username: Username)

object User {
  implicit def univEq: UnivEq[User] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/** A person's full name.
  *
  * E.g. "David Barri", "C K Panipuri".
  *
  * Don't try to reliably extract a given/family name.
  * https://www.w3.org/International/questions/qa-personal-names#fielddesign
  */
final case class PersonName(value: String)
object PersonName {
  implicit def univEq: UnivEq[PersonName] = UnivEq.derive
}
