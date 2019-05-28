package shipreq.webapp.server.logic

import japgolly.univeq.UnivEq
import java.util.Base64

final case class PasswordHash(value: String) extends AnyVal
object PasswordHash {
  implicit def univEq: UnivEq[PasswordHash] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): PasswordHash =
    apply(Base64.getEncoder.encodeToString(bytes))
}

final case class Salt(base64: String) extends AnyVal {
  def toBytes: Array[Byte] =
    Base64.getDecoder.decode(base64)
}
object Salt {
  implicit def univEq: UnivEq[Salt] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): Salt =
    apply(Base64.getEncoder.encodeToString(bytes))
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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class Cookie(name       : Cookie.Name,
                        value      : String,
                        maxAgeInSec: Option[Int],
                        httpOnly   : Option[Boolean],
                        secure     : Option[Boolean])

object Cookie {
  final case class Name(value: String)

  type LookupFn = Name => Option[String]

  final case class Update(add: List[Cookie], remove: List[Cookie.Name])

  object Update {
    val empty = apply(Nil, Nil)
    def add(c: Cookie) = apply(c :: Nil, Nil)
  }

  implicit def univEqName  : UnivEq[Name]   = UnivEq.derive
  implicit def univEqCookie: UnivEq[Cookie] = UnivEq.derive
  implicit def univEqUpdate: UnivEq[Update] = UnivEq.derive
}
