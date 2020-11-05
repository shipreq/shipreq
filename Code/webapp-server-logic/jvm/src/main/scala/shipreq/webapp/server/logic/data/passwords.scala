package shipreq.webapp.server.logic.data

import java.util.Base64

// =====================================================================================================================

final case class PasswordHash(value: String) extends AnyVal

object PasswordHash {
  implicit def univEq: UnivEq[PasswordHash] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): PasswordHash =
    apply(Base64.getEncoder.encodeToString(bytes))
}

// =====================================================================================================================

final case class Salt(base64: String) extends AnyVal {
  def toBytes: Array[Byte] =
    Base64.getDecoder.decode(base64)
}

object Salt {
  implicit def univEq: UnivEq[Salt] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): Salt =
    apply(Base64.getEncoder.encodeToString(bytes))
}

// =====================================================================================================================

/** A hashed password and the salt used to generate the hash. */
final case class PasswordAndSalt(passwordHash: PasswordHash, salt: Salt)

object PasswordAndSalt {
  implicit def univEq: UnivEq[PasswordAndSalt] = UnivEq.derive
}
