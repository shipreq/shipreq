package shipreq.webapp.server.security

import japgolly.univeq._
import org.apache.shiro.util.ByteSource
import org.apache.shiro.codec.Base64
import org.apache.shiro.crypto.hash.SimpleHash
import shipreq.webapp.base.user.PlainTextPassword
import Oshiro._

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class PasswordHash(value: String) extends AnyVal

object PasswordHash {
  implicit def univEq: UnivEq[PasswordHash] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final case class Salt(byteSource: ByteSource) extends AnyVal {
  def toBase64: String =
    byteSource.toBase64

  def hash(plainTextPassword: PlainTextPassword): PasswordHash =
    PasswordHash(new SimpleHash(HashingAlgorithm, plainTextPassword.value, byteSource, HashingIterations).toBase64)
}

object Salt {
  def random(): Salt =
    apply(RNG.nextBytes())

  def fromBase64(base64: String): Salt =
    apply(ByteSource.Util.bytes(Base64.decode(base64)))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * A hashed password and the salt used to generate the hash.
 */
final case class PasswordAndSalt(hashedPassword: PasswordHash, salt: Salt) {
  def matches(plainTextPassword: PlainTextPassword): Boolean =
    salt.hash(plainTextPassword) ==* hashedPassword
}

/**
 * Methods for creating and generating passwords & salt.
 */
object PasswordAndSalt {

  def create(plainTextPassword: PlainTextPassword, salt: Salt): PasswordAndSalt =
    PasswordAndSalt(salt.hash(plainTextPassword), salt)

  def createWithRandomSalt(plainTextPassword: PlainTextPassword): PasswordAndSalt =
    create(plainTextPassword, Salt.random())
}