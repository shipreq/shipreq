package shipreq.webapp
package security

import org.apache.shiro.util.ByteSource
import org.apache.shiro.codec.Base64
import org.apache.shiro.crypto.hash.SimpleHash
import Oshiro._
import lib.Types._

/**
 * A hashed password and the salt used to generate the hash.
 */
final case class PasswordAndSalt(hashedPassword: String @@ Hashed, saltBytes: ByteSource) {
  def salt = saltBytes.toBase64

  def hash(plainTextPassword: String): String @@ Hashed =
    PasswordAndSalt.hash(plainTextPassword, saltBytes)

  def matches(plainTextPassword: String): Boolean =
    hash(plainTextPassword) == hashedPassword
}

/**
 * Methods for creating and generating passwords & salt.
 */
final object PasswordAndSalt {

  def create(plainTextPassword: String, salt: ByteSource): PasswordAndSalt =
    PasswordAndSalt(hash(plainTextPassword, salt), salt)

  def createWithRandomSalt(plainTextPassword: String): PasswordAndSalt =
    create(plainTextPassword, RNG.nextBytes())

  def hash(plainTextPassword: String, salt: ByteSource): String @@ Hashed =
    new SimpleHash(HashingAlgorithm, plainTextPassword, salt, HashingIterations).toBase64.tag

  def salt(salt: String): ByteSource =
    ByteSource.Util.bytes(Base64.decode(salt))

  def restore(password: String @@ Hashed, saltStr: String): PasswordAndSalt =
    PasswordAndSalt(password, salt(saltStr))
}