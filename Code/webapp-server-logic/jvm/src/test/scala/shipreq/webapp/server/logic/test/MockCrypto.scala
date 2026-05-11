package shipreq.webapp.server.logic.test

import cats.Eval
import shipreq.base.util._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.data._

object MockCrypto {
  def generateKey256(i: Int): BinaryData = {
    val a = new Array[Byte](32)
    a(31) = (i & 0xff).toByte
    a(30) = ((i >> 8) & 0xff).toByte
    BinaryData.unsafeFromArray(a)
  }
}

final class MockCrypto extends Crypto[Eval] {

  private val default = Crypto.default[Eval]

  private var nextKey = 0

  override def generateKey256 = Eval.always[BinaryData] {
    val i = nextKey
    nextKey += 1
    MockCrypto.generateKey256(i)
  }

  def generateUserKey() =
    UserEncryptionKey(generateKey256.value)

  def generateProjectKey() =
    ProjectEncryptionKey(generateKey256.value)

  override def sha256(input: BinaryData): BinaryData =
    default.sha256(input)
}
