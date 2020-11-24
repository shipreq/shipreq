package shipreq.webapp.member.project.storage

import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.{ProjectId, UserId}
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import utest.assert

object TestData {

  def userId(i: Int): UserId.Public =
    Obfuscated("user-" + i)

  def projectId(i: Int): ProjectId.Public =
    Obfuscated("project-" + i)

  private val padding = "_" * 32

  def encKey(s: String): ClientSideProjectEncryptionKey = {
    assert(s.length <= 32)
    val s2 = (s + padding).take(32)
    ClientSideProjectEncryptionKey(BinaryData.fromStringBytes(s2))
  }

  val u1p1 = ClientSideStorage.Context(userId(1), projectId(1))
  val u2p1 = ClientSideStorage.Context(userId(2), projectId(1))
  val u1p2 = ClientSideStorage.Context(userId(1), projectId(2))

  val key_u1p1 = encKey("u1p1")
  val key_u2p1 = encKey("u2p1")
  val key_u1p2 = encKey("u1p2")
}
