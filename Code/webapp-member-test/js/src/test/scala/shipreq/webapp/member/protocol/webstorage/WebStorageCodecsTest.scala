package shipreq.webapp.member.protocol.webstorage

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.webstorage._
import utest._

object WebStorageCodecsTest extends TestSuite {
  import AbstractWebStorage.Key

  override def tests = Tests {

    "binary" - {
      val key = WebStorageKey(Key("omg"), WebStorageCodecs.binary)

      implicit val ws = AbstractWebStorage.inMemory()

      assertEq(key.get.runNow(), None)

      val bin1 = BinaryData.fromHex("9876543210abcdef")
      key.set(bin1).runNow()
      assertEq(key.get.runNow(), Some(bin1))

      val bin2 = BinaryData.fromHex("7418529630")
      key.set(bin2).runNow()
      assertEq(key.get.runNow(), Some(bin2))
    }

  }
}
