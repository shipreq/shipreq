package shipreq.webapp.member.test

import japgolly.scalajs.react.AsyncCallback
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node
import shipreq.base.util.BinaryData
import shipreq.webapp.member.protocol.binary.Encryption

object TestEncryption {

  lazy val engine: Encryption.Engine =
    Encryption.Engine.from(Node.webCrypto)
      .getOrThrow("Node.webCrypto not accepted as an Encryption.Engine")

  def apply(key: BinaryData): AsyncCallback[Encryption] =
    engine(key)

  object UnsafeTypes {
    implicit def binaryDataFromString(str: String): BinaryData = {
      val bytes = str.getBytes
      assert(bytes.length == str.length)
      BinaryData.unsafeFromArray(bytes)
    }
  }
}
