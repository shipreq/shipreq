package shipreq.webapp.member.test

import japgolly.scalajs.react.AsyncCallback
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node
import shipreq.base.util.BinaryData
import shipreq.webapp.member.protocol.binary.Encryption

object TestEncryption {

  def apply(key: BinaryData): AsyncCallback[Encryption] =
    Encryption(Node.webCrypto, key).map(_.getOrThrow("webCrypto not available"))

  object UnsafeTypes {
    implicit def binaryDataFromString(str: String): BinaryData = {
      val bytes = str.getBytes
      assert(bytes.length == str.length)
      BinaryData.unsafeFromArray(bytes)
    }
  }
}
