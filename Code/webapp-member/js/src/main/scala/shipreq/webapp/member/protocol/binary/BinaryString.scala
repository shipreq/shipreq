package shipreq.webapp.member.protocol.binary

import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.member.jsfacade.Base32768

/** Binary data efficiently encoded as a UTF-16 string. */
final class BinaryString(val encoded: String) {

  lazy val binaryValue: BinaryData =
    BinaryData.unsafeFromUint8Array(Base32768.decode(encoded))
}

object BinaryString {

  def encoded(str: String): BinaryString =
    new BinaryString(str)

  def apply(bin: BinaryData): BinaryString =
    encoded(Base32768.encode(bin.unsafeUint8Array))
}
