package shipreq.webapp.member.protocol.webstorage

import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.webstorage._
import shipreq.webapp.member.protocol.binary._

object WebStorageCodecs {

  val binaryString: ValueCodec[BinaryString] =
    ValueCodec.string.xmap(BinaryString.encoded)(_.encoded)

  val binary: ValueCodec[BinaryData] =
    binaryString.xmap(_.binaryValue)(BinaryString.apply)

  val binaryAsync: ValueCodec.Async[BinaryData] =
    binary.async

  def binaryFormat[A](fmt: BinaryFormat[A]): ValueCodec.Async[A] =
    binaryAsync.xmapAsync(fmt.decode)(fmt.encode)
}
