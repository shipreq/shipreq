package shipreq.webapp.member.protocol.binary

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Try
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.member.jsfacade.Pako

final case class Compression(compress  : BinaryData => BinaryData,
                             decompress: BinaryData => Try[BinaryData]) {

  def decompressOrThrow: BinaryData => BinaryData =
    decompress(_).get
}

object Compression {

  /** @param level Compression level [1-9]
    * @param addHeaders Add header and adler32 crc
    */
  def apply(level: Int, addHeaders: Boolean): Compression = {
    val pako = Pako.instance
    val deflateOptions = js.Dynamic.literal().asInstanceOf[Pako.DeflateOptions]
    deflateOptions.level = level
    if (addHeaders)
      Compression(
        compress   = data => pako.deflate(data, deflateOptions),
        decompress = data => Try(pako.inflate(data)),
      )
    else
      Compression(
        compress   = data => pako.deflateRaw(data, deflateOptions),
        decompress = data => Try(pako.inflateRaw(data)),
      )
  }

  private implicit def binaryDataToPakoData(b: BinaryData): Pako.Data =
    b.unsafeUint8Array

  private implicit def pakoDataToBinaryData(d: Pako.Data): BinaryData =
    BinaryData.unsafeFromUint8Array(d.asInstanceOf[Uint8Array])
}