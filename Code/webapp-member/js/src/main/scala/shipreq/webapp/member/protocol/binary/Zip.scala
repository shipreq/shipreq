package shipreq.webapp.member.protocol.binary

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Try
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.member.jsfacade.Pako

sealed trait Zip {
  val compress  : BinaryData => BinaryData
  val decompress: BinaryData => Try[BinaryData]

  final def decompressOrThrow: BinaryData => BinaryData =
    decompress(_).get
}

object Zip {

  /** @param level Compression level [1-9]
    * @param addHeaders Add header and adler32 crc
    */
  def apply(level: Int, addHeaders: Boolean): Zip = {
    val pako = Pako.instance
    val deflateOptions = js.Dynamic.literal().asInstanceOf[Pako.DeflateOptions]
    deflateOptions.level = level
    if (addHeaders)
      new Zip {
        override val compress   = data => pako.deflate(data, deflateOptions)
        override val decompress = data => Try(pako.inflate(data))
      }
    else
      new Zip {
        override val compress   = data => pako.deflateRaw(data, deflateOptions)
        override val decompress = data => Try(pako.inflateRaw(data))
      }
  }

  private implicit def binaryDataToPakoData(b: BinaryData): Pako.Data =
    b.unsafeUint8Array

  private implicit def pakoDataToBinaryData(d: Pako.Data): BinaryData =
    BinaryData.unsafeFromUint8Array(d.asInstanceOf[Uint8Array])
}