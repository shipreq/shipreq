package shipreq.webapp.base.protocol2

import boopickle.Pickler
import java.nio.ByteBuffer
import scalaz.\/
import shipreq.webapp.base.protocol.BinCodecGeneric

object WebSocketShared {
  import BinCodecGeneric.{byteBufferPickler => _, _}

  implicit def byteBufferPickler: Pickler[ByteBuffer] =
    BinCodecGeneric.byteBufferPickler

  val protocolCS: Pickler[(Int, ByteBuffer)] =
    Tuple2Pickler

  def protocolSC[Push: Pickler]: Pickler[Push \/ (Int, ByteBuffer)] = {
    implicit val r: Pickler[(Int, ByteBuffer)] = Tuple2Pickler
    pickleXor[Push, (Int, ByteBuffer)]
  }
}
