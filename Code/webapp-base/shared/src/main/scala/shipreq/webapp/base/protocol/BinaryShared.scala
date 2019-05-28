package shipreq.webapp.base.protocol

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import scala.util.Try
import shipreq.base.util.BinaryData

trait BinaryShared {

  final def decode(b: BinaryData, p: Protocol[Pickler]): Try[p.Type] =
    Try(decodeUnsafe(b, p))

  final def decodeUnsafe(b: BinaryData, p: Protocol[Pickler]): p.Type =
    UnpickleImpl(p.codec).fromBytes(b.unsafeByteBuffer)

  final def encode(p: Protocol[Pickler])(v: p.Type): BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl.intoBytes(v)(implicitly, p.codec))

}
