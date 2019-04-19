package shipreq.webapp.base.protocol2

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import scala.util.Try
import shipreq.base.util.BinaryData

object BinaryJvm {

  def unsafeDecode(b: BinaryData, p: Protocol[Pickler]): p.Type =
    UnpickleImpl(p.codec).fromBytes(b.toByteBuffer)

  def attemptDecode(b: BinaryData, p: Protocol[Pickler]): Try[p.Type] =
    Try(unsafeDecode(b, p))

  def encode(p: Protocol[Pickler])(v: p.Type): BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl.intoBytes(v)(implicitly, p.codec))

}
