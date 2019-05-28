package shipreq.webapp.base.protocol

import boopickle.{Pickler, UnpickleImpl}
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

object BinaryJs extends shipreq.base.util.BinaryJs with BinaryShared {

  def decodeUnsafeFromArrayBuffer(ab: ArrayBuffer, p: Protocol[Pickler]): p.Type = {
    val bb = TypedArrayBuffer.wrap(ab)
    UnpickleImpl(p.codec).fromBytes(bb)
  }

}
