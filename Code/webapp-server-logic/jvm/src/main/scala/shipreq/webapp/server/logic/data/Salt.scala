package shipreq.webapp.server.logic.data

import java.util.Base64

final case class Salt(base64: String) extends AnyVal {
  def toBytes: Array[Byte] =
    Base64.getDecoder.decode(base64)
}

object Salt {
  implicit def univEq: UnivEq[Salt] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): Salt =
    apply(Base64.getEncoder.encodeToString(bytes))
}
