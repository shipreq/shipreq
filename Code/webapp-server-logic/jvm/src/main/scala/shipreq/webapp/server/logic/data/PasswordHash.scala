package shipreq.webapp.server.logic.data

import java.util.Base64

final case class PasswordHash(value: String) extends AnyVal

object PasswordHash {
  implicit def univEq: UnivEq[PasswordHash] = UnivEq.derive

  def fromBytes(bytes: Array[Byte]): PasswordHash =
    apply(Base64.getEncoder.encodeToString(bytes))
}
