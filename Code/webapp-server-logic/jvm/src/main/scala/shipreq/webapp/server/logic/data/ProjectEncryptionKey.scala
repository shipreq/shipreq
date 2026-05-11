package shipreq.webapp.server.logic.data

import shipreq.base.util.BinaryData

final case class ProjectEncryptionKey(value: BinaryData)

object ProjectEncryptionKey {
  implicit def univEq: UnivEq[ProjectEncryptionKey] = UnivEq.derive
}
