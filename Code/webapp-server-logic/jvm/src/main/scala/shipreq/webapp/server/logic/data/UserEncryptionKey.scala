package shipreq.webapp.server.logic.data

import shipreq.base.util.BinaryData

final case class UserEncryptionKey(value: BinaryData)

object UserEncryptionKey {
  implicit def univEq: UnivEq[UserEncryptionKey] = UnivEq.derive
}
