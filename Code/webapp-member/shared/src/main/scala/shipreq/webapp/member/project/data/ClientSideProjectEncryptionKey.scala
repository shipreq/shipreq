package shipreq.webapp.member.project.data

import shipreq.base.util.BinaryData

final case class ClientSideProjectEncryptionKey(value: BinaryData)

object ClientSideProjectEncryptionKey {
  implicit def univEq: UnivEq[ClientSideProjectEncryptionKey] = UnivEq.derive
}
