package shipreq.webapp.member.project.data

import shipreq.base.util.BinaryData

/** Encryption key used for client-side project encryption.
 *
 * DO NOT keep references to this around in the client. Use it immediately and then make sure to release it for GC.
 * Otherwise it'll be retained in-memory as long as the app is open, which is vulnerable to an attacker who has access
 * to another person's session.
 */
final case class ClientSideProjectEncryptionKey(value: BinaryData)

object ClientSideProjectEncryptionKey {
  implicit def univEq: UnivEq[ClientSideProjectEncryptionKey] = UnivEq.derive
}
