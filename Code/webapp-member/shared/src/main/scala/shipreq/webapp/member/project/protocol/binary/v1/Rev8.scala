package shipreq.webapp.member.project.protocol.binary.v1

import shipreq.webapp.member.project.data._

/** v1.8
  *
  * Changes:
  *
  *   - add codec for [[ClientSideProjectEncryptionKey]]
  */
object Rev8 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._

  implicit lazy val picklerClientSideProjectEncryptionKey: Pickler[ClientSideProjectEncryptionKey] =
    transformPickler(ClientSideProjectEncryptionKey.apply)(_.value)

}