package shipreq.webapp.member.protocol.binary.v1

import shipreq.webapp.member.project.data._

/** v1.4 */
object Rev4 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import BaseMemberData2._

  implicit lazy val picklerReqTypePosNES: Pickler[NonEmptySet[ReqTypePos]] =
    pickleNES
}