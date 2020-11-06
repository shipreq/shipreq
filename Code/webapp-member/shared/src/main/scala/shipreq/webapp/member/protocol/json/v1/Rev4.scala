package shipreq.webapp.member.protocol.json.v1

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.protocol.json.JsonCodec

/** v1.4 */
object Rev4 {
  import JsonCodec.Implicits._
  import BaseData._

  implicit lazy val codecReqTypePos: JsonCodec[ReqTypePos] =
    JsonCodec.xmap(ReqTypePos)(_.value)

  implicit lazy val codecReqTypePosNES: JsonCodec[NonEmptySet[ReqTypePos]] =
    codecNES
}
