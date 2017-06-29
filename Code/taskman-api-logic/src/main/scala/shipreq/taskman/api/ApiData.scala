package shipreq.taskman.api

import japgolly.univeq.UnivEq

final case class MsgId(value: Long)
object MsgId {
  implicit def univEqMsgId: UnivEq[MsgId] = UnivEq.derive
}

final case class UserId(value: Long)
object UserId {
  implicit def univEqUserId: UnivEq[UserId] = UnivEq.derive
}

final case class EmailAddr(value: String)
object EmailAddr {
  implicit def univEqEmailAddr: UnivEq[EmailAddr] = UnivEq.derive
}
