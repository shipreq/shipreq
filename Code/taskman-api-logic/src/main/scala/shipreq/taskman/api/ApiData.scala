package shipreq.taskman.api

final case class TaskId(value: Long)
object TaskId {
  implicit def univEq: UnivEq[TaskId] = UnivEq.derive
}

final case class UserId(value: Long)
object UserId {
  implicit def univEq: UnivEq[UserId] = UnivEq.derive
}

final case class EmailAddr(value: String)
object EmailAddr {
  implicit def univEq: UnivEq[EmailAddr] = UnivEq.derive
}
