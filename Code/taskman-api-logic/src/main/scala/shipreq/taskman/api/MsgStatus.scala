package shipreq.taskman.api

import japgolly.univeq.UnivEq

sealed trait MsgStatus {
  def isArchived: Boolean
}

object MsgStatus {
  sealed trait Live extends MsgStatus {
    def isArchived = false
  }
  case object Unassigned extends Live
  case object NodeAssigned extends Live
  case object Working extends Live

  sealed trait Archived extends MsgStatus  {
    def isArchived = true
  }
  case object Complete extends Archived
  case object Aborted extends Archived

  implicit def univEq: UnivEq[MsgStatus] = UnivEq.derive
}