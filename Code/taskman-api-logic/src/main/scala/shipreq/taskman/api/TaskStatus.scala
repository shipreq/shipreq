package shipreq.taskman.api

sealed trait TaskStatus {
  def isArchived: Boolean
}

object TaskStatus {
  sealed trait Live extends TaskStatus {
    def isArchived = false
  }
  case object Unassigned extends Live
  case object NodeAssigned extends Live
  case object Working extends Live

  sealed trait Archived extends TaskStatus  {
    def isArchived = true
  }
  case object Complete extends Archived
  case object Aborted extends Archived

  implicit def univEq: UnivEq[TaskStatus] = UnivEq.derive
}