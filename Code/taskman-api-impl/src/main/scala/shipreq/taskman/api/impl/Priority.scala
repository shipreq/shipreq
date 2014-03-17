package shipreq.taskman.api.impl

import shipreq.taskman.api.TaskDef
import TaskDef._

private[api] case class Priority(value: Short)

private[api] object Priority {

  val High   = Priority(100)
  val Medium = Priority(50)
  val Low    = Priority(20)
  @inline def UserWaiting = High

  def forTask(t: TaskDef): Priority = t match {
    case _: RegistrationRequested
       | _: ReRegistrationAttempted
       | _: PasswordResetRequested
              => UserWaiting
    case _: RegistrationCompleted
       | _: LandingPageHit
              => Low
  }

}