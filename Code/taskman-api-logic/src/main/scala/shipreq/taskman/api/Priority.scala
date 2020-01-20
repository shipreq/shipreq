package shipreq.taskman.api

import japgolly.univeq.UnivEq
import Task._

final case class Priority(value: Short) extends AnyVal {
  def inc = Priority((value.toInt + 1).toShort)
}

object Priority {

  implicit def univEq: UnivEq[Priority] = UnivEq.derive

  @inline def Low         = Priority(20)
  @inline def Medium      = Priority(50)
  @inline def High        = Priority(80)
  @inline def UserWaiting = Priority(100)

  val of: Task => Priority = {

    case _: RegistrationRequested
       | _: ReRegistrationAttempted
       | _: PasswordResetRequested
              => UserWaiting

    case _: ReportServerError
              => High

    case _: DummyTask
       | _: SendDiagEmail
              => Medium

    case _: RegistrationCompleted
       | _: LandingPageHit
       | _: SyncToMailingList
       | _: UserFeedbackReceived
       | _: UserUpdated
              => Low
  }
}