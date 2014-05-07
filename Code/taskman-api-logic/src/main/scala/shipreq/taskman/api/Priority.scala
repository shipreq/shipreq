package shipreq.taskman.api

import Msg._

case class Priority(value: Short) extends AnyVal {
  def inc = Priority((value.toInt + 1).toShort)
}

object Priority {

  @inline def Low         = Priority(20)
  @inline def Medium      = Priority(50)
  @inline def High        = Priority(80)
  @inline def UserWaiting = Priority(100)

  def of(m: Msg): Priority = m match {

    case _: RegistrationRequested
       | _: ReRegistrationAttempted
       | _: PasswordResetRequested
              => UserWaiting

    case _: WebappErrorOccurred
              => High

    case _: DummyMsg
       | _: SendDiagEmail
              => Medium

    case _: RegistrationCompleted
       | _: LandingPageHit
       | _: SyncToMailingList
       | _: UserUpdated
              => Low
  }
}