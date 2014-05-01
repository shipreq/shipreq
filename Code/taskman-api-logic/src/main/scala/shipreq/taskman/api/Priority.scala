package shipreq.taskman.api

import Msg._

case class Priority(value: Short) extends AnyVal {
  def inc = Priority((value.toInt + 1).toShort)
}

object Priority {

  @inline def High        = Priority(100)
  @inline def Medium      = Priority(50)
  @inline def Low         = Priority(20)
  @inline def UserWaiting = High

  def of(m: Msg): Priority = m match {

    case _: RegistrationRequested
       | _: ReRegistrationAttempted
       | _: PasswordResetRequested
              => UserWaiting

    case _: DummyMsg
       | _: SendDiagEmail
              => Medium

    case _: RegistrationCompleted
       | _: LandingPageHit
       | _: SyncToMailingList
              => Low
  }
}