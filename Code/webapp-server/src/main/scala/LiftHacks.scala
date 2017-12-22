package net.liftweb.http

object LiftHacks {
  def sessionInactivityLength(s: LiftSession) = s.inactivityLength
}
