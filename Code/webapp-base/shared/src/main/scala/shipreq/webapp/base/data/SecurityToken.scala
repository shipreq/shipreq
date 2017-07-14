package shipreq.webapp.base.data

import japgolly.univeq.UnivEq

/**
  * Corresponds to either `confirmation_token` or `reset_password_token` in the DB.
  */
final case class SecurityToken(value: String)

object SecurityToken {
  implicit def univEq: UnivEq[SecurityToken] = UnivEq.derive

  sealed trait Status
  object Status {
    case object Valid   extends Status
    case object Invalid extends Status
    case object Expired extends Status
  }
}
