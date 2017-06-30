package shipreq.webapp.base.data

/**
  * Corresponds to either `confirmation_token` or `reset_password_token` in the DB.
  */
final case class SecurityToken(value: String)
