package shipreq.webapp.member.project.event

import shipreq.base.util.IsoBool

/**
 * Specifies the level of trust associated with data.
 */
sealed trait Trust extends IsoBool[Trust] {
  override final def companion = Trust
}

object Trust extends IsoBool.Object[Trust] {
  override def positive = Trusted
  override def negative = Untrusted
}

/**
 * The data has already been verified by the server as being acceptable/valid.
 * Typically refers to data coming out of the database.
 *
 * Perform no validation. Speed is king.
 */
case object Trusted extends Trust

/**
 * The data has not been verified and could be invalid/malicious.
 * Typically refers to data received from a client.
 *
 * Validate everything. Data integrity is king.
 */
case object Untrusted extends Trust