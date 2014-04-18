package shipreq.taskman.server.business

/**
 * Business Operation.
 * An operation in the domain of business logic.
 */
sealed trait Bop[A]

object Bop {

  /** Send an email. */
  case class SendEmail(e: Email.Envelope, c: Email.Content) extends Bop[Unit]

  /** Interact with MailChimp through its API. */
  case class MailChimpOp[A](op: MailChimp.API[A]) extends Bop[A]
}
