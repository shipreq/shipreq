package shipreq.taskman.server.business

/**
 * Business Operation.
 * An operation in the domain of business logic.
 */
sealed trait Bop[A]

object Bop {

  case class SendEmail(e: Email.Envelope, c: Email.Content) extends Bop[Unit]
}
