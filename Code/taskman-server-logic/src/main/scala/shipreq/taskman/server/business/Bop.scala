package shipreq.taskman.server.business

import scalaz.\/
import shipreq.taskman.api.Types._

/**
 * Business Operation.
 * An operation in the domain of business logic.
 */
sealed trait Bop[A]

object Bop {

  /** Send an email. */
  case class SendEmail(e: Email.Envelope, c: Email.Content) extends Bop[Unit]

  /** Manage the mailing list. */
  case class MailingListOp[A](op: MailingList.API[A]) extends Bop[A]

  /** Lookup someone's ShipReq user record. */
  case class LookupShipReqUser(q: UserId \/ EmailAddr) extends Bop[Option[ShipReqUser]]
}

case class ShipReqUser(id: UserId, username: String, email: EmailAddr, name: String, newsletter: Boolean)
