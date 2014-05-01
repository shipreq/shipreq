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

  /** Interact with the Support desk. */
  case class SupportOp[A](op: Support.API[A]) extends Bop[A]

  /** Lookup a user's details in the ShipReq DB. */
  case class FindShipReqUser(q: UserId \/ EmailAddr) extends Bop[Option[ShipReqUser]]

  /** Lookup users' details in the ShipReq DB. */
  case class FindShipReqUsers(sqlCond: Option[String]) extends Bop[List[ShipReqUser]]
}

case class ShipReqUser(id: UserId, username: String, email: EmailAddr, name: String, newsletter: Boolean)
