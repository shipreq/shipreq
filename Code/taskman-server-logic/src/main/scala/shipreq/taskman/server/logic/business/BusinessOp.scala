package shipreq.taskman.server.logic.business

import shipreq.base.util.Util.simpleNameMemo
import shipreq.taskman.api.{EmailAddr, UserId}

/**
 * Business Operation.
 * An operation in the domain of business logic.
 */
sealed trait BusinessOp[A]

object BusinessOp {

  def simpleName: BusinessOp[_] => String = {
    case MailingListOp(i) => s"MailingListOp(${simpleNameMemo(i.getClass)})"
    case SupportOp(i)     => s"SupportOp(${simpleNameMemo(i.getClass)})"
    case op               => simpleNameMemo(op.getClass)
  }

  /** Send an email. */
  final case class SendEmail(envelope: Email.Envelope, content: Email.Content) extends BusinessOp[Unit]

  /** Manage the mailing list. */
  final case class MailingListOp[A](op: MailingList.API[A]) extends BusinessOp[A]

  /** Interact with the Support desk. */
  final case class SupportOp[A](op: Support.API[A]) extends BusinessOp[A]

  /** Lookup a user's details in the ShipReq DB. */
  final case class FindShipReqUser(query: UserId \/ EmailAddr) extends BusinessOp[Option[ShipReqUser]]

  /** Lookup users' details in the ShipReq DB. */
  final case class FindShipReqUsers(sqlCond: Option[String]) extends BusinessOp[List[ShipReqUser]]
}

final case class ShipReqUser(id        : UserId,
                             username  : String,
                             email     : EmailAddr,
                             name      : String,
                             newsletter: Boolean) {

  def emailWithName: EmailAddr =
    EmailAddr(s"$name <${email.value}>")
}
