package shipreq.taskman.server.logic.business

import shipreq.taskman.api.EmailAddr

object MailingList {

  // ===================================================================================================================
  // Data

  sealed abstract class AccountStatus(final val remoteValue: String)
  object AccountStatus {

    /** User has never had a ShipReq account. */
    case object Never extends AccountStatus("Never")

    /** User has a ShipReq account. */
    case object Active extends AccountStatus("Active")
  }

  final case class Subscription(addr      : EmailAddr,
                                name      : String,
                                newsletter: Boolean,
                                status    : AccountStatus)

  // Result types
  sealed trait ResultOps
  case object Ok extends SubscribeResult with UpdateMemberResult

  sealed trait SubscribeResult extends ResultOps
  case object AlreadySubscribed extends SubscribeResult

  sealed trait UpdateMemberResult extends ResultOps
  case object NotSubscribed extends UpdateMemberResult

  implicit def univEqSubscribeResult: UnivEq[SubscribeResult] = UnivEq.derive
  implicit def univEqUpdateMemberResult: UnivEq[UpdateMemberResult] = UnivEq.derive

  // ===================================================================================================================
  // API

  sealed trait API[A]
  object API {

    final case class Subscribe(sub: Subscription, sendConfEmail: Boolean) extends API[SubscribeResult]

    final case class UpdateMember(sub: Subscription) extends API[UpdateMemberResult]

    final case class BatchSubscribe(subs: NonEmptyVector[Subscription]) extends API[Unit]
  }
}
