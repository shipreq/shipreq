package shipreq.taskman.server.logic.business

import japgolly.univeq.UnivEq
import shipreq.taskman.api.EmailAddr
import scalaz.old.NonEmptyList

object MailingList {

  // ===================================================================================================================
  // Data

  final case class ListId(value: String)

  implicit def univEqListId: UnivEq[ListId] = UnivEq.derive

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

    /** Looks up the ID of a mailing list by name. */
    final case class GetListId(name: String) extends API[Option[ListId]]

    final case class Subscribe(listId: ListId, sub: Subscription, sendConfEmail: Boolean) extends API[SubscribeResult]

    final case class UpdateMember(listId: ListId, sub: Subscription) extends API[UpdateMemberResult]

    final case class BatchSubscribe(listId: ListId, subs: NonEmptyList[Subscription]) extends API[Unit]
  }
}
