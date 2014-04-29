package shipreq.taskman.server.business

import shipreq.taskman.api.Types._
import scalaz.NonEmptyList

object MailingList {

  // ===================================================================================================================
  // Data

  case class ListId(value: String)

  sealed abstract class AccountStatus(val remoteValue: String)
  object AccountStatus {
    
    /** User has never had a ShipReq account. */
    case object Never extends AccountStatus("Never")

    /** User has a ShipReq account. */
    case object Active extends AccountStatus("Active")
  }

  case class Subscription(addr: EmailAddr, name: String, newsletter: Boolean, status: AccountStatus)

  // Failures
  sealed trait SubscribeFail
  sealed trait UpdateMemberFail
  case object AlreadySubscribed extends SubscribeFail
  case object NotSubscribed extends UpdateMemberFail

  // ===================================================================================================================
  // API

  sealed trait API[R]
  object API {

    /** Looks up the ID of a mailing list by name. */
    case class GetListId(name: String) extends API[Option[ListId]]

    case class Subscribe(l: ListId, s: Subscription, sendConfEmail: Boolean) extends API[Option[SubscribeFail]]

    case class UpdateMember(l: ListId, s: Subscription) extends API[Option[UpdateMemberFail]]

    case class BatchSubscribe(l: ListId, ss: NonEmptyList[Subscription]) extends API[Unit]
  }
}
