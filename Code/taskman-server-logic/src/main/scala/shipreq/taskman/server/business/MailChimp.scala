package shipreq.taskman.server.business

object MailChimp {

  // ===================================================================================================================
  // Data

  case class ListId(value: String)

  // ===================================================================================================================
  // API

  sealed trait API[R]
  object API {

    case class GetListId(name: String) extends API[Option[ListId]]
  }
}
