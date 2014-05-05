package shipreq.taskman.server.business

object Support {

  // ===================================================================================================================
  // Data

  case class TicketId(value: Long)

  sealed abstract trait Priority
  object Priority {
    case object Low    extends Priority
    case object Medium extends Priority
    case object High   extends Priority
    case object Urgent extends Priority
  }

  // ===================================================================================================================
  // API

  sealed trait API[R]
  object API {

    /** Notify support of a landing page hit. */
    case class NotifyLandingPage(email: String, subject: String, desc: String, priority: Priority) extends API[TicketId]

    case class ReportFailure(subject: String, desc: String, priority: Priority) extends API[TicketId]
  }
}
