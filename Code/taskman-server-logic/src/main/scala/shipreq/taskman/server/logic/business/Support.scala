package shipreq.taskman.server.logic.business

object Support {

  // ===================================================================================================================
  // Data

  final case class TicketId(value: Long)

  sealed trait Priority
  object Priority {
    case object Low    extends Priority
    case object Medium extends Priority
    case object High   extends Priority
    case object Urgent extends Priority
  }

  // ===================================================================================================================
  // API

  sealed trait API[A]
  object API {

    /** Notify support of a landing page hit. */
    final case class NotifyLandingPage(email   : String,
                                       subject : String,
                                       desc    : String,
                                       priority: Priority) extends API[TicketId]

    final case class ReportFailure(subject : String,
                                   desc    : String,
                                   priority: Priority) extends API[TicketId]
  }
}
