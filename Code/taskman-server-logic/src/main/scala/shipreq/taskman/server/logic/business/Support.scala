package shipreq.taskman.server.logic.business

import japgolly.univeq.UnivEq
import shipreq.taskman.api.EmailAddr

object Support {

  // ===================================================================================================================
  // Data

  final case class TicketId(value: Long)

  implicit def univEq: UnivEq[TicketId] = UnivEq.derive

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
    final case class NotifyLandingPage(email   : EmailAddr,
                                       content : Email.Content,
                                       priority: Priority) extends API[TicketId]

    final case class RecordUserFeedback(from   : EmailAddr,
                                        content: Email.Content) extends API[TicketId]

    final case class ReportFailure(subject : String,
                                   desc    : String,
                                   priority: Priority) extends API[TicketId]
  }
}
