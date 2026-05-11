package shipreq.taskman.server.business

import cats.~>
import japgolly.clearconfig._
import shipreq.base.util.FxModule._
import shipreq.taskman.server.business.SupportViaMail._
import shipreq.taskman.server.logic.business.BusinessOp.SendEmail
import shipreq.taskman.server.logic.business.{Email, Support}

object SupportViaMail {
  final case class Props(envelope: Email.Envelope)

  def config: ConfigDef[Props] =
    JavaMail.ConfigValueParsers.configEnvelope.map(Props(_))
      .withPrefix("supportDesk.mail.")

  val NullTicketId = Support.TicketId(0)
}

/** Interpreter of `Support.API` that simply sends emails rather than integrating with a support-desk system.
  */
final class SupportViaMail(props: Props, sendMail: SendEmail => Fx[Unit]) extends (Support.API ~> Fx) {

  override def apply[A](api: Support.API[A]): Fx[A] = {

    def send(content: Email.Content): Fx[Support.TicketId] = {
      val op = SendEmail(props.envelope, content)
      sendMail(op).map(_ => NullTicketId)
    }

    api match {
      case Support.API.NotifyLandingPage(email, content, pri) =>
        send(Email.Content(
          s"${content.subject} [pri=$pri]",
          s"email: $email\n\n${content.body}"))

      case Support.API.RecordUserFeedback(from, content) =>
        send(Email.Content(
          content.subject,
          s"from: $from\n\n${content.body}"))

      case Support.API.ReportFailure(content, pri) =>
        send(Email.Content(
          s"${content.subject} [pri=$pri]",
          content.body))
    }
  }
}
