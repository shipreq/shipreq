package shipreq.taskman.server.business

import japgolly.clearconfig._
import japgolly.univeq._
import io.circe._
import io.circe.syntax._
import scalaz.{\/, ~>}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.logic.business.Support
import shipreq.taskman.server.logic.business.Support.API._
import shipreq.taskman.server.logic.business.Support._
import Http._
import FreshDesk._

object FreshDesk {

  final case class Props(domain      : String,
                         key         : String,
                         taskmanEmail: String,
                         landingPage : UnverifiedTicketOrg,
                         failure     : UnverifiedTicketOrg)

  final case class VerifiedProps(landingPage: TicketOrg, failure: TicketOrg)

  final case class UnverifiedTicketOrg(groupName: String, ticketType: String)

  final case class TicketOrg(group: Group, ticketType: String)

  final case class Group(id: Long, name: String)

  object ConfigValueParsers {
    implicit def parseTicketOrg(implicit s: ConfigValueParser[String]): ConfigValueParser[UnverifiedTicketOrg] =
      s.mapOption(
        """^\s*(\S[^/]*?)\s*/\s*(\S[^/]*?)\s*$""".r.findFirstMatchIn(_).map(m => UnverifiedTicketOrg(m group 1, m group 2)),
        "Expected TicketOrg format: <groupName> / <ticketType>")
  }

  sealed abstract class Source(val value: Int)
  object Source {
    case object Email    extends Source(1)
    case object Portal   extends Source(2)
    case object Phone    extends Source(3)
    case object Forum    extends Source(4)
    case object Twitter  extends Source(5)
    case object Facebook extends Source(6)
    case object Chat     extends Source(7)
  }

  sealed abstract class Status(val value: Int)
  object Status {
    case object Open     extends Status(2)
    case object Pending  extends Status(3)
    case object Resolved extends Status(4)
    case object Closed   extends Status(5)
  }

  val priorityCode: Priority => Int = {
    case Priority.Low    => 1
    case Priority.Medium => 2
    case Priority.High   => 3
    case Priority.Urgent => 4
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Protocol

  final case class NewTicket(email   : String,
                             subject : String,
                             desc    : String,
                             priority: Priority,
                             org     : TicketOrg,
                             status  : Status = Status.Open,
                             source  : Source = Source.Portal)

  implicit val encoderNewTicket: Encoder[NewTicket] =
    Encoder.instance(t =>
      Json.obj(
        "helpdesk_ticket" -> Json.obj(
          "email"       -> t.email.asJson,
          "subject"     -> t.subject.asJson,
          "description" -> t.desc.asJson,
          "priority"    -> priorityCode(t.priority).asJson,
          "group_id"    -> t.org.group.id.asJson,
          "ticket_type" -> t.org.ticketType.asJson,
          "status"      -> t.status.value.asJson,
          "source"      -> t.source.value.asJson,
        )
      )
    )

  /** Sample:
    * {{{
    *     {
    *       "group": {
    *         "assign_time": null,
    *         "business_calendar_id": null,
    *         "created_at": "2014-04-30T08:05:51+09:00",
    *         "description": "Product Management group",
    *         "escalate_to": null,
    *         "id": 1000123401,
    *         "name": "Product Management",
    *         "ticket_assign_type": 0,
    *         "updated_at": "2014-04-30T08:05:51+09:00",
    *         "agents": []
    *       }
    *     }
    * }}}
    */
  implicit val decoderGroup: Decoder[Group] =
    Decoder.instance { c =>
      val g = c.downField("group")
      for {
        id   <- g.get[Long]("id")
        name <- g.get[String]("name")
      } yield Group(id, name)
    }

  val decoderTicketResponse: Decoder[TicketId] =
    Decoder.instance { c =>
      c.downField("helpdesk_ticket").downField("display_id").as[Long].map(TicketId.apply)
    }

  final class Endpoints(urlPrefix: String, key: String) {
    private val creds = Credential.basic(key, "X")

    val getGroups: Http[Unit, List[Group]] =
      Get(s"$urlPrefix/groups.json")
        .authWith(creds)
        .noRequest
        .responseAsJson[List[Group]]

    /*
     * curl -v https://${DOMAIN}.freshdesk.com/helpdesk/tickets.json -X POST -d '{"helpdesk_ticket":{"email":"David Barri <japgolly@gmail.com>","subject":"Landing Page Contact","description":"MsgId = 1002\nContact time = 2019-12-03T05:31:24.563353Z\nName = David Barri\nEmail = japgolly@gmail.com\nNewsletter = true\nMessage = \n\nyo","priority":2,"group_id":1000125446,"ticket_type":"Lead","status":2,"source":2}}' -u ${APIKEY}:X -H "Content-Type: application/json"
     */
    val createTicket: Http[NewTicket, TicketId] =
      Post(s"$urlPrefix/helpdesk/tickets.json")
        .authWith(creds)
        .requestAsJson[NewTicket]
        .responseAsJson(decoderTicketResponse)
  }

  def getGroup(groups: List[Group], n: String): ArticulateError \/ Group =
    ArticulateError.fromOption(groups.find(_.name ==* n), s"FreshDesk group not found: $n")

  def verifyTicketOrg(groups: List[Group], u: UnverifiedTicketOrg): ArticulateError \/ TicketOrg =
    getGroup(groups, u.groupName).map(TicketOrg(_, u.ticketType))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/**
 * Can connect to FreshDesk but lacks data required to interpret `Support.API` ops.
 */
sealed class FreshDesk0(props: Props)(implicit httpClient: HttpClient) extends HasLogger {

  protected final implicit val httpLogger: HttpLogger =
    HttpLogger(logger)

  protected final val endpoints: Endpoints =
    new Endpoints(s"https://${props.domain}.freshdesk.com", props.key)

  /**
   * Turns this into a full instance that can interpret `Support.API` ops.
   */
  def upgrade: Fx[FreshDesk] =
    for {
      groups      <- endpoints.getGroups.run(())
      failure     <- Fx lift verifyTicketOrg(groups, props.failure)
      landingPage <- Fx lift verifyTicketOrg(groups, props.landingPage)
    } yield {
      val verifiedProps = VerifiedProps(failure = failure, landingPage = landingPage)
      new FreshDesk(props, verifiedProps)
    }
}

/**
 * Full interpreter for `Support.API` ops.
 */
final class FreshDesk(props: Props, val verifiedProps: VerifiedProps)
                     (implicit httpClient: HttpClient) extends FreshDesk0(props) with (Support.API ~> Fx) {

  private def createTicket(t: NewTicket): Fx[TicketId] =
    endpoints.createTicket.run(t)

  override def apply[A](api: API[A]): Fx[A] =
    api match {
      case NotifyLandingPage(email, subject, desc, priority) =>
        createTicket(NewTicket(email, subject, desc, priority, verifiedProps.landingPage))

      case ReportFailure(subject, desc, priority) =>
        createTicket(NewTicket(props.taskmanEmail, subject, desc, priority, verifiedProps.failure))
    }
}
