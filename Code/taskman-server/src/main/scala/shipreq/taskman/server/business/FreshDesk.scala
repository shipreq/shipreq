package shipreq.taskman.server.business

import japgolly.microlibs.config.ConfigParser
import japgolly.univeq._
import org.json4s.JsonDSL._
import org.json4s._
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

  final case class TicketOrg(group: Group, ticketType: String) {
    val json = ("group_id" -> group.id) ~ ("ticket_type" -> ticketType)
  }

  final case class Group(id: Long, name: String)

  object ConfigParsers {
    implicit def parseTicketOrg(implicit s: ConfigParser[String]): ConfigParser[UnverifiedTicketOrg] =
      s.mapOption(
        """^\s*(\S[^/]*?)\s*/\s*(\S[^/]*?)\s*$""".r.findFirstMatchIn(_).map(m => UnverifiedTicketOrg(m group 1, m group 2)),
        "Expected TicketOrg format: <groupName> / <ticketType>")
  }

  sealed abstract class Source(val value: Int) { val json = JInt(value) }
  object Source {
    case object Email    extends Source(1)
    case object Portal   extends Source(2)
    case object Phone    extends Source(3)
    case object Forum    extends Source(4)
    case object Twitter  extends Source(5)
    case object Facebook extends Source(6)
    case object Chat     extends Source(7)
  }

  sealed abstract class Status(val value: Int) { val json = JInt(value) }
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
                             source  : Source = Source.Portal) {
    def json: JValue =
      "helpdesk_ticket" -> (
        ("email" -> email) ~
        ("subject" -> subject) ~
        ("description" -> desc) ~
        ("priority" -> priorityCode(priority)) ~
        org.json ~
        ("status" -> status.json) ~
        ("source" -> source.json))
  }

  final class Endpoints(urlPrefix: String, key: String) {
    private val creds = Credential.basic(key, "X")

    val getGroups: Http[Unit, List[Group]] =
      Get(s"$urlPrefix/groups.json")
        .authWith(creds)
        .noRequest
        .jsonResponse
        .parseJsonResponse(parseGroups)

    val createTicket: Http[NewTicket, TicketId] =
      Post(s"$urlPrefix/helpdesk/tickets.json")
        .authWith(creds)
        .jsonRequest
        .jsonResponse
        .contramap[NewTicket](_.json)
        .parseJsonResponse(parseTicketResponse)
  }

  def parseGroups(j: JValue): ArticulateError \/ List[Group] =
    ArticulateError.attempt(
      (j \\ "group").children.map { g =>
        val JString(name) = g \ "name"
        val JInt(id) = g \ "id"
        Group(id.toLong, name)
      })

  def parseTicketResponse(j: JValue): ArticulateError \/ TicketId =
    ArticulateError.attempt {
      val JInt(id) = j \ "helpdesk_ticket" \ "display_id"
      TicketId(id.toLong)
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
    HttpLogger(log)

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
