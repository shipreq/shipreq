package shipreq.taskman.server.business

import com.squareup.okhttp._
import japgolly.microlibs.config.ConfigParser
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.net.URL
import org.json4s._
import org.json4s.JsonDSL._
import scalaz.\/
import scalaz.std.list.listInstance
import scalaz.syntax.traverse._
import scalaz.syntax.bind._
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.{HasLogger, LogLevel}
import shipreq.taskman.server.logic.business.Support._
import shipreq.taskman.server.logic.business.Support.API._
import Http._

object FreshDesk {

  final case class Props(domain      : String,
                         key         : String,
                         taskmanEmail: String,
                         landingPage : UnverifiedTicketOrg,
                         failure     : UnverifiedTicketOrg,
                         logLevel    : LogLevel)

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

  // ---------------------------------------------------------------------------
  // Request

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
    private val cred = Some(Credential.basic(key, "X"))
    private def ep(m: Method, path: String) = Endpoint(new URL(s"$urlPrefix/$path.json"), m, cred)

    val createTicket = ep(Post, "helpdesk/tickets")
    val getGroups    = ep(Get,  "groups")
  }

  def getGroupsReq(e: Endpoints) =
    Req(e.getGroups, JNothing)

  // ---------------------------------------------------------------------------
  // Response

  def parseRespGetGroups(names: List[String])(j: JValue): ArticulateError \/ List[Group] =
    names.traverse[ArticulateError \/ ?, Group](parseRespGetGroup(_)(j))

  def parseRespGetGroup(name: String)(j: JValue): ArticulateError \/ Group = {
    val gs = (j \\ "group")
      .children
      .iterator
      .map(jj => (jj \ "name", jj \ "id"))
      .map {
        case (JString(`name`), JInt(id)) => Some(Group(id.toLong, name))
        case _                           => None
      }.filterDefined
    ArticulateError.fromOption(gs.nextOption(), s"FreshDesk group not found: $name")
  }

  def parseCreateTicketResponse(j: JValue): ArticulateError \/ TicketId =
    ArticulateError.attempt {
      val JInt(id) = j \ "helpdesk_ticket" \ "display_id"
      TicketId(id.toLong)
    }

  def parseResponse[A](a: API[A])(j: JValue): ArticulateError \/ A =
    a match {
      case _: ReportFailure
         | _: NotifyLandingPage => parseCreateTicketResponse(j)
    }

}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████


import FreshDesk._

/**
 * Can connect to FreshDesk but lacks data required to interpret `Support.API` ops.
 */
sealed class FreshDesk0(httpClient: OkHttpClient, props: Props) extends HasLogger {
  protected final val (logRequest, logResponse, logResult) = httpLoggers(log.atLevel(props.logLevel))

  protected final val endpoints =
    new Endpoints(s"https://${props.domain}.freshdesk.com", props.key)

  /**
   * Turns this into a full instance that can interpret `Support.API` ops.
   */
  def upgrade: Fx[FreshDesk] = {
    def parse(j: JValue): ArticulateError \/ FreshDesk = {
      val unverifiedOrgs = List(props.failure, props.landingPage)
      parseRespGetGroups(unverifiedOrgs.map(_.groupName))(j).map { groups =>
        val ticketOrgs = groups.zip(unverifiedOrgs).map(x => TicketOrg(x._1, x._2.ticketType))
        val failureOrg :: landingPageOrg :: Nil = ticketOrgs
        val verifiedProps = VerifiedProps(failure = failureOrg, landingPage = landingPageOrg)
        new FreshDesk(httpClient, props, verifiedProps)
      }
    }
    run(getGroupsReq(endpoints), parse)
  }

  protected final def run[A](r: Req, p: JValue => ArticulateError \/ A): Fx[A] =
    sendRequestAndLog(httpClient, logRequest)(r)
      .flatMap(recvResponse[A](logResponse, p))
      .tap(logResult)
}

/**
 * Full interpreter for `Support.API` ops.
 */
final class FreshDesk(httpClient: OkHttpClient, props: Props, val verifiedProps: VerifiedProps) extends FreshDesk0(httpClient, props) {

  private def createTicketReq(t: NewTicket) =
    Req(endpoints.createTicket, t.json)

  val buildRequest: API[_] => Req = {

    case NotifyLandingPage(email, subject, desc, priority) =>
      createTicketReq(NewTicket(email, subject, desc, priority, verifiedProps.landingPage))

    case ReportFailure(subject, desc, priority) =>
      createTicketReq(NewTicket(props.taskmanEmail, subject, desc, priority, verifiedProps.failure))
  }

  def run[A](api: API[A]): Fx[A] =
    run(buildRequest(api), parseResponse(api))
}
