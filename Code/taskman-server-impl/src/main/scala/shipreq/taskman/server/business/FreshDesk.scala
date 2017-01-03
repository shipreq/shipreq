package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import com.squareup.okhttp.OkAuthenticator.Credential
import japgolly.microlibs.config.ConfigParser
import java.net.URL
import org.json4s._
import org.json4s.JsonDSL._
import scalaz.std.list.listInstance
import scalaz.syntax.traverse._
import scalaz.syntax.bind._
import shipreq.base.util.effect.IOE
import shipreq.base.util.effect.IoUtils.IoExt
import shipreq.base.util.ErrorOr
import shipreq.base.util.log.{HasLogger, LogLevel}
import ErrorOr.Implicits._
import Http._
import Support._
import Support.API._

object FreshDesk {

  final case class Props(
    domain: String,
    key: String,
    taskmanEmail: String,
    landingPage: TicketOrg,
    failure: TicketOrg,
    logLevel: LogLevel)

  final case class PropsI(landingPage: TicketOrgI, failure: TicketOrgI)

  final case class TicketOrg(groupName: String, ticketType: String)

  final case class TicketOrgI(group: Group, ticketType: String) {
    val json = ("group_id"-> group.id) ~ ("ticket_type"-> ticketType)
  }

  object ConfigParsers {
    implicit def parseTicketOrg(implicit s: ConfigParser[String]): ConfigParser[TicketOrg] =
      s.mapOption(
        """^\s*(\S[^/]*?)\s*/\s*(\S[^/]*?)\s*$""".r.findFirstMatchIn(_).map(m => TicketOrg(m group 1, m group 2)),
        "Expected TicketOrg format: <groupName> / <ticketType>")
  }

  case class Group(id: Long, name: String)

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

  def priorityCode(p: Priority): Int = p match {
    case Priority.Low    => 1
    case Priority.Medium => 2
    case Priority.High   => 3
    case Priority.Urgent => 4
  }

  // ---------------------------------------------------------------------------
  // Request

  final case class NewTicket(email: String, subject: String, desc: String, priority: Priority, org: TicketOrgI,
                             status: Status = Status.Open,
                             source: Source = Source.Portal) {
    def json: JValue =
      "helpdesk_ticket"-> (
        ("email"-> email) ~ ("subject"-> subject) ~ ("description"-> desc) ~
        ("priority"-> priorityCode(priority)) ~ org.json ~ ("status"-> status.json) ~ ("source"-> source.json)
      )
  }

  class Endpoints(urlPrefix: String, key: String) {
    private[this] val cred = Some(Credential.basic(key, "X"))
    private[this] def ep(m: Method, path: String) = Endpoint(new URL(s"$urlPrefix/$path.json"), m, cred)

    val createTicket = ep(Post, "helpdesk/tickets")
    val getGroups    = ep(Get,  "groups")
  }

  def getGroupsReq(e: Endpoints) =
    Req(e.getGroups, JNothing)

  // ---------------------------------------------------------------------------
  // Response

  def parseRespGetGroups(names: List[String])(j: JValue): ErrorOr[List[Group]] =
    names.traverse[ErrorOr, Group](parseRespGetGroup(_)(j))

  def parseRespGetGroup(name: String)(j: JValue): ErrorOr[Group] = {
    val gs = (j \\ "group").children.toStream
      .map(jj => (jj \ "name", jj \ "id"))
      .map {
        case (JString(`name`), JInt(id)) => Some(Group(id.toLong, name))
        case _ => None
      }.filter(_.isDefined).map(_.get)
    ErrorOr.fromOptionS(gs.headOption, s"FreshDesk group not found: $name")
  }

  def parseCreateTicketResponse(j: JValue): ErrorOr[TicketId] =
    ErrorOr.safe {
      val JInt(id) = j \ "helpdesk_ticket" \ "display_id"
      TicketId(id.toLong)
    }

  def parseResponse[R](a: API[R])(j: JValue): ErrorOr[R] = a match {
    case _: ReportFailure
       | _: NotifyLandingPage => parseCreateTicketResponse(j)
  }

}

// =====================================================================================================================

import FreshDesk._

/**
 * Can connect to FreshDesk but lacks data required to interpret `Support.API` ops.
 */
class FreshDesk0(httpClient: OkHttpClient, props: Props) extends HasLogger {
  protected final val (logRequest, logResponse, logResult) = httpLoggers(log.atLevel(props.logLevel))

  protected final val endpoints =
    new Endpoints(s"https://${props.domain}.freshdesk.com", props.key)

  /**
   * Turns this into a full instance that can interpret `Support.API` ops.
   */
  def upgrade: IOE[FreshDesk] = {
    val orgs = List(props.failure, props.landingPage)

    def a = parseRespGetGroups(orgs.map(_.groupName)) _
    def b = (_: List[Group]).zip(orgs).map(x => TicketOrgI(x._1, x._2.ticketType))
    def c(l: List[TicketOrgI]) = {
      val f :: lp :: Nil = l
      PropsI(landingPage = lp, failure = f)
    }
    def d(i: PropsI) = new FreshDesk(httpClient, props, i)

    val parse: JValue => ErrorOr[FreshDesk] = a(_) >-> (b andThen c andThen d)
    run(getGroupsReq(endpoints), parse)
  }

  protected final def run[A](r: Req, p: JValue => ErrorOr[A]): IOE[A] =
    sendRequestL(httpClient, logRequest)(r) >==> recvResponse[A](logResponse, p) <| logResult
}

/**
 * Full interpreter for `Support.API` ops.
 */
class FreshDesk(httpClient: OkHttpClient, props: Props, val propsI: PropsI) extends FreshDesk0(httpClient, props) {

  @inline private def createTicketReq(t: NewTicket) = Req(endpoints.createTicket, t.json)

  val buildRequest: API[_] => Req = {

    case NotifyLandingPage(email, subject, desc, priority) =>
      createTicketReq(NewTicket(email, subject, desc, priority, propsI.landingPage))

    case ReportFailure(subject, desc, priority) =>
      createTicketReq(NewTicket(props.taskmanEmail, subject, desc, priority, propsI.failure))
  }

  def run[A](api: API[A]): IOE[A] =
    run(buildRequest(api), parseResponse(api))
}
