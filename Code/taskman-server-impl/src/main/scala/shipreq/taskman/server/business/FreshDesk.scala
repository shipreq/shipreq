package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import com.squareup.okhttp.OkAuthenticator.Credential
import java.net.URL
import org.json4s._
import org.json4s.JsonDSL._
import scalaz.syntax.bind._
import shipreq.base.util.effect.IOE
import shipreq.base.util.effect.IoUtils.IoExt
import shipreq.base.util.ErrorOr
import shipreq.base.util.log.{LogLevel, HasLogger}
import ErrorOr.Implicits._
import Http._
import Support._
import Support.API._

object FreshDesk {

  trait Props {
    val domain: String
    val key: String
    val landingPageGroup: String
    val landingPageTicketType: String
    val logLevel: LogLevel
  }

  case class GroupId(value: Long)

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

  final case class NewTicket(email     : String,
                             subject   : String,
                             desc      : String,
                             source    : Source,
                             priority  : Priority,
                             status    : Status,
                             group     : GroupId,
                             ticketType: String) {
    def json: JValue =
      "helpdesk_ticket"-> (
        ("email"-> email) ~ ("subject"-> subject) ~ ("description"-> desc) ~
        ("source"-> source.json) ~ ("priority"-> priorityCode(priority)) ~ ("status"-> status.json) ~
        ("group_id"-> group.value) ~ ("ticket_type"-> ticketType)
      )
  }

  class Endpoints(urlPrefix: String, key: String) {
    private[this] val cred = Some(Credential.basic(key, "X"))
    private[this] def ep(m: Method, path: String) = Endpoint(new URL(s"$urlPrefix/$path.json"), m, cred)

    val createTicket = ep(Post, "helpdesk/tickets")
    val getGroups    = ep(Get,  "groups")
  }

  def createTicketReq(e: Endpoints, t: NewTicket) =
    Req(e.createTicket, t.json)

  def getGroupsReq(e: Endpoints) =
    Req(e.getGroups, JNothing)

  // ---------------------------------------------------------------------------
  // Response

  def parseGetGroupIdResponse(name: String)(j: JValue): ErrorOr[GroupId] = {
    val ids: List[BigInt] = for {
      JArray(gs) <- j \\ "group"
      g          <- gs
      JString(n) <- g \ "name" if n == name
      JInt(id)   <- g \ "id"
    } yield id
    ids match {
      case Nil => ErrorOr error s"FreshDesk group not found: $name"
      case id :: _ => ErrorOr(GroupId(id.toLong))
    }
  }

  def parseCreateTicketResponse(j: JValue): ErrorOr[TicketId] =
    ErrorOr.safe {
      val JInt(id) = j \ "helpdesk_ticket" \ "display_id"
      TicketId(id.toLong)
    }

  def parseResponse[R](a: API[R])(j: JValue): ErrorOr[R] = a match {
    case _: NotifyLandingPage => parseCreateTicketResponse(j)
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
    val groupIdIo = run(getGroupsReq(endpoints), parseGetGroupIdResponse(props.landingPageGroup))
    groupIdIo >-> (g => new FreshDesk(httpClient, props, g))
  }

  protected final def run[A](r: Req, p: JValue => ErrorOr[A]): IOE[A] =
    sendRequestL(httpClient, logRequest)(r) >==> recvResponse[A](logResponse, p) <| logResult
}

/**
 * Full interpreter for `Support.API` ops.
 */
class FreshDesk(httpClient: OkHttpClient, props: Props, val landingPageGroupId: GroupId) extends FreshDesk0(httpClient, props) {

  val buildRequest: API[_] => Req = {
    case NotifyLandingPage(email, subject, desc, priority) =>
      createTicketReq(endpoints, NewTicket(
        email, subject, desc, Source.Portal, priority, Status.Open, landingPageGroupId, props.landingPageTicketType))
  }

  def run[A](api: API[A]): IOE[A] =
    run(buildRequest(api), parseResponse(api))
}
