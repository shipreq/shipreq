package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import java.net.URL
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import scalaz.old.NonEmptyList
import scalaz.syntax.bind._
import shipreq.base.util.FxModule._
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.effect._
import shipreq.base.util.ScalaExt.BaseUtilExtAny
import shipreq.base.util.log.{LogLevel, HasLogger}
import shipreq.taskman.api.EmailAddr
import ErrorOr.Implicits._
import Http._
import MailingList._
import MailingList.API._

object MailChimp {

  final case class Props(
    dc: String,
    key: String,
    masterList: String,
    logLevel: LogLevel)

  // ---------------------------------------------------------------------------
  // Request

  val i0 = JInt(0)
  val i1 = JInt(1)
  @inline def boolAsInt(b: Boolean) = if (b) i1 else i0

  def subscribeOptions(sendConfEmail: Boolean, updExisting: Boolean) =
    ("double_optin" -> sendConfEmail) ~ ("update_existing" -> updExisting) ~ ("send_welcome" -> false)

  val batchSubscribeStatic = subscribeOptions(false, true)

  def buildReqSubscription(s: Subscription) =
    ("email" -> ("email" -> s.addr.value)) ~ ("merge_vars" ->
      ("NAME" -> s.name) ~ ("NEWSLETTER" -> boolAsInt(s.newsletter)) ~ ("ACCT" -> s.status.remoteValue))

  class Endpoints(urlPrefix: String) {
    private[this] def url(path: String) = Endpoint(new URL(s"$urlPrefix/$path.json"), Post, None)
    object lists {
      val list           = url("lists/list")
      val batchSubscribe = url("lists/batch-subscribe")
      val subscribe      = url("lists/subscribe")
      val updateMember   = url("lists/update-member")
    }
  }

  def buildRequest(req: (Endpoints => Endpoint) => JValue => Req): API[_] => Req = {

    case GetListId(name) =>
      req(_.lists.list)(
        "filters" -> ("list_name" -> name) ~ ("exact" -> true))

    case BatchSubscribe(ListId(listId), ss) =>
      req(_.lists.batchSubscribe)(
        ("id" -> listId) ~ ("batch" -> ss.list.map(buildReqSubscription)) ~ batchSubscribeStatic)

    case Subscribe(ListId(listId), s, sendConfEmail) =>
      req(_.lists.subscribe)(
        ("id" -> listId) ~ buildReqSubscription(s) ~ subscribeOptions(sendConfEmail, false))

    case UpdateMember(ListId(listId), s) =>
      req(_.lists.updateMember)(
        ("id" -> listId) ~ buildReqSubscription(s))
  }

  // ---------------------------------------------------------------------------
  // Response

  def parseResponse[R](a: API[R]): JValue => ErrorOr[R] =
    j => ErrorOr.safe(a match {

      case _: GetListId =>
        val JInt(total) = j \ "total"
        total.toInt match {
          case 0 => None
          case 1 =>
            val JString(id) = (j \ "data")(0) \ "id"
            Some(ListId(id))
        }

      case _: BatchSubscribe => ()
      case _: Subscribe      => Ok
      case _: UpdateMember   => Ok
    })

  def parseResponseE[R](a: API[R])(f: TotalApiFailure): Option[R] = a match {
    case _: Subscribe    if f.name == "List_AlreadySubscribed" => Some(AlreadySubscribed)
    case _: UpdateMember if f.name == "List_NotSubscribed"     => Some(NotSubscribed)
    case _ => None
  }

  // ---------------------------------------------------------------------------
  // Error handling

  object ApiFailure {
    object Total {
      def apply(f: TotalApiFailure): Error = Error(f.fullMsg).withSupp(f)
      def unapply(e: Error): Option[TotalApiFailure] = e.trySupp { case f: TotalApiFailure => f }
    }

    object Partial {
      def apply(h: PartialApiFailure, t: List[PartialApiFailure]): Error =
        Error(s"${t.size + 1} partial API failure(s) occurred.").withSupp(NonEmptyList.nel(h, t))

      def unapply(e: Error): Option[NonEmptyList[PartialApiFailure]] = e.trySupp {
        case r@ NonEmptyList(_: PartialApiFailure, _) => r.asInstanceOf[NonEmptyList[PartialApiFailure]]
      }
    }
  }

  final case class TotalApiFailure(code: Int, name: String, msg: String) {
    def fullMsg = s"[$code] $name: $msg"
    def shortMsg = s"[$code] $name"
  }

  final case class PartialApiFailure(code: Int, msg: String, email: Option[EmailAddr]) {
    def fullMsg = {
      val emailPrefix = email.fold("")(e => s"$e: ")
      s"$emailPrefix[$code] $msg"
    }
  }

  def parseHttpErrorJson(j: JValue): ErrorOr[TotalApiFailure] =
    ErrorOr.catchException (
      (j \ "status") match {
        case JString("error") =>
          val JInt(code)    = j \ "code"
          val JString(name) = j \ "name"
          val JString(msg)  = j \ "error"
          ErrorOr(TotalApiFailure(code.toInt, name, msg))
        case _ => ErrorOr error "Not an error."
      }
    )

  val totalErrParser = ErrParser[TotalApiFailure](parseHttpErrorJson, ApiFailure.Total.apply)

  val catchPartialFailures: JValue => ErrorOr[JValue] =
    j => parsePartialFailures(j) >=> {
      case Nil    => ErrorOr(j)
      case h :: t => ApiFailure.Partial(h, t).toErrorOr
    }

  def parsePartialFailures: JValue => ErrorOr[List[PartialApiFailure]] =
    j => ErrorOr.safe(
      for {
        JArray(errors) <- j \ "errors"
        e <- errors
      } yield {
        val JInt(code) = e \ "code"
        val JString(msg) = e \ "error"
        val opEmail = (e \ "email").toOption.map { i =>
          val JString(email) = i \ "email"
          EmailAddr(email)
        }
        PartialApiFailure(code.toInt, msg, opEmail)
      }
    )
}

// =====================================================================================================================

import MailChimp._

final class MailChimp(httpClient: OkHttpClient, props: Props) extends HasLogger {
  private val (logRequest, logResponse, logResult) = httpLoggers(log.atLevel(props.logLevel))

  private val endpoints  = new Endpoints(s"https://${props.dc}.api.mailchimp.com/2.0")
  private val apikeyJson = render("apikey" -> props.key)

  private val requestBuilder =
    buildRequest(e => j => new Req(e(endpoints), apikeyJson merge j))

  def run[A](api: API[A]): FxE[A] =
    send(api) >==> recv(api) tap logResult

  @inline private def send[A](api: API[A]) =
    requestBuilder(api) |> sendRequestL(httpClient, logRequest)

  @inline private def recv[A](api: API[A]) =
    recvResponseE[A, TotalApiFailure](totalErrParser, parseResponseE(api))(logResponse,
      catchPartialFailures(_) >=> parseResponse(api))
}
