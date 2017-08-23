package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import java.net.URL
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.ScalaExt.BaseUtilExtAny
import shipreq.base.util.log.{HasLogger, LogLevel}
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.server.logic.business.MailingList._
import shipreq.taskman.server.logic.business.MailingList.API._
import Http._

object MailChimp {

  final case class Props(dc        : String,
                         key       : String,
                         masterList: String,
                         logLevel  : LogLevel)

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

  final class Endpoints(urlPrefix: String) {
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

  def parseResponse[A](a: API[A]): JValue => ArticulateError \/ A =
    j => ArticulateError.attempt(a match {

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

  def interpretApiFailure[A](a: API[A])(f: ApiFailure.Total): Option[A] =
    a match {
      case _: Subscribe    if f.name == "List_AlreadySubscribed" => Some(AlreadySubscribed)
      case _: UpdateMember if f.name == "List_NotSubscribed"     => Some(NotSubscribed)
      case _                                                     => None
    }

  // ---------------------------------------------------------------------------
  // Error handling

  object ApiFailure {

    final case class Total(code: Int, name: String, msg: String) {
      def toArticulateError: ArticulateError =
        ArticulateError("MailChimp API Failure")
          .hint(s"code = $code")
          .hint(s"name = $name")
          .hint(s"msg  = $msg")
    }

    object Total {
      val errParser = ErrParser[Total](parseHttpErrorJson, _.toArticulateError)

      def parseHttpErrorJson(j: JValue): ArticulateError \/ Total =
        ArticulateError.safe(
          (j \ "status") match {
            case JString("error") =>
              val JInt(code)    = j \ "code"
              val JString(name) = j \ "name"
              val JString(msg)  = j \ "error"
              \/-(Total(code.toInt, name, msg))
            case _ => -\/(ArticulateError("Not an error."))
          }
        )
    }

    final case class Partial(code: Int, msg: String, email: Option[EmailAddr]) {
      def fullMsg: String = {
        val emailPrefix = email.fold("")(e => s"$e: ")
        s"$emailPrefix[$code] $msg"
      }
    }

    object Partial {
      def toArticulateError(h: Partial, t: List[Partial]): ArticulateError =
        ArticulateError(s"${t.size + 1} partial MailChimp API failure(s) occurred.")
          .hint(h.fullMsg, t.map(_.fullMsg): _*)

      def parse(j: JValue): ArticulateError \/ List[Partial] =
        ArticulateError.attempt(
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
            Partial(code.toInt, msg, opEmail)
          }
        )
    }
  }

  def catchPartialFailures(j: JValue): ArticulateError \/ JValue =
    ApiFailure.Partial.parse(j).flatMap {
      case Nil    => \/-(j)
      case h :: t => -\/(ApiFailure.Partial.toArticulateError(h, t))
    }
}

// =====================================================================================================================

import MailChimp._

final class MailChimp(httpClient: OkHttpClient, props: Props) extends HasLogger {
  private val (logRequest, logResponse, logResult) = httpLoggers(log.atLevel(props.logLevel))

  private val endpoints  = new Endpoints(s"https://${props.dc}.api.mailchimp.com/2.0")
  private val apikeyJson = render("apikey" -> props.key)

  private val requestBuilder =
    buildRequest(e => j => Req(e(endpoints), apikeyJson merge j))

  def run[A](api: API[A]): Fx[A] =
    send(api) flatMap recv(api) tap logResult

  @inline private def send[A](api: API[A]) =
    requestBuilder(api) |> sendRequestAndLog(httpClient, logRequest)

  @inline private def recv[A](api: API[A]) =
    recvResponseE[A, ApiFailure.Total](ApiFailure.Total.errParser, interpretApiFailure(api))(
      logResponse,
      catchPartialFailures(_) flatMap parseResponse(api))
}
