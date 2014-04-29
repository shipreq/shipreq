package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.nio.charset.Charset
import org.apache.http.entity._
import org.apache.http.HttpEntity
import org.apache.http.util.EntityUtils
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._
import scalaz.{NonEmptyList, -\/, \/-}
import scalaz.effect.IO
import shipreq.base.util.effect.{IOE, IOExt}
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.ScalaExt.AnyExt
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Types._
import ErrorOr.Implicits._
import MailingList._
import MailingList.API._

object MailChimp extends HasLogger {

  trait Props {
    val dc: String
    val key: String
    val masterList: String
  }

  final case class Req(url: URL, bodyJ: JValue) {
    val bodyS: String = compact(bodyJ)
  }

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
      val emailPrefix = email.map(e => s"$e: ").getOrElse("")
      s"$emailPrefix[$code] $msg"
    }
  }

  val defaultCharset = Charset.forName("UTF-8")

  val contentTypeJson = s"application/json;charset=${defaultCharset.name}"

  def parseJson(str: String): ErrorOr[JValue] =
    ErrorOr.safe(parse(str))

  // ---------------------------------------------------------------------------
  // Request

  def logRequest(req: Req): IO[Unit] =
    IO(log.debug z s"HTTP request: ${req.url} << ${req.bodyS}")

  def openConn(httpClient: OkHttpClient, url: URL): IOE[HttpURLConnection] = IOE {
    val conn = httpClient.open(url)
    conn.setRequestProperty("Content-Type", contentTypeJson)
    conn.setRequestMethod("POST")
    conn
  }

  def sendRequest(httpClient: OkHttpClient)(req: Req): IOE[HttpURLConnection] = {
    val body = req.bodyS.getBytes(defaultCharset)
    openConn(httpClient, req.url) >==>^ writeRequestBody(body) |<<| logRequest(req)
  }

  def writeRequestBody(body: Array[Byte])(conn: HttpURLConnection): IOE[Unit] =
    IO(ErrorOr.withResource(conn.getOutputStream)(_.close)(_ write body))

  // ---------------------------------------------------------------------------
  // Response

  def logResponse(resp: String): IO[Unit] =
    IO(log.debug z s"HTTP response: $resp")

  def recv(f: HttpURLConnection => InputStream): HttpURLConnection => IOE[String] = conn =>
    IO(ErrorOr.withResource(f(conn))(_.close){ in =>
      val entity: HttpEntity = new InputStreamEntity(in)
      val charset = Option(ContentType get entity).map(_.getCharset) getOrElse defaultCharset
      val bytes = EntityUtils.toByteArray(entity)
      new String(bytes, charset)
    })

  val recvResponseInput = recv(_.getInputStream)

  val recvResponseError = recv(_.getErrorStream)

  def getResponseCode(conn: HttpURLConnection): IOE[Int] =
    IOE(conn.getResponseCode)

  def recvResponse[R](ok: IOE[String] => IOE[R], ko: TotalApiFailure => Option[R])(conn: HttpURLConnection): IOE[R] =
    getResponseCode(conn) >==> (code =>
      if (code == HttpURLConnection.HTTP_OK)
        recvResponseInput(conn) <<| logResponse |> ok
      else
        handleErrorResponse(conn, ko)
      )

  def processResponse[R](api: API[R]): String => ErrorOr[R] =
    parseJson(_) >=> catchPartialFailures >=> extractResult(api)

  // ---------------------------------------------------------------------------
  // Error handling

  def handleErrorResponse[R](conn: HttpURLConnection, h: TotalApiFailure => Option[R]): IOE[R] = {
    val parseApiFailureOrGeneric: String => IOE[R] = resp =>
      parseErrorResponse(resp) match {
        case \/-(f) =>
          h(f) match {
            case None    => IO(ApiFailure.Total(f).toErrorOr)
            case Some(r) => IOE(r)
          }
        case -\/(_) =>
          genericHttpError(conn, resp).map(_.toErrorOr)
      }
    recvResponseError(conn) <<| logResponse >==> parseApiFailureOrGeneric
  }

  def genericHttpError(c: HttpURLConnection, errResp: String): IO[Error] =
    IO(Error(s"Unexpected HTTP response: ${c.getResponseCode} ${c.getResponseMessage}. Response: $errResp"))

  def parseErrorResponse(resp: String): ErrorOr[TotalApiFailure] =
    parseJson(resp) >==> parseErrorResponseJson

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
          email.tag[IsEmailAddr]
        }
        PartialApiFailure(code.toInt, msg, opEmail)
      }
    )

  def parseErrorResponseJson(j: JValue): ErrorOr[TotalApiFailure] =
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

  // ---------------------------------------------------------------------------
  // API

  def logResult(r: Any): IO[Unit] =
    IO(log.debug z s"MailChimp result: $r")

  val i0 = JInt(0)
  val i1 = JInt(1)
  @inline def boolAsInt(b: Boolean) = if (b) i1 else i0

  def subscribeOptions(sendConfEmail: Boolean, updExisting: Boolean) =
    ("double_optin" -> sendConfEmail) ~ ("update_existing" -> updExisting) ~ ("send_welcome" -> false)

  val batchSubscribeStatic = subscribeOptions(false, true)

  def buildReqSubscription(s: Subscription) =
    ("email" -> ("email" -> s.addr)) ~ ("merge_vars" ->
      ("NAME" -> s.name) ~ ("NEWSLETTER" -> boolAsInt(s.newsletter)) ~ ("ACCT" -> s.status.remoteValue))

  def extractResult[R](a: API[R]): JValue => ErrorOr[R] =
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

  def extractResultFromError[R](a: API[R])(f: TotalApiFailure): Option[R] = a match {
    case _: Subscribe    if f.name == "List_AlreadySubscribed" => Some(AlreadySubscribed)
    case _: UpdateMember if f.name == "List_NotSubscribed"     => Some(NotSubscribed)
    case _ => None
  }
}

import MailChimp._

final class MailChimp(httpClient: OkHttpClient, props: Props) extends HasLogger {

  private val urlPrefix = s"https://${props.dc}.api.mailchimp.com/2.0"
  private val apikeyJson = render("apikey" -> props.key)

  private object urls {
    private[urls] def url(path: String) = new URL(s"$urlPrefix/$path.json")
    object lists {
      val list           = url("lists/list")
      val batchSubscribe = url("lists/batch-subscribe")
      val subscribe      = url("lists/subscribe")
      val updateMember   = url("lists/update-member")
    }
  }

  private def req(url: URL, reqJson: JValue): Req =
    new Req(url, apikeyJson merge reqJson)

  def run[A](api: API[A]): IOE[A] =
    (buildRequest(api) |> sendRequest(httpClient)) >==>
      recvResponse(_ >=> processResponse(api), extractResultFromError(api)) <| logResult

  val buildRequest: API[_] => Req = {

    case GetListId(name) =>
      req(urls.lists.list,
        "filters" -> ("list_name" -> name) ~ ("exact" -> true))

    case BatchSubscribe(ListId(listId), ss) =>
      req(urls.lists.batchSubscribe,
        ("id" -> listId) ~ ("batch" -> ss.list.map(buildReqSubscription)) ~ batchSubscribeStatic)

    case Subscribe(ListId(listId), s, sendConfEmail) =>
      req(urls.lists.subscribe,
        ("id" -> listId) ~ buildReqSubscription(s) ~ subscribeOptions(sendConfEmail, false))

    case UpdateMember(ListId(listId), s) =>
      req(urls.lists.updateMember,
        ("id" -> listId) ~ buildReqSubscription(s))
  }
}
