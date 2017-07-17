package shipreq.taskman.server.business

import com.squareup.okhttp.OkHttpClient
import com.squareup.okhttp.OkAuthenticator.Credential
import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.nio.charset.Charset
import org.apache.http.entity._
import org.apache.http.HttpEntity
import org.apache.http.util.EntityUtils
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scalaz.{\/, -\/, \/-}
import shipreq.base.util.FxModule._
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.effect._
import shipreq.base.util.log.Logger
import shipreq.base.util.ScalaExt.BaseUtilExtAny
import ErrorOr.Implicits._

object Http {

  sealed abstract class Method(val value: String)
  case object Get extends Method("GET")
  case object Put extends Method("PUT")
  case object Post extends Method("POST")
  case object Delete extends Method("DELETE")

  final case class Endpoint(url: URL, method: Method, credential: Option[Credential])

  final case class Req(e: Endpoint, bodyJ: JValue) {
    val bodyS = compact(bodyJ)
    def bodyB = bodyS.getBytes(defaultCharset)
  }

  def httpLoggers(log: Logger#AtLevel) = {
    val p = log.printer[Fx]
    def s(prefix: String, str: String) = if (str.isEmpty) "" else prefix + str
    val logRequest  = (r: Req)    => p(s"HTTP request: ${r.e.method.value} ${r.e.url}${s(" ~ ", r.bodyS)}")
    val logResponse = (r: String) => p(s"HTTP response: $r")
    val logResult   = (r: Any)    => p(s"Op result: $r")
    (logRequest, logResponse, logResult)
  }

  val defaultCharset = Charset.forName("UTF-8")

  val contentTypeJson = s"application/json;charset=${defaultCharset.name}"

  def parseIntoJson(str: String): ErrorOr[JValue] = ErrorOr.safe(parse(str))
  def parseIntoJsonI(str: String): FxE[JValue]    = Fx(parseIntoJson(str))

  // ---------------------------------------------------------------------------
  // Request

  def sendRequest(httpClient: OkHttpClient)(req: Req): FxE[HttpURLConnection] = {
    val io = openConn(httpClient, req.e)
    if (req.bodyS.isEmpty)
      io
    else
      io >==>^ writeRequestBody(req.bodyB)
  }

  def sendRequestL(httpClient: OkHttpClient, log: Req => Fx[Unit])(req: Req): FxE[HttpURLConnection] =
    sendRequest(httpClient)(req) <<^ log(req)

  def openConn(httpClient: OkHttpClient, e: Endpoint): FxE[HttpURLConnection] = FxE {
    val conn = httpClient.open(e.url)
    conn.setRequestProperty("Content-Type", contentTypeJson)
    conn.setRequestMethod(e.method.value)
    for (c <- e.credential) conn.addRequestProperty("Authorization", c.getHeaderValue)
    conn
  }

  def writeRequestBody(body: Array[Byte])(conn: HttpURLConnection): FxE[Unit] =
    Fx(ErrorOr.withResource(conn.getOutputStream)(_.close)(_ write body))

  // ---------------------------------------------------------------------------
  // Response

  def recv(f: HttpURLConnection => InputStream): HttpURLConnection => FxE[String] = conn =>
    Fx(ErrorOr.withResource(f(conn))(_.close){ in =>
      val entity: HttpEntity = new InputStreamEntity(in)
      val charset = Option(ContentType get entity).fold(defaultCharset)(_.getCharset)
      val bytes = EntityUtils.toByteArray(entity)
      new String(bytes, charset)
    })

  val recvResponseInput = recv(_.getInputStream)

  val recvResponseError = recv(_.getErrorStream)

  def getResponseCode(conn: HttpURLConnection): FxE[Int] =
    FxE(conn.getResponseCode)

  def recvResponseG[R](ko: HttpURLConnection => String => FxE[R])
                      (log: String => Fx[Unit], ok: JValue => ErrorOr[R])
                      (conn: HttpURLConnection): FxE[R] =
    getResponseCode(conn) >==> (code =>
      if (code == HttpURLConnection.HTTP_OK)
        recvResponseInput(conn) <-<^ log >=> (parseIntoJson(_) >=> ok)
      else
        recvResponseError(conn) <-<^ log >==> ko(conn)
      )

  def recvResponse[R] = recvResponseG[R](genericHttpErrorN) _

  def recvResponseE[R, E](ep: ErrParser[E], er: E => Option[R]) =
    recvResponseG[R](conn => handleErrorResponse(ep, er, genericHttpError(conn))) _

  // ---------------------------------------------------------------------------
  // Error handling

  case class ErrParser[E](parse: JValue => ErrorOr[E], mkError: E => Error)

  def handleErrorResponse[R, E](ep: ErrParser[E], mkResult: E => Option[R], fallback: String => Fx[Error])(resp: String): FxE[R] =
    parseIntoJson(resp) >==> parseErrorJson(ep, mkResult) match {
      case \/-(-\/(e)) => Fx(ep.mkError(e).toErrorOr)
      case \/-(\/-(r)) => FxE.pure(r)
      case -\/(_)      => fallback(resp).map(_.toErrorOr)
    }

  def parseErrorJson[R, E](ep: ErrParser[E], mkResult: E => Option[R]): JValue => ErrorOr[E \/ R] =
    ep.parse(_) >-> tryMkResult(mkResult)

  def tryMkResult[R, E](mkResult: E => Option[R])(e: E): E \/ R =
    mkResult(e) match {
      case Some(r) => \/-(r)
      case None    => -\/(e)
    }

  def genericHttpError(c: HttpURLConnection)(resp: String): Fx[Error] =
    Fx(Error(s"Unexpected HTTP response: ${c.getResponseCode} ${c.getResponseMessage}. Response: $resp"))

  def genericHttpErrorN[N](c: HttpURLConnection)(resp: String): FxE[N] =
    genericHttpError(c)(resp).map(_.toErrorOr)
}
