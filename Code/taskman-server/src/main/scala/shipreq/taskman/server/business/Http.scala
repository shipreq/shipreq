package shipreq.taskman.server.business

import com.squareup.okhttp._
import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.nio.charset.Charset
import org.apache.http.entity._
import org.apache.http.HttpEntity
import org.apache.http.util.EntityUtils
import org.json4s._
import org.json4s.jackson.JsonMethods._
import japgolly.univeq._
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.applicative._
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.Logger

object Http {

  sealed abstract class Method(val value: String)
  case object Get extends Method("GET")
  case object Put extends Method("PUT")
  case object Post extends Method("POST")
  case object Delete extends Method("DELETE")

  final case class Credential(getHeaderValue: String)
  object Credential {
    def basic(username: String, password: String): Credential =
      Credential(Credentials.basic(username, password))
  }

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

  def parseIntoJson(str: String): ArticulateError \/ JValue =
    ArticulateError.attempt(parse(str))

  // ---------------------------------------------------------------------------
  // Request

  def sendRequest(httpClient: OkHttpClient)(req: Req): Fx[HttpURLConnection] = {
    val open = openConn(httpClient, req.e)
    if (req.bodyS.isEmpty)
      open
    else
      open tap writeRequestBody(req.bodyB)
  }

  def sendRequestAndLog(httpClient: OkHttpClient, log: Req => Fx[Unit])(req: Req): Fx[HttpURLConnection] =
    sendRequest(httpClient)(req) tap_ log(req)

  def openConn(httpClient: OkHttpClient, e: Endpoint): Fx[HttpURLConnection] =
    Fx {
      val conn = new OkUrlFactory(httpClient).open(e.url)
      conn.setRequestProperty("Content-Type", contentTypeJson)
      conn.setRequestMethod(e.method.value)
      for (c <- e.credential) conn.addRequestProperty("Authorization", c.getHeaderValue)
      conn
    }

  def writeRequestBody(body: Array[Byte])(conn: HttpURLConnection): Fx[Unit] =
    Fx(conn.getOutputStream).bracket(
      release = c => Fx(c.close()),
      use     = c => Fx(c write body))

  // ---------------------------------------------------------------------------
  // Response

  def recv(f: HttpURLConnection => InputStream): HttpURLConnection => Fx[String] = conn =>
    Fx(f(conn)).bracket(
      release = i => Fx(i.close()),
      use = i => Fx {
        val entity: HttpEntity = new InputStreamEntity(i)
        val charset = Option(ContentType get entity).fold(defaultCharset)(_.getCharset)
        val bytes = EntityUtils.toByteArray(entity)
        new String(bytes, charset)
      })

  val recvResponseInput = recv(_.getInputStream)

  val recvResponseError = recv(_.getErrorStream)

  def getResponseCode(conn: HttpURLConnection): Fx[Int] =
    Fx(conn.getResponseCode)

  def recvResponseG[A](ko: (HttpURLConnection, String) => Fx[A])
                      (log: String => Fx[Unit], ok: JValue => ArticulateError \/ A)
                      (conn: HttpURLConnection): Fx[A] =
    getResponseCode(conn).flatMap(code =>
      if (code ==* HttpURLConnection.HTTP_OK)
        for {
          s <- recvResponseInput(conn)
          _ <- log(s)
          a <- Fx.lift(parseIntoJson(s).flatMap(ok))
        } yield a
      else
        for {
          s <- recvResponseError(conn)
          _ <- log(s)
          a <- ko(conn, s)
        } yield a
    )

  def recvResponse[R] = recvResponseG[R](genericHttpErrorN) _

  def recvResponseE[R, E](ep: ErrParser[E], er: E => Option[R]) =
    recvResponseG[R]((conn, resp) => handleErrorResponse(ep)(er)(genericHttpErrorN(conn, _), resp)) _

  // ---------------------------------------------------------------------------
  // Error handling

  final case class ErrParser[E](parse: JValue => ArticulateError \/ E,
                                mkError: E => ArticulateError)

  def handleErrorResponse[R, E](ep: ErrParser[E])(mkResult: E => Option[R])(fallback: String => Fx[R], resp: String): Fx[R] =
    Fx.lift(parseIntoJson(resp)).flatMap(j =>
      parseErrorJson(ep, mkResult, j) match {
        case \/-(-\/(e)) => Fx.fail(ep.mkError(e))
        case \/-(\/-(r)) => Fx.pure(r)
        case -\/(_)      => fallback(resp)
      })

  def parseErrorJson[R, E](ep: ErrParser[E], mkResult: E => Option[R], json: JValue): ArticulateError \/ (E \/ R) =
    ep.parse(json).map(tryMkResult(mkResult))

  def tryMkResult[R, E](mkResult: E => Option[R])(e: E): E \/ R =
    mkResult(e) match {
      case Some(r) => \/-(r)
      case None    => -\/(e)
    }

  def genericHttpError(c: HttpURLConnection, resp: String): Fx[ArticulateError] =
    Fx(ArticulateError(s"Unexpected HTTP response: ${c.getResponseCode} ${c.getResponseMessage}. Response: $resp"))

  def genericHttpErrorN[N](c: HttpURLConnection, resp: String): Fx[N] =
    genericHttpError(c, resp).flatMap(Fx.fail)
}
