package shipreq.taskman.server.business

import com.squareup.okhttp.{Credentials, OkHttpClient, OkUrlFactory}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.io.{InputStream, OutputStream}
import java.net.{HttpURLConnection, URL}
import java.nio.charset.Charset
import org.apache.http.HttpEntity
import org.apache.http.entity.{ContentType, InputStreamEntity}
import org.apache.http.util.EntityUtils
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scalaz.syntax.bind._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ArticulateError, Identity}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.Logger

final case class Http[I, O](runFn: (I, OkHttpClient, HttpLoggers) => Fx[O]) /*extends AnyVal*/ {

  def run(i: I)(implicit client: OkHttpClient, log: HttpLoggers): Fx[O] =
    log.result(runFn(i, client, log))

  def contramap[A](f: A => I): Http[A, O] =
    Http((a, c, l) => runFn(f(a), c, l))

  def and[A](f: Fx[O] => Fx[A]): Http[I, A] =
    Http((i, c, l) => f(runFn(i, c, l)))

  def tap[A](f: O => A): Http[I, O] =
    and(_.unsafeTap(f))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Http {

  final case class Credential(getHeaderValue: String)
  object Credential {
    def basic(username: String, password: String): Credential =
      Credential(Credentials.basic(username, password))
  }

  sealed abstract class Method(val value: String) {
    def apply(url: String): Http[Unit, HttpURLConnection] =
      Http[Unit, HttpURLConnection]((_, client, _) => Fx(new OkUrlFactory(client).open(new URL(url))))
        .tap(_.setRequestMethod(value))
  }
  case object Get extends Method("GET")
  case object Put extends Method("PUT")
  case object Post extends Method("POST")
  case object Delete extends Method("DELETE")

  val DefaultCharset = Charset.forName("UTF-8")
  val ContentTypeJson = s"application/json;charset=${DefaultCharset.name}"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def readResponseStream(conn: HttpURLConnection, stream: HttpURLConnection => InputStream): Fx[String] =
    Fx(stream(conn)).bracket(
      release = i => Fx(i.close()),
      use = i => Fx {
        val entity: HttpEntity = new InputStreamEntity(i)
        val charset = Option(ContentType get entity).fold(DefaultCharset)(_.getCharset)
        val bytes = EntityUtils.toByteArray(entity)
        new String(bytes, charset)
      })

  def genericResponseErrorHandler[A](c: HttpURLConnection, response: Any): Fx[A] =
    Fx.fail(ArticulateError(s"Unexpected HTTP response: ${c.getResponseCode} ${c.getResponseMessage}. Response: $response"))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class HttpExt1(private val http: Http[Unit, HttpURLConnection]) extends AnyVal {
    def authWith(c: Credential): Http[Unit, HttpURLConnection] =
      http.tap(_.addRequestProperty("Authorization", c.getHeaderValue))

    def request[I](prep: (HttpLoggers, HttpURLConnection, I) => Fx[Option[OutputStream => Fx[Unit]]]): Http[I, HttpURLConnection] =
      Http { (i, client, log) =>

        def writeRequestContent(conn: HttpURLConnection, w: OutputStream => Fx[Unit]): Fx[Unit] =
          Fx(conn.getOutputStream).bracket(release = c => Fx(c.close()), use = w)

        for {
          conn  <- http.runFn((), client, log)
          reqFn <- prep(log, conn, i)
          _     <- reqFn.fold(Fx.unit)(writeRequestContent(conn, _))
        } yield conn
      }

    def jsonRequest: Http[JValue, HttpURLConnection] =
      request[JValue]((log, conn, i) =>
        for {
          _   <- Fx(conn.setRequestProperty("Content-Type", ContentTypeJson))
          str <- Fx(compact(i))
          _   <- log.request(conn, str)
        } yield
          Option.unless(str.isEmpty)(o => Fx(o write str.getBytes(DefaultCharset)))
        )
  }

  implicit class HttpExt2[I](private val http: Http[I, HttpURLConnection]) extends AnyVal {
    def readResponse: Http[I, (HttpURLConnection, String \/ String)] =
      Http((i, client, log) =>
        log.response(
          http.runFn(i, client, log).flatMap(conn =>
            if (conn.getResponseCode ==* HttpURLConnection.HTTP_OK)
              readResponseStream(conn, _.getInputStream).map(r => (conn, \/-(r)))
            else
              readResponseStream(conn, _.getErrorStream).map(r => (conn, -\/(r))))))

    def jsonResponse: Http[I, (HttpURLConnection, JValue \/ JValue)] =
      readResponse.and(_.flatMap {
        case (conn, \/-(str)) => Fx(parse(str)).map(j => (conn, \/-(j)))
        case (conn, -\/(str)) => Fx(parse(str)).map(j => (conn, -\/(j)))
      })
  }

  implicit class HttpExt3[I](private val http: Http[I, (HttpURLConnection, JValue \/ JValue)]) extends AnyVal {
    def parseJsonResponse[O](ok: JValue => ArticulateError \/ O,
                             ko: (HttpURLConnection, JValue) => Fx[O] = genericResponseErrorHandler[O](_, _)): Http[I, O] =
      http.and(_.flatMap {
        case (_, \/-(j)) => Fx.lift(ok(j)).mapArticulateError(_.hint(s"Response = $j"))
        case (c, -\/(j)) => ko(c, j).mapArticulateError(_.hint(s"Response = $j", s"Code = ${c.getResponseCode}"))
      })
  }

}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object HttpLoggers {
  def apply(logger: Logger#AtLevel, mod: String => String = Identity.apply): HttpLoggers =
    new HttpLoggers(logger, mod)
}
final class HttpLoggers(logger: Logger#AtLevel, mod: String => String) {
  private[this] val enabled = logger.?
  private def p(prefix: String, str: String) = if (str.isEmpty) "" else prefix + str

  def logFx(s: => String): Fx[Unit] =
    Fx(logger.z(mod(s)))

  private val logIfEnabled: (=> String) => Fx[Unit] =
    if (enabled)
      logFx
    else
      _ => Fx.unit

  val request: (HttpURLConnection, String) => Fx[Unit] =
    (c, body) => logIfEnabled(s"HTTP request: ${c.getRequestMethod} ${c.getURL.toExternalForm}${p(" ← ", body)}")

  def response[A](fx: Fx[(HttpURLConnection, A)]): Fx[(HttpURLConnection, A)] =
    if (enabled)
      fx.tap(x =>
        logFx(s"HTTP response: ${x._1.getResponseCode} ${x._1.getResponseMessage}${p(" → ", x._2.toString)}"))
    else
      fx

  def result[A](fx: Fx[A]): Fx[A] =
    if (enabled)
      fx.attemptArticulateError.flatMap(r => logFx(s"HTTP result: $r") >> Fx.lift(r))
    else
      fx
}
