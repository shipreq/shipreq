package shipreq.taskman.server.business

import com.typesafe.scalalogging.Logger
import japgolly.univeq._
import java.nio.charset.Charset
import okhttp3._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scalaz.syntax.bind._
import scalaz.syntax.catchable._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ArticulateError, Identity}
import shipreq.base.util.FxModule._
import Http._

final case class Http[I, O](prep: (I, HttpClient, HttpLoggers) => Fx[Request],
                            recv: (Response, String) => Fx[O]) {

  def run(i: I)(implicit client: HttpClient, log: HttpLoggers): Fx[O] =
    log.result(
      prep(i, client, log)
        .map(req => client.newCall(req).execute())
        .bracket(
          release = r => Fx(r.close()),
          use = r =>
            Fx(r.body.string())
              .tap(log.response(r, _))
              .flatMap(recv(r, _))))

  def contramap[A](f: A => I): Http[A, O] =
    Http((a, c, l) => prep(f(a), c, l), recv)

  def and[A](f: Fx[O] => Fx[A]): Http[I, A] =
    Http(prep, (r, s) => f(recv(r, s)))

  def and[A](f: (Response, Fx[O]) => Fx[A]): Http[I, A] =
    Http(prep, (r, s) => f(r, recv(r, s)))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Http {

  type HttpClient = OkHttpClient

  final case class Credential(getHeaderValue: String)
  object Credential {
    def basic(username: String, password: String): Credential =
      Credential(Credentials.basic(username, password))
  }

  sealed abstract class Method(val value: String)
  case object Get extends Method("GET") {
    def apply(url: String): PreCallGet =
      PreCallGet((_, _) => Fx(new Request.Builder().url(url)))
  }
  case object Put extends Method.WithRequest("PUT")
  case object Post extends Method.WithRequest("POST")
  case object Delete extends Method.WithRequest("DELETE")
  object Method {
    sealed abstract class WithRequest(value: String) extends Method(value) {
      final def apply(url: String): PreCall[Unit] =
        PreCall((_, _, _) => Fx(new Request.Builder().url(url)), this)
    }
    implicit def univEqMethod: UnivEq[Method] = UnivEq.derive
  }

  val DefaultCharset = Charset.forName("UTF-8")
  val MediaTypeJson = MediaType.parse(s"application/json;charset=${DefaultCharset.name}")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class PreCallGet(prep: (HttpClient, HttpLoggers) => Fx[Request.Builder]) {
    def map(f: Request.Builder => Request.Builder): PreCallGet =
      PreCallGet(prep(_, _) map f)

    def authWith(c: Credential): PreCallGet =
      map(_.addHeader("Authorization", c.getHeaderValue))

    def noRequest: Http[Unit, Unit] =
      Http(
        (_, c, log) => prep(c, log).map(_.build).tap(log.request(_, () => "")),
        (_, _) => Fx.unit)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class PreCall[I](prep: (I, HttpClient, HttpLoggers) => Fx[Request.Builder], method: Method) {
    def contramap[A](f: A => I): PreCall[A] =
      PreCall((a, c, l) => prep(f(a), c, l), method)

    def map(f: Request.Builder => Request.Builder): PreCall[I] =
      PreCall(prep(_, _, _) map f, method)

    def authWith(c: Credential): PreCall[I] =
      map(_.addHeader("Authorization", c.getHeaderValue))
  }

  implicit class PreCallExt1(private val http: PreCall[Unit]) extends AnyVal {
    def request[I](buildReq: (Request.Builder, I) => Fx[(Request, () => String)]): Http[I, Unit] =
      Http((i, client, log) =>
        for {
          reqBuilder  ← http.prep((), client, log)
          built       ← buildReq(reqBuilder, i)
          (req, body) = built
          _           ← log.request(req, body)
        } yield req,
        (_, _) => Fx.unit)

    def jsonRequest: Http[JValue, Unit] =
      request[JValue]((b, i) => Fx {
        val str = compact(i)
        val body = RequestBody.create(MediaTypeJson, str.getBytes(DefaultCharset))
        val req = b.method(http.method.value, body).build
        (req, () => str)
      })

    def formRequest[I](formData: I => TraversableOnce[(String, String)]): Http[I, Unit] =
      request[I]((reqBuilder, i) => Fx {
        val bodyBuilder = new FormBody.Builder()
        formData(i).foreach(x => bodyBuilder.add(x._1, x._2))
        val body = bodyBuilder.build()
        val req = reqBuilder.method(http.method.value, body).build
        (req, () => formData(i).toIterator.map(x => s"${x._1}=${x._2}").mkString("{", ", ", "}"))
      })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class HttpExt1[I](private val http: Http[I, Unit]) extends AnyVal {
    def jsonResponse: Http[I, JValue \/ JValue] =
      http.copy(recv = (resp, body) => Fx {
        val json = parse(body)
        if (resp.code ==* 200) \/-(json) else -\/(json)
      })
  }

  implicit class HttpExt2[I](private val http: Http[I, JValue \/ JValue]) extends AnyVal {
    def parseJsonResponse[O](ok: JValue => ArticulateError \/ O,
                             ko: (Response, JValue) => Fx[O] = genericResponseErrorHandler[O](_, _)): Http[I, O] =
      http.and((r, fx) => fx.flatMap {
        case \/-(j) => Fx.lift(ok(j)).mapArticulateError(_.hint(s"Response = $j"))
        case -\/(j) => ko(r, j).mapArticulateError(_.hint(s"Response = $j", s"Code = ${r.code}"))
      })
  }

  def genericResponseErrorHandler[A](r: Response, response: Any): Fx[A] = {
    val respString: String =
      response match {
        case j: JValue => compact(j)
        case a         => a.toString
      }
    Fx.fail(ArticulateError(s"Unexpected HTTP response: ${r.code} ${r.message}. Response: $respString"))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object HttpLoggers {
  def apply(logger: Logger, mod: String => String = Identity.apply): HttpLoggers =
    new HttpLoggers(logger.underlying, mod)
}
final class HttpLoggers(logger: org.slf4j.Logger, mod: String => String) {
  private[this] val enabled =
    logger.isDebugEnabled

  private def p(prefix: String, str: String) =
    if (str.isEmpty) "" else prefix + str

  def logFx(s: => String): Fx[Unit] =
    Fx(logger.debug(mod(s)))

  private val logIfEnabled: (=> String) => Fx[Unit] =
    if (enabled)
      logFx
    else
      _ => Fx.unit

  val request: (Request, () => String) => Fx[Unit] =
    (r, body) => logIfEnabled(s"HTTP request: ${r.method} ${r.url.url.toExternalForm}${p(" ← ", body())}")

  val response: (Response, String) => Fx[Unit] =
    (r, body) => logIfEnabled(s"HTTP response: ${r.code} ${r.message}${p(" → ", body)}")

  def result[A](fx: Fx[A]): Fx[A] =
    if (enabled)
      fx.attemptArticulateError.flatMap(r => logFx(s"HTTP result: $r") >> Fx.lift(r))
    else
      fx
}
