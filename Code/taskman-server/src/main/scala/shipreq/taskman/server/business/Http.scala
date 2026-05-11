package shipreq.taskman.server.business

import com.typesafe.scalalogging.Logger
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.nio.charset.Charset
import java.time.Duration
import okhttp3._
import okio.Buffer
import scala.util.control.NonFatal
import shipreq.base.util.FxModule._
import shipreq.base.util.log.TaskmanLogFields
import shipreq.base.util.{ArticulateError, Identity}
import shipreq.taskman.server.business.Http._

final case class Http[I, O](prep: (I, HttpClient, HttpLogger) => Fx[Request],
                            recv: (Response, String) => Fx[O]) {

  def run(i: I)(implicit client: HttpClient, log: HttpLogger): Fx[O] =
    log.result(
      prep(i, client, log).flatMap(req =>
        Fx(client.newCall(req).execute()).bracketFx(
          release = resp => Fx(resp.close()),
          use = resp =>
            Fx(resp.body.string())
              .tap(log.response(req, resp, _))
              .flatMap(recv(resp, _)))))

  def contramap[A](f: A => I): Http[A, O] =
    Http((a, c, l) => prep(f(a), c, l), recv)

  def map[A](f: O => A): Http[I, A] =
    Http(prep, recv(_, _).map(f))

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

  final case class PreCallGet(prep: (HttpClient, HttpLogger) => Fx[Request.Builder]) {
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

  final case class PreCall[I](prep: (I, HttpClient, HttpLogger) => Fx[Request.Builder], method: Method) {
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
          reqBuilder  <- http.prep((), client, log)
          (req, body) <- buildReq(reqBuilder, i)
          _           <- log.request(req, body)
        } yield req,
        (_, _) => Fx.unit)

    def requestAsJson[A: Encoder]: Http[A, Unit] =
      request[A]((b, a) => Fx {
        val json = a.asJson
        val str  = json.noSpaces
        val body = RequestBody.create(MediaTypeJson, str.getBytes(DefaultCharset))
        val req  = b.method(http.method.value, body).build
        (req, () => str)
      })

    def requestAsForm[I](formData: I => IterableOnce[(String, String)]): Http[I, Unit] =
      request[I]((reqBuilder, i) => Fx {
        val bodyBuilder = new FormBody.Builder()
        formData(i).iterator.foreach(x => bodyBuilder.add(x._1, x._2))
        val body = bodyBuilder.build()
        val req = reqBuilder.method(http.method.value, body).build
        (req, () => formData(i).iterator.map(x => s"${x._1}=${x._2}").mkString("{", ", ", "}"))
      })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class HttpExt1[I](private val http: Http[I, Unit]) extends AnyVal {

    def responseAsJson[A: Decoder]: Http[I, A] =
      http.copy(recv = (r, body) =>
        if (isStatusSuccessful(r.code))
          decode[A](body) match {
            case Right(a) => Fx.pure(a)
            case Left(e)  => Fx.fail(ArticulateError(e).hint(s"Response = $body", s"StatusCode = ${r.code}"))
          }
        else
          genericResponseErrorHandler[A](r, body)
      )

    def responseAsStatusCodeAndJson: Http[I, (Int, Json)] =
      http.copy(recv = (r, body) =>
        parse(body) match {
          case Right(json) => Fx.pure((r.code, json))
          case Left(e)     => Fx.fail(ArticulateError(e).hint(s"Response = $body", s"StatusCode = ${r.code}"))
        }
      )

    def responseAsJsonOrJson[A: Decoder]: Http[I, Json \/ A] =
      responseAsStatusCodeAndJson.and(_.flatMap {
        case (code, json) =>
          if (isStatusSuccessful(code))
            Decoder[A].decodeJson(json) match {
              case Right(a) => Fx.pure(\/-(a))
              case Left(e)  => Fx.fail(ArticulateError(e).hint(s"Response = ${json.noSpaces}", s"StatusCode = ${code}"))
            }
          else
            Fx.pure(-\/(json))
      })
  }

  /** Generic HTTP code validation */
  private def isStatusSuccessful(status: Int): Boolean =
    (status >= 200 && status < 300) || status == 304

  def genericResponseErrorHandler[A](r: Response, response: Any): Fx[A] = {
    val respString: String =
      response match {
        case j: Json => j.noSpaces
        case a       => a.toString
      }
    Fx.fail(ArticulateError(s"Unexpected HTTP response: ${r.code} ${r.message}. Response: $respString"))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object HttpLogger {
  def apply(logger: Logger, modContent: String => String = Identity.apply): HttpLogger =
    new HttpLogger(logger, modContent)
}

final class HttpLogger(logger: Logger, modContent: String => String) {
  private[this] val debugEnabled = logger.underlying.isDebugEnabled

  val request: (Request, () => String) => Fx[Unit] =
    if (debugEnabled)
      (req, body) => Fx {
        def url = req.url.url.toExternalForm
        logger.debug(s"HTTP request: ${req.method} $url ← ${modContent(body())}")
      }
    else
      (_, _) => Fx.unit

  val response: (Request, Response, String) => Fx[Unit] =
    (req, resp, respBody) => Fx {
      val url = req.url.url.toExternalForm
      val dur = Duration.ofMillis(resp.receivedResponseAtMillis() - resp.sentRequestAtMillis())
      logger.info(
        s"HTTP ${req.method} $url responded with ${resp.code} in ${dur.conciseDesc}",
        TaskmanLogFields.http.request.body(modContent(requestBody(req))),
        TaskmanLogFields.http.request.method(req.method),
        TaskmanLogFields.http.request.url(url),
        TaskmanLogFields.http.response.body(modContent(respBody)),
        TaskmanLogFields.http.response.code(resp.code),
        TaskmanLogFields.http.response.durMs(dur),
      )
    }

  private def requestBody(req: Request): String =
    try {
      val copy = req.newBuilder().build()
      val buffer = new Buffer()
      copy.body().writeTo(buffer)
      buffer.readUtf8()
    } catch {
      case NonFatal(_) => "?"
    }

  def result[A](fx: Fx[A]): Fx[A] =
    if (debugEnabled)
      fx.attemptArticulateError.flatMap(r => Fx(logger.debug(s"HTTP result: ${modContent(r.toString)}")) >> Fx.lift(r))
    else
      fx
}
