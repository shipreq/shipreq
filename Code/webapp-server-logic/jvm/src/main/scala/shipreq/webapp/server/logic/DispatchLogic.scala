package shipreq.webapp.server.logic

import boopickle.Pickler
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.utils.Utils
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.ParseLong
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scala.util.{Failure, Success}
import scalaz.{-\/, Monad, Need, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.{AssetManifest, Urls}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol2.{BinaryJvm, Protocol}
import shipreq.webapp.base.user._
import shipreq.webapp.base.util.ResourceHint
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.ServerConfig

/** Usage:
  *
  * 1. Wire up [[DispatchLogic.Ops]] to session-less dispatch.
  *    Use [[DispatchLogic.Ops.candidate]] as the condition and [[DispatchLogic.Ops.total]] as the handler.
  *
  * 2. Wire up [[DispatchLogic.mainDispatcher()]] to normal (session-dependent) dispatch.
  */
object DispatchLogic {

  /** A request to the server.
    *
    * @param path Does *NOT* include query params
    */
  final case class Request[+Real](method: Method,
                                  path  : Url.Relative,
                                  body  : Need[Option[BinaryData]],
                                  param : String => Option[String],
                                  cookie: Cookie.LookupFn,
                                  real  : Real)

  sealed abstract class Method
  object Method {
    case object Get   extends Method
    case object Post  extends Method
    case object Other extends Method
    implicit def univEq: UnivEq[Method] = UnivEq.derive
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class Response(cmd: ResponseCmd, cookies: Cookie.Update)

  sealed trait ResponseCmd {
    def headers: ResponseCmd.Headers = Nil
  }

  object ResponseCmd {

    type Header = (String, String)
    type Headers = List[Header]

    implicit class ResourceHintExt(private val rh: ResourceHint) extends AnyVal {
      def headerValue: String = {
        val g = rh.generic
        var h = s"<${g.href}>; rel=${g.rel}"
        g.as.foreach(as => h = s"$h; as=$as")
        g.`type`.foreach(t => h = h + "; type=\"" + t + "\"")
        g.crossorigin match {
          case Some("anonymous") => h = h + "; crossorigin"
          case Some(v)           => h = h + "; crossorigin=\"" + v + "\""
          case None              => ()
        }
        if (g.relativeHref) h = h + "; nopush"
        h
      }
    }

    final case class ServePublicSpa(user: Option[User]) extends ResponseCmd

    final case class ServeHomeSpa(user: User) extends ResponseCmd

    object ProjectSpa {
      final case class Serve(user: User, projectId: ProjectId) extends ResponseCmd
      case object NotOwner extends ResponseCmd
      case object InvalidId extends ResponseCmd
    }

    final case class Redirect(dest: Url.Relative) extends ResponseCmd

    /** Respond with a HTTP status only; no content */
    final case class StatusOnly(status: Int) extends ResponseCmd {
      val withoutCookieUpdate = Response(this, Cookie.Update.empty)
    }

    object StatusOnly {

      val OK = apply(200)

      /** The server cannot or will not process the request due to an apparent client error
        * (e.g., malformed request syntax, size too large). */
      val BadRequest = apply(400)

      /** The request was valid, but the server is refusing action.
        * The user might not have the necessary permissions for a resource, or may need an account of some sort. */
      val Forbidden = apply(403)

      /** The requested resource could not be found but may be available in the future. */
      val NotFound = apply(404)

      /** Indicates the HTTP method (GET, POST etc) wasn't allowed */
      val MethodNotAllowed = apply(405)
    }

    final case class Text(status: Int, body: String) extends ResponseCmd

    final case class Json(status: Int, bodyJs: upickle.Js.Value) extends ResponseCmd {
      val body: String =
        upickle.json.write(bodyJs)
    }

    final case class Binary(status: Int, body: BinaryData) extends ResponseCmd

    implicit def univEq: UnivEq[ResponseCmd] = {
      implicit def a: UnivEq[upickle.Js.Value] = UnivEq.force
      UnivEq.derive
    }

    val redirectToPublicHome = Redirect(Urls.publicHome)
    val redirectToMemberHome = Redirect(Urls.memberHome)
  }

  object StatusCode {
    final val OK         = 200
    final val BadRequest = 400
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @inline private[this] def isSepChar(c: Char): Boolean =
    c == '/' || c == '#'

  /** If arg is /home this will match:
    *
    * /home
    * /home/…
    * /home#…
    */
  def spaTest(au: Url.Relative): Request[Any] => Boolean = {
    val a = au.relativeUrlNoHeadSlash
    val aLen = a.length
    req => {
      val bu = req.path
      val b = bu.relativeUrlNoHeadSlash
      val cmp = b.length - aLen
      if (cmp == 0)
        b == a
      else
        (cmp > 0) && isSepChar(b(aLen)) && b.startsWith(a)
    }
  }

  def spaTest1(au: Url.Relative.Param1[_]): Request[Any] => Option[String] = {
    val prefix = au.prefixNoHeadSlash
    val prefixLen = prefix.length
    req => {
      val path = req.path.relativeUrlNoHeadSlash
      Option.when(path.startsWith(prefix)) {
        var i = prefixLen
        val pathLastIdx = path.length - 1
        while (i <= pathLastIdx && !isSepChar(path(i)))
          i += 1
        path.substring(prefixLen, i)
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Prefix for all ops routes */
  val opsRoot = Url.Relative("/ops")

  val opsSecretKey = "secret"

  /** This is a lazy, hardcoded password to validate admin access. When ShipReq gets serious this shit will be
    * replaced with proper authentication and authorisation.
    */
  val opsSecretValue = PlainTextPassword("Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid")

  /** FOR UNIT-TESTS ONLY */
  val unitTestLoginUrl = Url.Relative("/c8c8f430-93b2-43fe-a072-11f9a1ab52a0")

  /** DEV-MODE ONLY */
  val quickDevUrl = Url.Relative("/xx")
  final case class QuickDev(user: Username \/ EmailAddr,
                            pass: PlainTextPassword,
                            goto: Url.Relative)
  object QuickDev {
    import japgolly.clearconfig._

    def config =
      ( ConfigDef.need[String]("USER").map(Username.orEmail) |@|
        ConfigDef.need[String]("PASS").map(PlainTextPassword(_)) |@|
        ConfigDef.get [String]("GOTO").map(_.fold(Urls.memberHome)(Url.Relative(_)))
      )(apply).withPrefix("SHIPREQ_DEV_")

    def get(): Option[QuickDev] = {
      import FxModule._
      config.run(Props.sources).unsafeRun().toDisjunction.toOption
    }
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class DispatchLogic[F[_], RealReq, RealRes](readRealReq: RealReq => DispatchLogic.Request[RealReq],
                                                  makeRealRes: (RealReq, DispatchLogic.Response) => F[RealRes])
                                                 (implicit F: Monad[F],
                                                  config    : ServerConfig,
                                                  db        : DB.SecurityTokenReadOnly[F],
                                                  metrics   : MetricsLogic[F],
                                                  ops       : OpsEndpoints[F],
                                                  publicSpa : PublicSpaLogic[F],
                                                  security  : Security.Algebra[F],
                                                  svrS      : Server.Session[F],
                                                  svr       : Server.Time[F],
                                                  tracer    : TraceLogic[F, RealReq, RealRes]) {

  import DispatchLogic.{Request => _, _}
  import DispatchLogic.Method._

  private type Request = DispatchLogic.Request[RealReq]

  @inline private def makeReal(r: Response)(implicit req: Request): F[RealRes] =
    makeRealRes(req.real, r)

  @inline private def makeRealF(f: F[Response])(implicit req: Request): F[RealRes] =
    F.bind(f)(makeRealRes(req.real, _))

  @inline private def traceUrl(url: Url.Relative, f: F[RealRes])(implicit req: Request): F[RealRes] =
    traceUrlWithSpan(url, _ => f)

  @inline private def traceUrlWithSpan(url: Url.Relative, f: tracer.Span => F[RealRes])(implicit req: Request): F[RealRes] =
    metrics.setHttpName(url.relativeUrl) >> tracer.http(url.relativeUrl, req.real, req.path)(f)

  private def onMethod(m: Method)(f: F[Response])(implicit req: Request): F[RealRes] =
    if (req.method eq m)
      makeRealF(f)
    else
      makeReal(ResponseCmd.StatusOnly.MethodNotAllowed.withoutCookieUpdate)

  // Occasional type inference problem
  @inline private def when[A](cond: Request => Boolean)(ok: Request => F[A]): Request ?=> F[A] = FnWithFallback.when(cond)(ok)
  @inline private def extract[E, A](cond: Request => Option[E])(ok: Request => E => F[A]): Request ?=> F[A] = FnWithFallback.extract(cond)(ok)

  private def whenUrlIs[A](url: Url.Relative): (Request => F[A]) => Request ?=> F[A] =
    when(_.path ==* url)

  private def whenUrlIsAnyOf[A](urls: NonEmptySet[Url.Relative]): (Request => F[A]) => Request ?=> F[A] = {
    import Url.dropTailSlashes
    val norm: Url.Relative => String = u => dropTailSlashes(u.underlying)
    val lookup = Utils.quickStringExists(urls.whole.map(norm))
    when(r => lookup(norm(r.path)))
  }

  private def get(url: Url.Relative, resp: F[Response]): Request ?=> F[RealRes] =
    whenUrlIs(url)(implicit req =>
      traceUrl(req.path,
        onMethod(Get)(resp)))

  private def spa(root: Url.Relative, onGet: (tracer.Span, Request) => F[Response]): Request ?=> F[RealRes] =
    when(spaTest(root))(implicit req =>
      traceUrlWithSpan(root, span =>
        onMethod(Get)(onGet(span, req))))

  private def spaWithObfuscatedParam[A](url       : Url.Relative.Param1[Obfuscated[A]])
                                       (obfuscator: Obfuscator[A])
                                       (onGet     : (tracer.Span, Request, String \/ A) => F[Response]): Request ?=> F[RealRes] =
    extract(spaTest1(url))(implicit req => param =>
      traceUrlWithSpan(url.prefix, span =>
        onMethod(Get)(onGet(span, req, obfuscator.deobfuscate(Obfuscated(param))))))

  private def parseParams[A](parsed: Option[A])(f: A => F[Response]): F[Response] =
    parsed match {
      case Some(a) => f(a)
      case None    => F pure ResponseCmd.StatusOnly.BadRequest.withoutCookieUpdate
    }

  /**
    * Re-scopes routes so that they must first match a prefix.
    *
    * Eg. /a & /b can be re-scoped under /x resulting in /x/a and /x/b
    */
  private def scope[A](prefix: Url.Relative, routes: Request ?=> A): Request ?=> A = {
    val test = prefix.isEqualToOrParentOf
    val mod = prefix.removeSelfOrParent
    routes.embed(r => test(r.path), r => r.copy(path = mod(r.path)))
  }

  private def initOrExtendSession(cmd: Security.SessionToken => ResponseCmd)(implicit req: Request): F[Response] =
    for {
      so <- security.sessionRestore(req.cookie)
      s   = so.getOrElse(Security.SessionToken.anonymous)
      cu <- security.sessionPersist(s)
    } yield Response(cmd(s), cu)

  private def requireSession(fCmd: Security.SessionToken => F[Response])(implicit req: Request): F[Response] =
    security.sessionRestore(req.cookie).flatMap {
      case Some(s) => fCmd(s)
      case None    => F pure ResponseCmd.StatusOnly.Forbidden.withoutCookieUpdate
    }

  private def cmdWhenLoginRequired(implicit req: Request): ResponseCmd =
    ResponseCmd.Redirect(
      if (req.path ==* Urls.memberHome)
        Urls.login
      else
        Urls.login / req.path.relativeUrlNoHeadSlash)

  private def needAuth(f: User => F[ResponseCmd])(implicit span: tracer.Span, req: Request): F[Response] =
    for {
      sessionO <- security.sessionRestore(req.cookie)
      session   = sessionO.getOrElse(Security.SessionToken.anonymous)
      response <- session.authenticatedUser match {
                    case Some(u) =>
                      for {
                        _   <- tracer.alg.addAttrs(Trace.Attr.ShipReqUserId(u.id.value) :: Nil)
                        cmd <- f(u)
                        cu  <- security.sessionPersist(session)
                      } yield Response(cmd, cu)
                    case None =>
                      F pure Response(cmdWhenLoginRequired, Cookie.Update.empty)
                  }
    } yield response

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Main {

    val publicSpa: Request ?=> F[RealRes] = {
      import Urls.{PublicSpaRoute => R}

      // This logic is mirrored in .public.spa.Routes
      val login: Request ?=> F[RealRes] =
        spa(R.Login.url, (_, req) =>
          security.sessionRestore(req.cookie).flatMap { os =>
            val s = os.getOrElse(Security.SessionToken.anonymous)
            if (s.authenticatedUser.isEmpty)
              for {
                cu <- security.sessionPersist(s)
              } yield Response(ResponseCmd.ServePublicSpa(s.authenticatedUser), cu)
            else
              F pure Response(ResponseCmd.redirectToMemberHome, Cookie.Update.empty)
          })

      val staticRoutes: Request ?=> F[RealRes] =
        whenUrlIsAnyOf(R.static.filterNot(_ ==* R.Login).get.map(_.url).toNES) { implicit req =>
          traceUrl(req.path,
            onMethod(Get)(
              initOrExtendSession(t =>
                ResponseCmd.ServePublicSpa(t.authenticatedUser))))
        }

      val securityTokenFn: R.NeedsToken => SecurityToken => F[SecurityToken.Status] = {
        case R.Register2     => PublicSpaLogic.tokenStatusFn(db.getUserRegistrationTokenIssueDate, config.security.registrationTokenLifespan)
        case R.ResetPassword => PublicSpaLogic.tokenStatusFn(db.getResetPasswordTokenIssueDate, config.security.passwordResetTokenLifespan)
      }

      val securityTokenRoutes: Request ?=> F[RealRes] =
        R.needsToken.map { r =>
          val getTokenStatus = securityTokenFn(r)
          extract(spaTest1(r.url)) { implicit req => param =>
            val token = SecurityToken(param)
            traceUrl(r.url.prefix,
              onMethod(Get)(
                security.protect(
                  for {
                    status <- getTokenStatus(token)
                    resp   <- initOrExtendSession(s =>
                                status match {
                                  case SecurityToken.Status.Valid   => ResponseCmd.ServePublicSpa(s.authenticatedUser)
                                  case SecurityToken.Status.Invalid => ResponseCmd.redirectToPublicHome
                                  case SecurityToken.Status.Expired => ResponseCmd.redirectToPublicHome // could be better but good enough
                                }
                              )
                  } yield resp
                )))
          }
        }.reduce(_ | _)

      login | staticRoutes | securityTokenRoutes
    }

    val memberHomeSpa: Request ?=> F[RealRes] =
      spa(Urls.memberHome, needAuth(F pure ResponseCmd.ServeHomeSpa(_))(_, _))

    val projectSpa: Request ?=> F[RealRes] =
      spaWithObfuscatedParam(Urls.project)(Obfuscators.projectId) { (span, req, result) =>
        implicit val _span = span
        implicit val _req = req
        result match {
          case \/-(projectId) =>
            tracer.alg.addAttrs(Trace.Attr.ShipReqProjectId(projectId) :: Nil) >>
            needAuth(user =>
              // TODO Check ProjectStore first
              security.db.getProjectOwner(projectId).map {
                case Some(o) if o ==* user.id => ResponseCmd.ProjectSpa.Serve(user, projectId)
                case Some(_)                  => ResponseCmd.ProjectSpa.NotOwner
                case None                     => ResponseCmd.ProjectSpa.InvalidId
              }
            )
          case -\/(_) => F pure Response(ResponseCmd.ProjectSpa.InvalidId, Cookie.Update.empty)
        }
      }

    val logout: Request ?=> F[RealRes] =
      get(Urls.logout,
        for {
          cu <- SimpleEndpoints.logout
        } yield Response(ResponseCmd.redirectToPublicHome, cu)
      )

    val routes: Request ?=> F[RealRes] =
      publicSpa | memberHomeSpa | projectSpa | logout

    val fallback: Request => F[RealRes] = { implicit req =>
      val cmd =
        if (req.method eq Get)
          ResponseCmd.redirectToPublicHome
        else
          ResponseCmd.StatusOnly.MethodNotAllowed
      makeReal(Response(cmd, Cookie.Update.empty))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Ajax extends StrictLogging {

    private type Handler = (Security.SessionToken, BinaryData) => F[Response]

    private val _library = {
      val handlerMap = new Url.Relative.MutableMap[Handler]
      var nameMap = Map.empty[Url.Relative, String]

      def _register[A](p: Protocol.Ajax[Pickler], name: String)
                      (f: (Security.SessionToken, p.protocol.PreparedRequestType) => F[Response]): Unit = {
        assert(p.url.underlying startsWith Urls.ajaxRoot.underlying, s"${p.url} must start with ${Urls.ajaxRoot}")

        val h: Handler = (token, reqBin) =>
          BinaryJvm.attemptDecode(reqBin, p.prepReq) match {
            case Success(req) =>
              f(token, req)
            case Failure(t) =>
              F.point {
                logger.warn("Failed to decode ajax binary body.", t)
                ResponseCmd.StatusOnly.BadRequest.withoutCookieUpdate
              }
          }

        handlerMap += (p.url -> h)
        nameMap += (p.url -> name)
      }

      def register(p: Protocol.Ajax[Pickler])(name: String, f: p.ServerSideFn[F]): Unit =
        _register(p, name)((token, req) =>
          for {
            out   <- f(req)
            outBin = BinaryJvm.encode(p.responseProtocol(req))(out)
            resCmd = ResponseCmd.Binary(StatusCode.OK, outBin)
            cu    <- security.sessionPersist(token)
          } yield Response(resCmd, cu)
        )

      def registerA[A](p: Protocol.Ajax[Pickler])
                      (name: String, f: p.ServerSideFnA[F, A])
                      (g: (Security.SessionToken, ResponseCmd, A) => F[Response]): Unit =
        _register(p, name)((token, req) =>
          for {
            (out, a) <- f(req)
            outBin    = BinaryJvm.encode(p.responseProtocol(req))(out)
            resCmd    = ResponseCmd.Binary(StatusCode.OK, outBin)
            res      <- g(token, resCmd, a)
          } yield res
        )

      // Register endpoints
      register (PublicSpaProtocols.landingPage   )("landingPage"   , publicSpa.ajaxLandingPage   )
      registerA(PublicSpaProtocols.login         )("login"         , publicSpa.ajaxLogin         )(useNewToken)
      register (PublicSpaProtocols.register1     )("register1"     , publicSpa.ajaxRegister1     )
      registerA(PublicSpaProtocols.register2     )("register2"     , publicSpa.ajaxRegister2     )(useNewToken)
      register (PublicSpaProtocols.resetPassword1)("resetPassword1", publicSpa.ajaxResetPassword1)
      register (PublicSpaProtocols.resetPassword2)("resetPassword2", publicSpa.ajaxResetPassword2)

      (handlerMap.toMapNoHeadSlash, nameMap)
    }

    private[this] val handlers: Map[String, Handler] =
      _library._1

    val pathsToNames: Map[Url.Relative, String] =
      _library._2

    private def useNewToken(oldToken: Security.SessionToken, res: ResponseCmd, newToken: Option[Security.SessionToken]): F[Response] =
      security.sessionPersist(newToken getOrElse oldToken).map(Response(res, _))

    private val notFound: Response =
      Response(ResponseCmd.StatusOnly.NotFound, Cookie.Update.empty)

    val candidate: Url.Relative => Boolean =
      Urls.ajaxRoot.isEqualToOrParentOf

    val routes: Request ?=> F[RealRes] =
      when(r => candidate(r.path)) { implicit req =>

        handlers.get(req.path.relativeUrlNoHeadSlash) match {
          case Some(handler) =>

            val fResponse: F[Response] =
              requireSession(s =>
                if (req.method eq Post)
                  req.body.value match {
                    case Some(reqBin) => handler(s, reqBin)
                    case None         => F pure ResponseCmd.StatusOnly.BadRequest.withoutCookieUpdate
                  }
                else
                  F pure ResponseCmd.StatusOnly.MethodNotAllowed.withoutCookieUpdate
              )

            makeRealF(fResponse)

          case None =>
            makeReal(notFound)
        }
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Ops & Diagnostics

  object Ops {
    import upickle.Js
    import shipreq.taskman.api.MsgId

    private def response(cmd: ResponseCmd): Response =
      Response(cmd, Cookie.Update.empty)

    private val notFoundSecure: F[Response] =
      security.protect( // prevent response-time hacking to discover endpoints (meaning ops URLs)
        F pure ResponseCmd.StatusOnly.NotFound.withoutCookieUpdate)

    private def endpoint(method: Method, url: Url.Relative)(f: Request => F[Response]): Request ?=> F[RealRes] =
      whenUrlIs(url) { implicit req =>
        traceUrl(req.path,
          makeRealF(req.param(opsSecretKey) match {
            case Some(key) if key ==* opsSecretValue.value && req.method ==* method => security.protect(f(req))
            case _                                                                  => notFoundSecure
          })
        )
      }

    private def whenValid[A](fa: F[ErrorMsg \/ A])(f: A => Response): F[Response] =
      fa.map {
        case \/-(a) => f(a)
        case -\/(e) => response(ResponseCmd.Text(StatusCode.BadRequest, e.value))
      }

    private def jsonResponse(r: OpsEndpoints.HasJsValue): Response =
      response(ResponseCmd.Json(StatusCode.OK, r.toJsValue))

    /** Return a static 200.
      * Useful to test that the web-server is up and serving requests.
      * Used for container health-checks.
      */
    private val ok: Request ?=> F[RealRes] = {
      val r = response(ResponseCmd.Text(StatusCode.OK, "OK."))
      get(Url.Relative("ok"), F pure r)
    }

    /** API for invoking the first part of the registration process
      * (regardless of whether public registrations are enabled or not).
      */
    private val register1: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("register1"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(publicSpa.apiRegister1(email))(id =>
            response(ResponseCmd.Json(StatusCode.OK, Js.Obj("taskId" -> Js.Num(id.value)))))))

    private val statsDb: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("stats/db"))(
        Function const ops.dbStats.map(jsonResponse))

    private val statsUsers: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("stats/users"))(
        Function const ops.userStats.map(jsonResponse))

    /** API to inspect the status of a Taskman message. */
    private val task: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("task"))(req =>
        parseParams(req.param("id") flatMap ParseLong.unapply)(id =>
          ops.taskmanMsgStatus(MsgId(id)).map {
            case Some(r) => jsonResponse(r)
            case None    => response(ResponseCmd.StatusOnly.NotFound)
          }
        )
      )

    private val testSendMail: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("test-sendmail"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(ops.sendMail(email))(
            jsonResponse)))

    private def innerRoutes: Request ?=> F[RealRes] =
      ok | register1 | statsDb | statsUsers | task | testSendMail

    private def fallback: Request => F[RealRes] = { implicit req =>
      makeRealF(notFoundSecure)
    }

    val candidate: Url.Relative => Boolean =
      opsRoot.isEqualToOrParentOf

    val routes: Request ?=> F[RealRes] =
      when(r => candidate(r.path))(scope(opsRoot, innerRoutes).withFallback(fallback))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Other

  /** DEV-MODE ONLY */
  private val quickDev: Option[Request ?=> F[RealRes]] =
    QuickDev.get().map(q =>
      get(quickDevUrl,
        security.attemptLogin(q.user, q.pass).flatMap {
          case None    => F pure Response(ResponseCmd.Redirect(Urls.login), Cookie.Update.empty)
          case Some(u) => security.sessionPersist(Security.SessionToken(Some(u))).map(Response(ResponseCmd.Redirect(q.goto), _))
        }
      )
    )

  /** FOR UNIT-TESTS ONLY */
  private val unitTestLogin: Request ?=> F[RealRes] =
    whenUrlIs(unitTestLoginUrl){ implicit req =>
      onMethod(Post)(security.protect(
        parseParams(
          for {
            u <- req.param("user")
            p <- req.param("pass")
          } yield (Username.orEmail(u), PlainTextPassword(p))
        ) { case (u, p) =>
          security.attemptLogin(u, p).flatMap {
            case Some(u) => security.sessionPersist(Security.SessionToken(Some(u))).map(Response(ResponseCmd.StatusOnly.OK, _))
            case None    => F pure ResponseCmd.StatusOnly.Forbidden.withoutCookieUpdate
          }
        }
      ))
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Stateful routes (i.e. using a session) */
  def statefulDispatcher(devMode: Boolean, testMode: Boolean): RealReq => F[RealRes] =
    ( Main.routes
    | Option.when(devMode)(quickDev).flatten
    | Option.when(testMode)(unitTestLogin)
    ).withFallback(Main.fallback)
      .compose(readRealReq)

  /** Stateless routes (i.e. using a session) */
  val statelessDispatcher: RealReq => F[RealRes] =
    ( Ajax.routes
    | Ops.routes
    ).withFallback(Main.fallback)
      .compose(readRealReq)

  def statelessCandidate(u: Url.Relative): Boolean =
    Ajax.candidate(u) || Ops.candidate(u)

}
