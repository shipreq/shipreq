package shipreq.webapp.server.logic

import japgolly.microlibs.utils.Utils
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.ParseLong
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.{AssetManifest, Urls}
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.base.util.ResourceHint
import shipreq.webapp.server.ServerConfig

/**
  * Usage
  * =====
  *
  * 1. Wire up [[DispatchLogic.Ops]] to session-less dispatch.
  *    Use [[DispatchLogic.Ops.candidate]] as the condition and [[DispatchLogic.Ops.total]] as the handler.
  *
  * 2. Wire up [[DispatchLogic.mainDispatcher()]] to normal (session-dependent) dispatch.
  *
  */
object DispatchLogic {

  /** A request to the server.
    *
    * @param path Does *NOT* include query params
    */
  final case class Request[+Real](method: Method,
                                  path  : Url.Relative,
                                  param : String => Option[String],
                                  real  : Real)

  sealed abstract class Method
  object Method {
    case object Get   extends Method
    case object Post  extends Method
    case object Other extends Method
    implicit def univEq: UnivEq[Method] = UnivEq.derive
  }

  sealed trait Response {
    def headers: Response.Headers = Nil
  }
  object Response {
    type Header = (String, String)
    type Headers = List[Header]

    private def linkHeader(params: TraversableOnce[String]): Header =
      ("Link", params.mkString(", "))

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

    case object ServePublicSpa extends Response

    final case class ServeHomeSpa(user: User) extends Response

    object ProjectSpa {
      final case class Serve(user: User, projectId: ProjectId) extends Response
      case object NotOwner extends Response
      case object InvalidId extends Response
    }

    /** Respond in a way that indicates the HTTP method (GET, POST etc) wasn't allowed */
    case object MethodNotAllowed extends Response

    final case class Redirect(dest: Url.Relative) extends Response

    /** Respond with a HTTP status only; no content */
    final case class StatusOnly(status: Int) extends Response

    final case class Text(status: Int, body: String) extends Response

    final case class Json(status: Int, bodyJs: upickle.Js.Value) extends Response {
      val body: String =
        upickle.json.write(bodyJs)
    }

    implicit def univEq: UnivEq[Response] = {
      implicit def a: UnivEq[upickle.Js.Value] = UnivEq.force
      UnivEq.derive
    }

    val redirectToPublicHome = Redirect(Urls.publicHome)
    val redirectToMemberHome = Redirect(Urls.memberHome)
  }

  object StatusCode {
    final val OK           = 200
    final val BadRequest   = 400
    final val Unauthorized = 401
    final val Forbidden    = 403
    final val NotFound     = 404
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
  val quickDevUrl = Url.Relative("/x")
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
                                                  publicApi : PublicSpaLogic.ForApi[F],
                                                  security  : Security.Algebra[F],
                                                  svrS      : Server.Session[F],
                                                  svr       : Server.Time[F],
                                                  tracer    : TraceLogic[F, RealReq, RealRes]) {

  import DispatchLogic.{Request => _, Response => AbsRes, _}
  import DispatchLogic.Method._

  private type Request = DispatchLogic.Request[RealReq]
  private[this] val Response = DispatchLogic.Response

  @inline private def makeReal(r: AbsRes)(implicit req: Request): F[RealRes] =
    makeRealRes(req.real, r)

  @inline private def makeRealF(f: F[AbsRes])(implicit req: Request): F[RealRes] =
    F.bind(f)(makeRealRes(req.real, _))

  @inline private def traceUrl(url: Url.Relative, f: F[RealRes])(implicit req: Request): F[RealRes] =
    traceUrlWithSpan(url, _ => f)

  @inline private def traceUrlWithSpan(url: Url.Relative, f: tracer.Span => F[RealRes])(implicit req: Request): F[RealRes] =
    metrics.setHttpName(url.relativeUrl) >> tracer.http(url.relativeUrl, req.real, req.path)(f)

  private def onMethod(m: Method, f: F[AbsRes])(implicit req: Request): F[RealRes] =
    if (req.method eq m)
      makeRealF(f)
    else
      makeReal(AbsRes.MethodNotAllowed)

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

  private def loginRequired(implicit req: Request): AbsRes =
    Response.Redirect(
      if (req.path ==* Urls.memberHome)
        Urls.login
      else
        Urls.login / req.path.relativeUrlNoHeadSlash)

  private def needAuth(f: User => F[AbsRes])(implicit span: tracer.Span, req: Request): F[AbsRes] =
    security.authenticatedUser.flatMap(_.fold(F pure loginRequired)(u =>
      tracer.alg.addAttrs(Trace.Attr.ShipReqUserId(u.id.value) :: Nil) >> f(u)))

  private def get(url: Url.Relative, resp: F[AbsRes]): Request ?=> F[RealRes] =
    whenUrlIs(url)(implicit req =>
      traceUrl(req.path,
        onMethod(Get, resp)))

  private def spa(root: Url.Relative, onGet: (tracer.Span, Request) => F[AbsRes]): Request ?=> F[RealRes] =
    when(spaTest(root))(implicit req =>
      traceUrlWithSpan(root, span =>
        onMethod(Get, onGet(span, req))))

  private def spaWithObfuscatedParam[A](url       : Url.Relative.Param1[Obfuscated[A]])
                                       (obfuscator: Obfuscator[A])
                                       (onGet     : (tracer.Span, Request, String \/ A) => F[AbsRes]): Request ?=> F[RealRes] =
    extract(spaTest1(url))(implicit req => param =>
      traceUrlWithSpan(url.prefix, span =>
        onMethod(Get, onGet(span, req, obfuscator.deobfuscate(Obfuscated(param))))))

  private def parseParams[A](parsed: Option[A])(f: A => F[AbsRes]): F[AbsRes] =
    parsed match {
      case Some(a) => f(a)
      case None    => F pure Response.StatusOnly(StatusCode.BadRequest)
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Main {

    val publicSpa: Request ?=> F[RealRes] = {
      import Urls.{PublicSpaRoute => R}

      // This logic is mirrored in .public.spa.Routes
      val login: Request ?=> F[RealRes] =
        spa(R.Login.url, (_, _) =>
          security.isAuthenticated.map(a =>
            if (a) Response.redirectToMemberHome else Response.ServePublicSpa))

      val staticRoutes: Request ?=> F[RealRes] = {
        val serve: F[AbsRes] = F pure Response.ServePublicSpa
        whenUrlIsAnyOf(R.static.filterNot(_ ==* R.Login).get.map(_.url).toNES) { implicit req =>
          traceUrl(req.path,
            onMethod(Get, serve))
        }
      }

      val securityTokenFn: R.NeedsToken => SecurityToken => F[SecurityToken.Status] = {
        case R.Register2     => PublicSpaLogic.tokenStatusFn(db.getUserRegistrationTokenIssueDate, config.registrationTokenLifespan)
        case R.ResetPassword => PublicSpaLogic.tokenStatusFn(db.getResetPasswordTokenIssueDate, config.passwordResetTokenLifespan)
      }

      val onSecurityTokenStatus: SecurityToken.Status => AbsRes = {
        case SecurityToken.Status.Valid   => Response.ServePublicSpa
        case SecurityToken.Status.Invalid => Response.redirectToPublicHome
        case SecurityToken.Status.Expired => Response.redirectToPublicHome // could be better but good enough
      }

      val securityTokenRoutes: Request ?=> F[RealRes] =
        R.needsToken.map { r =>
          val getTokenStatus = securityTokenFn(r)
          extract(spaTest1(r.url)) { implicit req => param =>
            val token = SecurityToken(param)
            traceUrl(r.url.prefix,
              onMethod(Get,
                security.protect(
                  getTokenStatus(token).map(onSecurityTokenStatus))))
          }
        }.reduce(_ | _)

      login | staticRoutes | securityTokenRoutes
    }

    val memberHomeSpa: Request ?=> F[RealRes] =
      spa(Urls.memberHome, needAuth(F pure Response.ServeHomeSpa(_))(_, _))

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
                case Some(o) if o ==* user.id => Response.ProjectSpa.Serve(user, projectId)
                case Some(_)                  => Response.ProjectSpa.NotOwner
                case None                     => Response.ProjectSpa.InvalidId
              }
            )
          case -\/(_) => F pure Response.ProjectSpa.InvalidId
        }
      }

    val logout: Request ?=> F[RealRes] =
      get(Urls.logout, SimpleEndpoints.logout >| Response.redirectToPublicHome)

    val routes: Request ?=> F[RealRes] =
      publicSpa | memberHomeSpa | projectSpa | logout

    val fallback: Request => F[RealRes] = { implicit req =>
      makeReal(
        if (req.method eq Get)
          AbsRes.redirectToPublicHome
        else
          AbsRes.MethodNotAllowed)
    }

//    def cacheUsualPaths(f: Request => F[RealRes]): Request => F[RealRes] = {
//      // Caching ignores params - beware
//      val noParams: String => Option[String] = _ => None
//      val urls = Urls.PublicSpaRoute.static.map(_.url) ++ Urls.MemberRoute.static.map(_.url)
//      val cacheMap = urls.iterator.map(u => u.underlying -> f(Request(Get, u, noParams))).toMap
//      val cache = Util.quickStringLookup(cacheMap)
//      req => cache(req.path.underlying).fold(f(req))(
//        cachedResponse => if (req.method eq Get) cachedResponse else fMethodNotAllowed)
//    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Ops & Diagnostics

  object Ops {
    import upickle.Js
    import shipreq.taskman.api.MsgId

    private val notFoundSecure: F[AbsRes] =
      security.protect( // prevent response-time hacking to discover endpoints (meaning ops URLs)
        F pure Response.Text(404, "Not found."))

    private def endpoint(method: Method, url: Url.Relative)(resp: Request => F[AbsRes]): Request ?=> F[RealRes] =
      whenUrlIs(url) { implicit req =>
        traceUrl(req.path,
          makeRealF(req.param(opsSecretKey) match {
            case Some(key) if key ==* opsSecretValue.value && req.method ==* method =>
              security.protect(resp(req))
            case _ =>
              notFoundSecure
          })
        )
      }

    private def whenValid[A](fa: F[ErrorMsg \/ A])(f: A => AbsRes): F[AbsRes] =
      fa.map {
        case \/-(a) => f(a)
        case -\/(e) => Response.Text(StatusCode.BadRequest, e.value)
      }

    private def jsonResponse(r: OpsEndpoints.HasJsValue): AbsRes =
      Response.Json(StatusCode.OK, r.toJsValue)

    /** Return a static 200.
      * Useful to test that the web-server is up and serving requests.
      * Used for container health-checks.
      */
    private val ok: Request ?=> F[RealRes] =
      get(Url.Relative("ok"),
        F pure Response.Text(StatusCode.OK, "OK."))

    /** API for invoking the first part of the registration process
      * (regardless of whether public registrations are enabled or not).
      */
    private val register1: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("register1"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(publicApi.register1(email))(id =>
            Response.Json(StatusCode.OK, Js.Obj("taskId" -> Js.Num(id.value))))))

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
            case None    => Response.StatusOnly(StatusCode.NotFound)
          }
        )
      )

    private val testSendMail: Request ?=> F[RealRes] =
      endpoint(Post, Url.Relative("test-sendmail"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(ops.sendMail(email))(
            jsonResponse)))

    private def routes: Request ?=> F[RealRes] =
      scope(opsRoot, ok | register1 | statsDb | statsUsers | task | testSendMail)

    /** Is the request a candidate for ops route parsing? */
    val candidate: Url.Relative => Boolean =
      opsRoot.isEqualToOrParentOf

    private def fallback: Request => F[RealRes] = { implicit req =>
      makeRealF(notFoundSecure)
    }

    val total: RealReq => F[RealRes] =
      routes.withFallback(fallback).compose(readRealReq)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Other

  /** DEV-MODE ONLY */
  private val quickDev: Option[Request ?=> F[RealRes]] =
    QuickDev.get().map(q =>
      get(quickDevUrl,
        security.attemptLogin(q.user, q.pass).map {
          case Some(_) => Response.Redirect(q.goto)
          case None    => Response.Redirect(Urls.login)
        }
      )
    )

  /** FOR UNIT-TESTS ONLY */
  private val unitTestLogin: Request ?=> F[RealRes] =
    whenUrlIs(unitTestLoginUrl){ implicit req =>
      onMethod(Post, security.protect(
        parseParams(
          for {
            u <- req.param("user")
            p <- req.param("pass")
          } yield (Username.orEmail(u), PlainTextPassword(p))
        ) { case (u, p) =>
          security.attemptLogin(u, p).map {
            case Some(_) => Response.StatusOnly(StatusCode.OK)
            case None    => Response.StatusOnly(StatusCode.Unauthorized)
          }
        }
      ))
    }

  /** Stateful routes (i.e. using a session) */
  def mainDispatcher(devMode: Boolean, testMode: Boolean): RealReq => F[RealRes] =
    ( Main.routes
    | Option.when(devMode)(quickDev).flatten
    | Option.when(testMode)(unitTestLogin)
    ).withFallback(Main.fallback)
      .compose(readRealReq)

}
