package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import monocle.macros.Lenses
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
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
  @Lenses
  final case class Request(method: Method, path: Url.Relative, param: String => Option[String])

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

  @inline private[this] def isSepChar(c: Char): Boolean =
    c == '/' || c == '#'

  /** If arg is /home this will match:
    *
    * /home
    * /home/…
    * /home#…
    */
  def spaTest(au: Url.Relative): Request => Boolean = {
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

  def spaTest1(au: Url.Relative.Param1[_]): Request => Option[String] = {
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
    import japgolly.microlibs.config._
    import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._

    def config =
      ( Config.need[String]("USER").map(Username.orEmail) |@|
        Config.need[String]("PASS").map(PlainTextPassword(_)) |@|
        Config.get [String]("GOTO").map(_.fold(Urls.memberHome)(Url.Relative(_)))
      )(apply).withPrefix("SHIPREQ_DEV_")

    def get(): Option[QuickDev] = {
      import FxModule._
      config.run(Props.sources).unsafeRun().toDisjunction.toOption
    }
  }
}

import DispatchLogic._

final class DispatchLogic[F[_], RealReq, RealRes](readRealReq: RealReq => Request,
                                                  makeRealRes: (RealReq, Response) => F[RealRes])
                                                 (implicit F: Monad[F],
                                                  config    : ServerConfig,
                                                  db        : DB.SecurityTokenReadOnly[F],
                                                  publicApi : PublicSpaLogic.ForApi[F],
                                                  security  : Security.Algebra[F],
                                                  svr       : Server.Time[F],
                                                  tracer    : Trace.Algebra[F, RealReq, RealRes]) {
  import Method._

  type Route = Request ?=> F[Response]

  private type FR = F[Response]

  private val fRedirectToPublicHome: FR = F pure Response.redirectToPublicHome
  private val fMethodNotAllowed    : FR = F pure Response.MethodNotAllowed

  private def onMethod(m: Method)(f: Request => FR): Request => FR =
    req => if (req.method eq m) f(req) else fMethodNotAllowed

  private def onGet(f: Request => FR): Request => FR =
    onMethod(Get)(f)

  // Occasional type inference problem
  @inline private def when(cond: Request => Boolean)(ok: Request => FR): Route = FnWithFallback.when(cond)(ok)
  @inline private def extract[E](cond: Request => Option[E])(ok: Request => E => FR): Route = FnWithFallback.extract(cond)(ok)
  @inline private def extractFlip[E](cond: Request => Option[E])(ok: E => Request => FR): Route = FnWithFallback.extract(cond)(r => ok(_)(r))

  private def whenUrlIs(url: Url.Relative): (Request => FR) => Route =
    when(_.path ==* url)

  private def whenUrlIsAnyOf(urls: NonEmptySet[Url.Relative]): (Request => FR) => Route = {
    import Url.dropTailSlashes
    val norm: Url.Relative => String = u => dropTailSlashes(u.underlying)
    val lookup = Util.quickStringExists(urls.whole.map(norm))
    when(r => lookup(norm(r.path)))
  }

  private def onAuthFail(req: Request): Response =
    Response.Redirect(
      if (req.path ==* Urls.memberHome)
        Urls.login
      else
        Urls.login / req.path.relativeUrlNoHeadSlash)

  private def needAuth(f: User => Response): Request => FR =
    req => security.authenticatedUser.map(_.fold(onAuthFail(req))(f))

  private def needAuthF(f: User => FR): Request => FR =
    req => security.authenticatedUser.flatMap(_.fold(F pure onAuthFail(req))(f))

  private def get(url: Url.Relative, resp: FR): Route =
    whenUrlIs(url)(onGet(_ => resp))

  private def spa(root: Url.Relative)(f: Request => FR): Route =
    when(spaTest(root))(onGet(f))

  private def spaWithObfuscatedParam[A](url       : Url.Relative.Param1[Obfuscated[A]])
                                       (obfuscator: Obfuscator[A])
                                       (response  : String \/ A => Request => FR): Route =
    extractFlip(spaTest1(url))(param =>
      onGet(response(obfuscator.deobfuscate(Obfuscated(param)))))

  /**
    * Re-scopes routes so that they must first match a prefix.
    *
    * Eg. /a & /b can be re-scoped under /x resulting in /x/a and /x/b
    */
  private def scope(prefix: Url.Relative, routes: Route): Route = {
    val test = prefix.isEqualToOrParentOf
    val mod = prefix.removeSelfOrParent
    routes.embed(r => test(r.path), Request.path.modify(mod))
  }

  private def parseParams[A](parsed: Option[A])(f: A => FR): FR =
    parsed match {
      case Some(a) => f(a)
      case None    => F pure Response.StatusOnly(StatusCode.BadRequest)
    }

  def makeReal(trace: Boolean)(d: Request => F[Response]): RealReq => F[RealRes] =
    if (trace)
      realReq => {
        val req = readRealReq(realReq)
        tracer.http(realReq, req.path)(
          d(req).flatMap(makeRealRes(realReq, _)))
      }
    else
      realReq => d(readRealReq(realReq)).flatMap(makeRealRes(realReq, _))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Main {

    val publicSpa: Route = {
      import Urls.{PublicSpaRoute => R}

      // This logic is mirrored in .public.spa.Routes
      val login: Route =
        spa(R.Login.url)(_ =>
          security.isAuthenticated.map(a =>
            if (a) Response.redirectToMemberHome else Response.ServePublicSpa))

      val staticRoutes: Route = {
        val fr: FR = F pure Response.ServePublicSpa
        whenUrlIsAnyOf(R.static.filterNot(_ ==* R.Login).get.map(_.url).toNES)(onGet(_ => fr))
      }

      val securityTokenFn: R.NeedsToken => SecurityToken => F[SecurityToken.Status] = {
        case R.Register2     => PublicSpaLogic.tokenStatusFn(db.getUserRegistrationTokenIssueDate, config.registrationTokenLifespan)
        case R.ResetPassword => PublicSpaLogic.tokenStatusFn(db.getResetPasswordTokenIssueDate, config.passwordResetTokenLifespan)
      }

      val onSecurityTokenStatus: SecurityToken.Status => Response = {
        case SecurityToken.Status.Valid   => Response.ServePublicSpa
        case SecurityToken.Status.Invalid => Response.redirectToPublicHome
        case SecurityToken.Status.Expired => Response.redirectToPublicHome // could be better but good enough
      }

      val securityTokenRoutes: Route =
        R.needsToken.map { r =>
          val getTokenStatus = securityTokenFn(r)
          def respond(t: SecurityToken): FR = security.protect(getTokenStatus(t).map(onSecurityTokenStatus))
          extractFlip(spaTest1(r.url))(t => onGet(_ => respond(SecurityToken(t))))
        }.reduce(_ | _)

      login | staticRoutes | securityTokenRoutes
    }

    val memberHomeSpa: Route =
      spa(Urls.memberHome)(needAuth(Response.ServeHomeSpa))

    val projectSpa: Route =
      spaWithObfuscatedParam(Urls.project)(Obfuscators.projectId) {
        case \/-(projectId) =>
          needAuthF(user =>
            // TODO Check ProjectStore first
            security.db.getProjectOwner(projectId).map {
              case Some(o) if o ==* user.id => Response.ProjectSpa.Serve(user, projectId)
              case Some(_)                  => Response.ProjectSpa.NotOwner
              case None                     => Response.ProjectSpa.InvalidId
            }
          )
        case -\/(_) => _ => F pure Response.ProjectSpa.InvalidId
      }

    val logout: Route =
      get(Urls.logout,
        security.logout >| Response.redirectToPublicHome)

    val routes: Request ?=> F[Response] =
      publicSpa | memberHomeSpa | projectSpa | logout

    val fallback: Request => F[Response] =
      onGet(_ => fRedirectToPublicHome)

    def cacheUsualPaths(f: Request => F[Response]): Request => F[Response] = {
      // Caching ignores params - beware
      val noParams: String => Option[String] = _ => None
      val urls = Urls.PublicSpaRoute.static.map(_.url) ++ Urls.MemberRoute.static.map(_.url)
      val cacheMap = urls.iterator.map(u => u.underlying -> f(Request(Get, u, noParams))).toMap
      val cache = Util.quickStringLookup(cacheMap)
      req => cache(req.path.underlying).fold(f(req))(
        cachedResponse => if (req.method eq Get) cachedResponse else fMethodNotAllowed)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Ops & Diagnostics

  object Ops {
    import upickle.Js

    private val notFoundSecure: FR =
      security.protect( // prevent response-time hacking to discover endpoints (meaning ops URLs)
        F pure Response.Text(404, "Not found."))

    private def endpoint(method: Method, url: Url.Relative)(resp: Request => FR): Route =
      whenUrlIs(url)(req =>
        req.param(opsSecretKey) match {
          case Some(key) if key ==* opsSecretValue.value && req.method ==* method =>
            security.protect(resp(req))
          case _ =>
            notFoundSecure
        }
      )

    /** Return a static 200.
      * Useful to test that the web-server is up and serving requests.
      * Used for container health-checks.
      */
    private val ok: Route =
      get(Url.Relative("ok"),
        F pure Response.Text(StatusCode.OK, "OK."))

    /** API for invoking the first part of the registration process
      * (regardless of whether public registrations are enabled or not).
      */
    private val register1: Route =
      endpoint(Post, Url.Relative("register1"))(req =>
        parseParams(req.param("email"))(e =>
          publicApi.register1(e).map {
            case \/-(id) => Response.Json(StatusCode.OK, Js.Obj("taskId" -> Js.Num(id.value)))
            case -\/(m)  => Response.Text(StatusCode.BadRequest, m.value)
          }
        )
      )

    private def routes: Route =
      scope(opsRoot, ok | register1)

    /** Is the request a candidate for ops route parsing? */
    val candidate: Url.Relative => Boolean =
      opsRoot.isEqualToOrParentOf

    val total: RealReq => F[RealRes] =
      makeReal(trace = false)(routes.withFallback(_ => notFoundSecure))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Other

  /** DEV-MODE ONLY */
  private val quickDev: Option[Route] =
    QuickDev.get().map(q =>
      get(quickDevUrl,
        security.attemptLogin(q.user, q.pass).map {
          case Some(_) => Response.Redirect(q.goto)
          case None    => Response.Redirect(Urls.login)
        }
      )
    )

  /** FOR UNIT-TESTS ONLY */
  private val unitTestLogin: Route =
    whenUrlIs(unitTestLoginUrl)(onMethod(Post)(req => security.protect(
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
    )))

  /** Stateful routes (i.e. using a session) */
  def mainDispatcher(devMode: Boolean, testMode: Boolean): RealReq => F[RealRes] =
    makeReal(trace = true)(
      Main.cacheUsualPaths(
        ( Main.routes
        | Option.when(devMode)(quickDev).flatten
        | Option.when(testMode)(unitTestLogin)
        ).withFallback(Main.fallback)
      )
    )

}
