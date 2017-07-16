package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig

object DispatchLogic {

  /** A request to the server.
    *
    * @param path Does *NOT* include query params
    */
  final case class Request(method: Method, path: Url.Relative, param: String => Option[String])

  sealed abstract class Method
  object Method {
    case object Get   extends Method
    case object Post  extends Method
    case object Other extends Method
  }

  sealed trait Response
  object Response {
    case object ServePublicSpa extends Response
    final case class ServeHomeSpa(user: User) extends Response
    object ProjectSpa {
      final case class Serve(user: User, projectId: ProjectId) extends Response
      case object NotOwner extends Response
      case object InvalidId extends Response
    }
    case object MethodNotAllowed extends Response
    final case class Redirect(dest: Url.Relative) extends Response
    final case class StatusOnly(status: Int) extends Response

    implicit def univEq: UnivEq[Response] = UnivEq.derive

    val redirectToPublicHome = Redirect(Urls.publicHome)
    val redirectToMemberHome = Redirect(Urls.memberHome)
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

  /** For tests only. Not meant for production */
  val loginApiUrl = Url.Relative("/api/login")

  /** For dev only. Not meant for production */
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

final class DispatchLogic[F[_]](implicit F: Monad[F],
                                config    : ServerConfig,
                                security  : Security.Algebra[F],
                                db        : DB.SecurityTokenReadOnly[F],
                                svr       : Server.Time[F]) {
  import DispatchLogic._
  import Method._
  import Response._

  type Route = Request ?=> F[Response]

  private type FR = F[Response]

  private val fRedirectToPublicHome: FR = F pure redirectToPublicHome
  private val fMethodNotAllowed    : FR = F pure MethodNotAllowed

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
    Redirect(Urls.login / req.path.relativeUrlNoHeadSlash)

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


  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  val publicSpa: Route = {
    import Urls.{PublicSpaRoute => R}

    // This logic is mirrored in .public.spa.Routes
    val login: Route =
      spa(R.Login.url)(_ =>
        security.isAuthenticated.map(a =>
          if (a) redirectToMemberHome else ServePublicSpa))

    val staticRoutes: Route = {
      val fr: FR = F pure ServePublicSpa
      whenUrlIsAnyOf(R.static.filterNot(_ ==* R.Login).get.map(_.url).toNES)(onGet(_ => fr))
    }

    val securityTokenFn: R.NeedsToken => SecurityToken => F[SecurityToken.Status] = {
      case R.Register2     => PublicSpaLogic.tokenStatusFn(db.getUserRegistrationTokenIssueDate, config.confirmationTokenLifespan)
      case R.ResetPassword => PublicSpaLogic.tokenStatusFn(db.getResetPasswordTokenIssueDate, config.passwordResetTokenLifespan)
    }

    val onSecurityTokenStatus: SecurityToken.Status => Response = {
      case SecurityToken.Status.Valid   => ServePublicSpa
      case SecurityToken.Status.Invalid => redirectToPublicHome
      case SecurityToken.Status.Expired => redirectToPublicHome // could be better but good enough
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
    spa(Urls.memberHome)(needAuth(ServeHomeSpa))

  val projectSpa: Route =
    spaWithObfuscatedParam(Urls.project)(Obfuscators.projectId) {
      case \/-(projectId) =>
        needAuthF(user =>
          // TODO Check ProjectStore first
          security.db.getProjectOwner(projectId).map {
            case Some(o) if o ==* user.id => ProjectSpa.Serve(user, projectId)
            case Some(_)                  => ProjectSpa.NotOwner
            case None                     => ProjectSpa.InvalidId
          }
        )
      case -\/(_) => _ => F pure ProjectSpa.InvalidId
    }

  val logout: Route =
    get(Urls.logout,
      security.logout >| redirectToPublicHome)

  val main: Request ?=> F[Response] =
    publicSpa | memberHomeSpa | projectSpa | logout

  val fallback: Request => F[Response] =
    onGet(_ => fRedirectToPublicHome)

  def cacheUsualPaths(f: Request => F[Response]): Request => F[Response] = {
    // Caching ignores params - beware
    val noParams: String => Option[String] = _ => None
    val urls = Urls.PublicSpaRoute.static.map(_.url) ++ Urls.MemberRoute.static.map(_.url)
    val cacheMap = urls.iterator.map(u => u.underlying -> f(Request(Get, u, noParams))).toMap
    val cache = Util.quickStringLookup(cacheMap)
    req => cache(req.path.underlying) getOrElse f(req)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** For tests only. Not meant for production */
  val loginApi: Route =
    whenUrlIs(loginApiUrl)(
      onMethod(Post) { req =>

        val credentials = for {
          u <- req.param("user")
          p <- req.param("pass")
        } yield (Username.orEmail(u), PlainTextPassword(p))

        credentials match {
          case Some((u, p)) => security.protect(security.attemptLogin(u, p).map {
            case Some(_) => StatusOnly(200)
            case None    => StatusOnly(401)
          })
          case None => F pure StatusOnly(400)
        }
      })

  /** For dev only. Not meant for production */
  val quickDev: Option[Route] =
    QuickDev.get().map(q =>
      get(quickDevUrl,
        security.attemptLogin(q.user, q.pass).map {
          case Some(_) => Redirect(q.goto)
          case None    => Redirect(Urls.login)
        }
      )
    )
}
