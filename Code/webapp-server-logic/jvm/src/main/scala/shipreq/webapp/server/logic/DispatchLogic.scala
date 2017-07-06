package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util._
import shipreq.webapp.base.{MemberUrls, PublicUrls}
import shipreq.webapp.base.data.{ExternalId => XId}
import shipreq.webapp.base.user._

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
    case object ServeHomeSpa extends Response
    case object ServePublicSpa extends Response
    object ProjectSpa {
      case object Serve extends Response
      case object NotOwner extends Response
      case object InvalidId extends Response
    }
    case object MethodNotAllowed extends Response
    final case class Redirect(dest: Url.Relative) extends Response
    final case class StatusOnly(status: Int) extends Response

    implicit def univEq: UnivEq[Response] = UnivEq.derive

    val redirectToPublicHome = Redirect(PublicUrls.home)
    val redirectToLogin      = Redirect(PublicUrls.login)
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
        Config.get [String]("GOTO").map(_.fold(MemberUrls.home)(Url.Relative(_)))
      )(apply).withPrefix("SHIPREQ_DEV_")

    def get(): Option[QuickDev] =
      config.run(Props.sources).unsafePerformIO().toDisjunction.toOption
  }
}

final class DispatchLogic[F[_]](implicit F: Monad[F], security: Security.Algebra[F]) {
  import DispatchLogic._
  import Method._
  import Response._

  type Route = Request ?=> F[Response]

  private type FR = F[Response]

  private val fRedirectToPublicHome: FR = F pure redirectToPublicHome
  private val fRedirectToLogin     : FR = F pure redirectToLogin
  private val fMethodNotAllowed    : FR = F pure MethodNotAllowed

  private def onMethod(m: Method, resp: FR): Request => FR =
    req => if (req.method eq m) resp else fMethodNotAllowed

  private def onMethod(m: Method)(f: Request => FR): Request => FR =
    req => if (req.method eq m) f(req) else fMethodNotAllowed

  private def onGet(resp: FR)        : Request => FR = onMethod(Get, resp)
  private def onGet(f: Request => FR): Request => FR = onMethod(Get)(f)

  // Occasional type inference problem
  @inline private def when(cond: Request => Boolean)(ok: Request => FR): Route = FnWithFallback.when(cond)(ok)

  private def whenUrl(url: Url.Relative, ok: Request => FR): Route =
    when(_.path ==* url)(ok)

  private def needAuth(resp: Response): FR =
    security.isAuthenticated.map(auth => if (auth) resp else redirectToLogin)

  private def needAuth(resp: FR): FR =
    security.isAuthenticated.flatMap(auth => if (auth) resp else fRedirectToLogin)

  private def needAuth(f: User => FR): FR =
    security.authenticatedUser.flatMap(_.fold(fRedirectToLogin)(f))

  private def get(url: Url.Relative, resp: FR): Route =
    whenUrl(url, onGet(resp))

  private def spa(root: Url.Relative): FR => Route =
    fr => when(spaTest(root))(onGet(fr))

  private def spaId[T, I](url: Url.Relative.Param1[XId[T]])
                         (scheme: ExternalId.Scheme[T, I])
                         (response: String \/ I => FR): Route =
    FnWithFallback.extract(spaTest1(url))(
      req => str => onGet(response(scheme.parse(str)))(req))


  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  val publicSpa: Route =
    PublicUrls.PublicSpaRoute.static
      .map(s => spa(s.url)(F pure ServePublicSpa))
      .reduce(_ | _)

  val memberHomeSpa: Route =
    spa(MemberUrls.home)(
      needAuth(
        ServeHomeSpa))

  val projectSpa: Route =
    spaId(MemberUrls.project)(ProjectId.Extern) {
      case \/-(projectId) =>
        needAuth(u =>
          // TODO Check ProjectStore first
          security.db.getProjectOwner(projectId).map {
            case Some(o) if o ==* u.id => ProjectSpa.Serve
            case Some(_)               => ProjectSpa.NotOwner
            case None                  => ProjectSpa.InvalidId
          }
        )
      case -\/(_) => F pure ProjectSpa.InvalidId
    }

  val logout: Route =
    get(MemberUrls.logout,
      security.logout >| redirectToPublicHome)

  val main: Request ?=> F[Response] =
    publicSpa | memberHomeSpa | projectSpa | logout

  val fallback: Request => F[Response] =
    onGet(fRedirectToPublicHome)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** For tests only. Not meant for production */
  val loginApi: Route =
    whenUrl(loginApiUrl,
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
          case None    => redirectToLogin
        }
      )
    )
}
