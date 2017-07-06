package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util._
import shipreq.webapp.base.{MemberUrls, PublicUrls}
import shipreq.webapp.base.data.{ExternalId => XId}
import shipreq.webapp.base.user.User

object DispatchLogic {

  /** A request to the server.
    *
    * @param get_? Is the request method GET?
    * @param path Does *NOT* include query params
    */
  final case class Request(get_? : Boolean, path: Url.Relative)

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
}

final class DispatchLogic[F[_]](implicit F: Monad[F], security: Security.Algebra[F]) {
  import DispatchLogic.{Request, Response}
  import Response._

  type Route = Request ?=> F[Response]

  private type FResp = F[Response]
  private type Cond = FResp => Route

  private val fRedirectToPublicHome: FResp = F pure redirectToPublicHome
  private val fRedirectToLogin     : FResp = F pure redirectToLogin
  private val fMethodNotAllowed    : FResp = F pure MethodNotAllowed

  private def getOnly(cond: Request => Boolean): Cond =
    resp => FnWithFallback.when(cond)(req => if (req.get_?) resp else fMethodNotAllowed)

  private def needAuth(resp: Response): FResp =
    security.isAuthenticated.map(auth => if (auth) resp else redirectToLogin)

  private def needAuth(resp: FResp): FResp =
    security.isAuthenticated.flatMap(auth => if (auth) resp else fRedirectToLogin)

  private def needAuth(f: User => FResp): FResp =
    security.authenticatedUser.flatMap(_.fold(fRedirectToLogin)(f))

  private def staticUrl(url: Url.Relative): Cond =
    getOnly(_.path ==* url)

  private def spa(root: Url.Relative): Cond =
    getOnly(DispatchLogic.spaTest(root))

  private def spaId[T, I](url: Url.Relative.Param1[XId[T]])
                         (scheme: ExternalId.Scheme[T, I])
                         (response: String \/ I => FResp): Route =
    FnWithFallback.extract(DispatchLogic.spaTest1(url))(
      req => str => if (req.get_?) response(scheme.parse(str)) else fMethodNotAllowed)


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
    staticUrl(MemberUrls.logout)(
      security.logout >| redirectToPublicHome)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val partial: Request ?=> F[Response] =
    publicSpa | memberHomeSpa | projectSpa | logout

  val all: Request => F[Response] =
    partial.withFallback(r => if (r.get_?) fRedirectToPublicHome else fMethodNotAllowed)
}
