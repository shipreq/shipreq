package shipreq.webapp.server.app

import com.typesafe.scalalogging.StrictLogging
import java.nio.charset.Charset
import net.liftweb.common.{Box, Empty, Failure => BoxFailure, Full}
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.http.{Req => LiftReq, _}
import net.liftweb.util.Props
import scala.xml.NodeSeq
import scalaz.Need
import shipreq.base.util.FxModule._
import shipreq.base.util.{BinaryData, Url}
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.user.User
import shipreq.webapp.base.{Urls, WebappConfig}
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.{DB, DispatchLogic, dispatch}

object LiftDispatcher {
  object ProjectIdVar extends RequestVar[ProjectId](null)
  object UserVar      extends RequestVar[User     ](null)

  val UTF8 = Charset.forName("UTF-8")

  final case class StatusOnlyResponse(status: Int) extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(Array.empty, headers, cookies, status)
  }

  final case class GenericResponse(status: Int, body: String, mime: String) extends LiftResponse with HeaderDefaults {
    def toResponse = {
      val bytes = body.getBytes(UTF8)
      val headers2 = ("Content-Length", bytes.length.toString) :: ("Content-Type", mime + "; charset=utf-8") :: headers
      InMemoryResponse(bytes, headers2, cookies, status)
    }
  }

  final case class BinaryResponse(status: Int, body: BinaryData) extends LiftResponse with HeaderDefaults {
    def toResponse = {
      val headers2 = ("Content-Length" -> body.length.toString) :: ("Content-Type" -> "application/octet-stream") :: headers
      new OutputStreamResponse(body.writeTo, body.length, headers2, cookies, status)
    }
  }

  def redirectWithCookies(uri: String): RedirectResponse =
    RedirectResponse(uri, S.responseCookies: _*)

  final case class Template(name: String) {
    private val templateSrc = Templates(name :: Nil).openOrThrowException(s"Template not found: $name")

    def render(req: LiftReq): Box[LiftResponse] =
      // This is taken from LiftSession#processTemplate
      for {
        s <- S.session
      } yield {
        val xml1: NodeSeq = s.processSurroundAndInclude(PageName.get, templateSrc)
        val xml2: NodeSeq = StatelessLiftMerge(s).merge(xml1, req)
        LiftRules.convertResponse((
          (xml2, 200),
          S.getResponseHeaders(LiftRules.defaultHeaders((xml2, req))),
          S.responseCookies,
          req
        ))
      }
  }
}

final class LiftDispatcher(global: Global) extends StrictLogging {
  import LiftDispatcher._

  def init(): Unit = {
    LiftRules.statelessDispatch.prepend(mainDispatchPF)
    LiftRules.statelessDispatch.prepend(removeWwwSubdomainPF)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val paramFn: String => Option[String] =
    S.param(_).toOption

  private val cookieFn: Cookie.Name => Option[String] =
    n => S.cookieValue(n.value).toOption

  private def liftReqUrl(r: LiftReq): Url.Relative =
    Url.Relative(r.request.uri)

  def parseReq(r: LiftReq): dispatch.Request[LiftReq] = {
    val method: dispatch.Method =
      if (r.get_?)       dispatch.Method.Get
      else if (r.post_?) dispatch.Method.Post
      else               dispatch.Method.Other

    val url = liftReqUrl(r)

    val body = Need {
      S.request.flatMap(_.body) match {
        case Full(b)       => Some(BinaryData.unsafeFromArray(b))
        case Empty         => None
        case e: BoxFailure => logger.warn(s"Failure reading request body: ${e.msg}", e.rootExceptionCause); None
      }
    }

    dispatch.Request(method, url, body, paramFn, cookieFn, r)
  }

  val makeResponse: (LiftReq, dispatch.Response) => Fx[Box[LiftResponse]] = {
    val templatePublic  = Template("public")
    val templateHome    = Template("members-home")
    val templateProject = Template("members-project")

    val setHeader: ((String, String)) => Unit =
      x => S.setHeader(x._1, x._2)

    val deleteCookie: Cookie.Name => Unit =
      n => S.deleteCookie(n.value)

    val addCookie: Cookie => Unit = {
      val path = Full("/") // This is required for browsers to update the cookie on AJAX
      c => {
        val httpCookie = new HTTPCookie(
          name     = c.name.value,
          value    = Full(c.value),
          maxAge   = c.maxAgeInSec,
          secure_? = c.secure,
          httpOnly = c.httpOnly,
          domain   = Empty,
          path     = path,
          version  = Empty)
        S.addCookie(httpCookie)
      }
    }

    (req, response) => {
      import shipreq.webapp.server.logic.dispatch.ResponseCmd._

      val setHeaders: Fx[Unit] =
        Fx {
          response.cmd.headers.foreach(setHeader)
          response.cookies.remove.foreach(deleteCookie)
          response.cookies.add.foreach(addCookie)
        }

      val respond: Fx[Box[LiftResponse]] =
        response.cmd match {
          case ServePublicSpa(ou)     => Fx{ ou.foreach(UserVar.set); templatePublic.render(req) }
          case ServeHomeSpa(u)        => Fx{ UserVar.set(u); templateHome.render(req) }
          case ProjectSpa.Serve(u, p) => Fx{ UserVar.set(u); ProjectIdVar.set(p); templateProject.render(req) }
          case ProjectSpa.NotOwner
             | ProjectSpa.InvalidId   => Fx(Full(RedirectResponse(Urls.memberHome.relativeUrl)))
          case Redirect(to)           => Fx(Full(redirectWithCookies(to.relativeUrl)))
          case r: Binary              => Fx(Full(BinaryResponse(r.status, r.body)))
          case r: Text                => Fx(Full(GenericResponse(r.status, r.body, "text/plain")))
          case r: Json                => Fx(Full(GenericResponse(r.status, r.body, "application/json")))
          case StatusOnly(status)     => Fx(Full(StatusOnlyResponse(status)))
          // NOTE: Do NOT use Fx.pure here. These lift responses need to be created after headers/cookies are set
        }

      S.statelessInit(req) {
        setHeaders.flatMap(_ => respond)
      }
    }
  }

  val logic: DispatchLogic[Fx, LiftReq] = {
    implicit val config    = global.config.server
    implicit val metrics   = global.metrics
    implicit val trace     = global.trace
    implicit val security  = global.security
    implicit val common    = global.logic.common
    implicit val publicSpa = global.logic.publicSpa
    implicit val homeSpa   = global.logic.homeSpa
    implicit val ops       = global.ops
    implicit val db        = DB.VerificationTokenReadOnly.trans(DbInterpreter.VerificationTokenReadOnly)(global.runDB)
    implicit val server    = ServerInterpreter
    new DispatchLogic[Fx, LiftReq](parseReq)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val mainDispatchPF: LiftRules.DispatchPF = {

    /** Is a request by/to Lift (eg. Ajax, Comet) */
    def isLiftRequest(r: LiftReq): Boolean = {
      val pp = r.path.partPath // path separated by slashes
      pp.nonEmpty && pp.head == WebappConfig.liftCtxPath
    }

    def noFileExtension(r: LiftReq): Boolean =
      r.path.suffix.isEmpty && // Fast path
        r.request.uri.indexOf('.') == -1 // Because r.path.suffix is empty when more than one '.' exists

    def hasHtmlFileExtension(r: LiftReq): Boolean =
      r.request.uri endsWith ".html"

    val dispatch = logic.all(testMode = Props.testMode)

    {
      case r if (r.request ne null) && noFileExtension(r) && !isLiftRequest(r) =>
        () => {
          // The following two lines NEED to be run separately. Fusing them into the same Fx will break things.
          // makeResponse needs to execute on the same thread that the request came in on. This is because ol' fashioned
          // Lift uses thread-local variables for its cookies and headers.
          // Because of Doobie's thread control, dispatch logic often ends up on the Hikari or blocker[IO] thread pools.
          val genericResponse = dispatch(r).unsafeRun()
          val realResponse = makeResponse(r, genericResponse).unsafeRun()
          realResponse
        }

      case r if (r.request ne null) && hasHtmlFileExtension(r) =>
        () => Full(r.createNotFound)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val removeWwwSubdomainPF: LiftRules.DispatchPF = {
    case r if (r.request ne null) && r.request.serverName.startsWith("www.") =>
      () => Full(RedirectResponse(r.request.url.replace("://www.", "://")))
  }
}
