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
import shipreq.webapp.server.logic.{Cookie, DB, DispatchLogic}

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
}

final class LiftDispatcher(global: Global) extends StrictLogging {
  import LiftDispatcher._

  def init(): Unit = {
    LiftRules.dispatch.append(statefulDispatchPF)
    LiftRules.statelessDispatch.prepend(statelessDispatchPF)
    LiftRules.statelessDispatch.prepend(removeWwwSubdomainPF)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val paramFn: String => Option[String] =
    S.param(_).toOption

  private val cookieFn: Cookie.Name => Option[String] =
    n => S.cookieValue(n.value).toOption

  private def liftReqUrl(r: LiftReq): Url.Relative =
    Url.Relative(r.request.uri)

  def parseReq(r: LiftReq): DispatchLogic.Request[LiftReq] = {
    val method: DispatchLogic.Method =
      if (r.get_?)       DispatchLogic.Method.Get
      else if (r.post_?) DispatchLogic.Method.Post
      else               DispatchLogic.Method.Other

    val url = liftReqUrl(r)

    val body = Need {
      S.request.flatMap(_.body) match {
        case Full(b)       => Some(BinaryData.unsafeFromArray(b))
        case Empty         => None
        case e: BoxFailure => logger.warn(s"Failure reading request body: ${e.msg}", e.rootExceptionCause); None
      }
    }

    DispatchLogic.Request(method, url, body, paramFn, cookieFn, r)
  }

  val makeResponse: (LiftReq, DispatchLogic.Response) => Fx[Box[LiftResponse]] = {
    type Template = Box[NodeSeq]
    val templatePublic : Template = Templates("public" :: Nil)
    val templateHome   : Template = Templates("members-home" :: Nil)
    val templateProject: Template = Templates("members-project" :: Nil)

    def render(req: LiftReq, t: Template): Box[LiftResponse] =
      S.session.flatMap(_.processTemplate(t, req, req.path, 200))

    val setHeader: ((String, String)) => Unit =
      x => S.setHeader(x._1, x._2)

    val deleteCookie: Cookie.Name => Unit =
      n => S.deleteCookie(n.value)

    val addCookie: Cookie => Unit = {
      val path = Full("/") // This is required for browsers to update the cookie on AJAX
      c => S.addCookie(new HTTPCookie(
        name     = c.name.value,
        value    = Full(c.value),
        maxAge   = c.maxAgeInSec,
        secure_? = c.secure,
        httpOnly = c.httpOnly,
        domain   = Empty,
        path     = path,
        version  = Empty))
      }

    (req, response) => {
      import DispatchLogic.ResponseCmd._

      val setHeaders: Fx[Unit] =
        Fx {
          response.cmd.headers.foreach(setHeader)
          response.cookies.remove.foreach(deleteCookie)
          response.cookies.add.foreach(addCookie)
        }

      val respond: Fx[Box[LiftResponse]] =
        response.cmd match {
          case ServePublicSpa(ou)     => Fx{ ou.foreach(UserVar.set); render(req, templatePublic) }
          case ServeHomeSpa(u)        => Fx{ UserVar.set(u); render(req, templateHome) }
          case ProjectSpa.Serve(u, p) => Fx{ UserVar.set(u); ProjectIdVar.set(p); render(req, templateProject) }
          case ProjectSpa.NotOwner
             | ProjectSpa.InvalidId   => Fx(Full(RedirectResponse(Urls.memberHome.relativeUrl)))
          case Redirect(to)           => Fx(Full(redirectWithCookies(to.relativeUrl)))
          case r: Binary              => Fx(Full(BinaryResponse(r.status, r.body)))
          case r: Text                => Fx(Full(GenericResponse(r.status, r.body, "text/plain")))
          case r: Json                => Fx(Full(GenericResponse(r.status, r.body, "application/json")))
          case StatusOnly(status)     => Fx(Full(StatusOnlyResponse(status)))
          // NOTE: Do NOT use Fx.pure here. These lift responses need to be created after headers/cookies are set
        }

      setHeaders.flatMap(_ => respond)
    }
  }

  val logic: DispatchLogic[Fx, LiftReq, Box[LiftResponse]] = {
    implicit val config    = global.config
    implicit val metrics   = global.metrics
    implicit val trace     = global.trace
    implicit val taskman   = global.taskman
    implicit val security  = global.security
    implicit val publicSpa = global.logic.publicSpa
    implicit val ops       = global.ops
    implicit val db        = DB.SecurityTokenReadOnly.trans(DbInterpreter.SecurityTokenReadOnly)(global.db.fx.trans)
    implicit val server    = ServerInterpreter
    new DispatchLogic(parseReq, makeResponse)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val statefulDispatchPF: LiftRules.DispatchPF = {

    /** Is a request by/to Lift (eg. Ajax, Comet) */
    def isLiftRequest(r: LiftReq): Boolean = {
      val pp = r.path.partPath // path separated by slashes
      pp.nonEmpty && pp.head == WebappConfig.liftPath1
    }

    def noFileExtension(r: LiftReq): Boolean =
      r.path.suffix.isEmpty && // Fast path
        r.request.uri.indexOf('.') == -1 // Because r.path.suffix is empty when more than one '.' exists

    def hasHtmlFileExtension(r: LiftReq): Boolean =
      r.request.uri endsWith ".html"

    val dispatch = logic.statefulDispatcher(devMode = Props.devMode, testMode = Props.testMode)

    {
      case r if (r.request ne null) && noFileExtension(r) && !isLiftRequest(r) => () => dispatch(r).unsafeRun()
      case r if (r.request ne null) && hasHtmlFileExtension(r)                 => () => Full(r.createNotFound)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val statelessDispatchPF: LiftRules.DispatchPF = {
    val dispatch = logic.statelessDispatcher

    {
      case r if (r.request ne null) && logic.statelessCandidate(liftReqUrl(r)) => () => dispatch(r).unsafeRun()
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val removeWwwSubdomainPF: LiftRules.DispatchPF = {
    case r if (r.request ne null) && r.request.serverName.startsWith("www.") =>
      () => Full(RedirectResponse(r.request.url.replace("://www.", "://")))
  }
}
