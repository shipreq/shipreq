package shipreq.webapp.server.app

import japgolly.microlibs.stdlib_ext.StdlibExt._
import net.liftweb.common.{Box, Full}
import net.liftweb.http.{Req => LiftReq, _}
import net.liftweb.util.Props
import scala.xml.NodeSeq
import shipreq.base.util.FxModule._
import shipreq.base.util.Url
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.user.User
import shipreq.webapp.base.{Urls, WebappConfig}
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.{DB, DispatchLogic}

object LiftDispatcher {
  object ProjectIdVar extends RequestVar[ProjectId](null)
  object UserVar      extends RequestVar[User     ](null)

  final case class StatusOnlyResponse(status: Int) extends LiftResponse with HeaderDefaults {
    def toResponse = InMemoryResponse(Array.empty, headers, cookies, status)
  }
}

final class LiftDispatcher(global: Global) {
  import LiftDispatcher._

  private[this] final val liftPathPart = WebappConfig.liftPath

  /** Is a request by/to Lift (eg. Ajax, Comet) */
  private def isLiftRequest(r: LiftReq): Boolean = {
    val pp = r.path.partPath // path separated by slashes
    pp.nonEmpty && pp.head == liftPathPart
  }

  private def noFileExtension(r: LiftReq): Boolean =
    r.path.suffix.isEmpty && // Fast path
      r.request.uri.indexOf('.') == -1 // Because r.path.suffix is empty when more than one '.' exists

  def dispatchPF: LiftRules.DispatchPF = {
    case r if noFileExtension(r) && !isLiftRequest(r) => dispatchLiftReq(r)
  }

  val logic: DispatchLogic[Fx] = {
    implicit val config   = global.config
    implicit val security = global.security
    implicit val db       = DB.SecurityTokenReadOnly.trans(DbInterpreter.SecurityTokenReadOnly)(global.db.fx.trans)
    implicit val server   = ServerInterpreter
    new DispatchLogic
  }

  private val dispatcher: DispatchLogic.Request => Fx[DispatchLogic.Response] =
    logic.cacheUsualPaths(
      ( logic.main
      | Option.when(Props.testMode)(logic.loginApi)
      | Option.when(Props.devMode)(logic.quickDev).flatten
      ).withFallback(logic.fallback)
    )

  private val paramFn: String => Option[String] =
    S.param(_).toOption

  private def dispatchLiftReq(r: LiftReq): () => Box[LiftResponse] = () => {
    val m: DispatchLogic.Method =
      if (r.get_?)       DispatchLogic.Method.Get
      else if (r.post_?) DispatchLogic.Method.Post
      else               DispatchLogic.Method.Other

    //// Was thinking a fast path here might be good because r.uri is lazy and non-trivial.
    //// Decided to use r.request.uri instead which seems to be fine
    //val pp = r.path.partPath
    //val url: Url.Relative =
    //  if (pp.isEmpty)
    //  if (pp.isEmpty)
    //    Url.Relative("")
    //  else {
    //    val t = pp.tail
    //    if (t.isEmpty)
    //      Url.Relative(pp.head)
    //    else
    //       Url.Relative(r.uri)
    //      // Url.Relative(pp.mkString("/"))
    //  }

    val url = Url.Relative(r.request.uri)
    val req = DispatchLogic.Request(m, url, paramFn)
    val res = dispatcher(req).unsafeRun()
//    println(s"[${req.path.relativeUrl}] -> $res")
    liftResponse(r, res)
  }

  private type Template = Box[NodeSeq]
  private val templatePublic : Template = Templates("public" :: Nil)
  private val templateHome   : Template = Templates("members-home" :: Nil)
  private val templateProject: Template = Templates("members-project" :: Nil)

  private def render(req: LiftReq, t: Template): Box[LiftResponse] =
    S.session.flatMap(_.processTemplate(t, req, req.path, 200))

  private val setHeader: ((String, String)) => Unit =
    x => S.setHeader(x._1, x._2)

  private def liftResponse(req: LiftReq, response: DispatchLogic.Response): Box[LiftResponse] = {
    import DispatchLogic.Response._
    response.headers.foreach(setHeader)
    response match {
      case ServePublicSpa         => render(req, templatePublic)
      case ServeHomeSpa(u)        => UserVar.set(u); render(req, templateHome)
      case ProjectSpa.Serve(u, p) => UserVar.set(u); ProjectIdVar.set(p); render(req, templateProject)
      case ProjectSpa.NotOwner
         | ProjectSpa.InvalidId   => Full(RedirectResponse(Urls.memberHome.relativeUrl))
      case Redirect(to)           => Full(RedirectResponse(to.relativeUrl))
      case MethodNotAllowed       => Full(MethodNotAllowedResponse())
      case StatusOnly(s)          => Full(StatusOnlyResponse(s))
    }
  }
}
