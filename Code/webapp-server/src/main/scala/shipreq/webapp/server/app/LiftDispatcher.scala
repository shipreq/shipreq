package shipreq.webapp.server.app

import net.liftweb.common.{Box, Full}
import net.liftweb.http.{Req => LiftReq, _}
import scala.xml.NodeSeq
import scalaz.effect.IO
import shipreq.base.util.Url
import shipreq.webapp.base.MemberUrls
import shipreq.webapp.server.logic.DispatchLogic

final class LiftDispatcher {

  def dispatchPF: LiftRules.DispatchPF = {
    case r if r.path.suffix.isEmpty => dispatch(r)
  }

  val logic: DispatchLogic[IO] = {
    implicit val security = Global.security
    new DispatchLogic
  }

  def dispatch(r: LiftReq): () => Box[LiftResponse] = () => {
    val req = DispatchLogic.Request(r.get_?, Url.Relative(r.uri))
    val res = logic.all(req).unsafePerformIO()
    liftResponse(r, res)
  }

  type Template = Box[NodeSeq]
  private val templatePublic : Template = Templates("public" :: Nil)
  private val templateHome   : Template = Templates("home" :: Nil)
  private val templateProject: Template = Templates("project" :: Nil)

  private def render(req: LiftReq, t: Template): Box[LiftResponse] =
    S.session.flatMap(_.processTemplate(templatePublic, req, req.path, 200))

  def liftResponse(req: LiftReq, response: DispatchLogic.Response): Box[LiftResponse] = {
    import DispatchLogic.Response._
    response match {
      case ServePublicSpa       => render(req, templatePublic)
      case ServeHomeSpa         => render(req, templateHome)
      case ProjectSpa.Serve     => render(req, templateProject)
      case ProjectSpa.NotOwner
         | ProjectSpa.InvalidId => Full(RedirectResponse(MemberUrls.home.relativeUrl))
      case Redirect(to)         => Full(RedirectResponse(to.relativeUrl))
      case MethodNotAllowed     => Full(MethodNotAllowedResponse())
    }
  }
}
