package shipreq.webapp.server.app

/*
import net.liftweb.http.{LiftResponse, Req, S, XmlResponse}
import scala.xml.Group
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.lib.Taskman

object ExceptionHandler extends HasLogger {

  def handleServerError(r: Req, e: Throwable): LiftResponse = {
    val uri = r.uri.toString
    val m = Taskman.webappErrorOccurred(e, Some(uri), s"Request: $r")
    log.error(e, s"500 Error serving $uri to user ${m.usr}")
    Taskman.submitAsync(m).unsafeRun()

    // TODO Oshiro.enforceHumanSpeed()

    val content = r.normalizeHtml(S.render(<lift:embed what="500" />, r.request))
    XmlResponse(Group(content), 500, "text/html; charset=utf-8", r.cookies)
  }

}
*/