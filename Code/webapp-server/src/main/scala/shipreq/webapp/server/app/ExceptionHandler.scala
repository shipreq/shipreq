package shipreq.webapp.server.app

import net.liftweb.http.{S, XmlResponse, LiftResponse, Req}
import shipreq.webapp.server.security.Oshiro
import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.lib.Taskman

object ExceptionHandler extends HasLogger {

  def handleServerError(r: Req, e: Throwable): LiftResponse = {
    val uri = r.uri.toString
    val m = Taskman.errorMsg(e, Some(uri), s"Request: $r")
    log.error(e, s"500 Error serving $uri to user ${m.usr}")
    Global.taskman.submitMsgAsync(m).unsafePerformIO()

    Oshiro.enforceHumanSpeed()

    val content = S.render(<lift:embed what="500" />, r.request)
    XmlResponse(content.head, 500, "text/html", r.cookies)
  }

}
