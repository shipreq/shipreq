package shipreq.webapp.server.app

import net.liftweb.http._
import net.liftweb.util.NamedPF
import shipreq.base.util.log.HasLogger
import shipreq.base.util.FxModule._

/**
  * Terrible name, but basically serves generic non-200 responses based on HTTP status.
  */
object HttpStatusHandler extends HasLogger {

  def init(): Unit = {

    // Custom 404
    LiftRules.uriNotFound.prepend(NamedPF("404") {
      case (req, _) => NotFoundAsResponse(render404(req))
    })

    // Custom 500
    LiftRules.exceptionHandler.prepend {
      case (_, req, exception) => on500(req, exception).unsafeRun()
    }
  }

  private val headers: List[(String, String)] =
    "Content-Type" -> "text/html;charset=utf-8" ::
    "Cache-Control" -> "no-cache,private,no-store" ::
    "Pragma" -> "no-cache" ::
    Nil

  private def responseFn(status: Int): Req => InMemoryResponse = {
    val f = s"/$status.html"
    val d = LiftRules.loadResource(f).openOrThrowException(s"Template not found: $f")
    _ => InMemoryResponse(d, headers, S.responseCookies, status)
  }

  private val render404 = responseFn(404)
  private val render500 = responseFn(500)

  def on500(req: Req, exception: Throwable): Fx[LiftResponse] =
    Global.security.protect(Fx[LiftResponse] {
      val uri = req.request.uri
      //    val m = Taskman.webappErrorOccurred(e, Some(uri), s"Request: $r")
      //    log.error(e, s"500 Error serving $uri to user ${m.usr}")
      //    Taskman.submitAsync(m).unsafeRun()
      log.error(s"${exception.getClass.getSimpleName} serving $uri", exception)
      render500(req)
    })
}
