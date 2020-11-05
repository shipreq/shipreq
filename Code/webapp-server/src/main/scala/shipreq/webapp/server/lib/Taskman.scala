package shipreq.webapp.server.lib

import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.slf4j.MDC
import scala.jdk.CollectionConverters._
import scala.util.Try
import shipreq.base.util.FxModule._
import shipreq.taskman.api.Task
import shipreq.webapp.base.config.{Urls, WebappConfig}
import shipreq.webapp.base.data.UserId
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.util.WebappTaskmanConverters._

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}

  def updateCfg(g: Global): Fx[Unit] =
    g.taskman.cfgPutBulk(
      K.appName  -> WebappConfig.appName,
      K.homeUrl  -> g.config.server.baseUrl.value,
      K.loginUrl -> (g.config.server.baseUrl / Urls.login).absoluteUrl)

  private final val exceptionPrefix     = "error."
  private final val exceptionNameSuffix = "name"
  private final val exceptionMsgSuffix  = "message"

  def reportServerError(userId: Option[UserId], error: Throwable, other: Map[String, String] = Map.empty): Task.ReportServerError = {

    var m = other

    def add(k: String, v: String): Unit =
      if (v != null)
        m = m.updated(k, v.trim)

    def addError(prefix: String, t: Throwable): Unit = {

      def add2(k: String)(v: String): Unit =
        add(prefix + k, v)

      Try(t.getClass.getSimpleName)
        .filter(_ != null)
        .orElse(Try(t.getClass.getName))
        .foreach(add2(exceptionNameSuffix))

      Try(t.getMessage)
        .filter(_ != null)
        .foreach(add2(exceptionMsgSuffix))

      add2("stack")(t.stackTraceAsString)
    }

    // Add main error
    addError(exceptionPrefix, error)

    // Add root cause
    var rootCause = error
    while (rootCause.getCause != null)
      rootCause = rootCause.getCause
    if (rootCause ne error)
      addError("rootCause.", rootCause)

    // Add MDC
    for ((k, v) <- MDC.getCopyOfContextMap.asScala)
      add(k, v)

    Task.ReportServerError(
      userId     = userId.map(_.toTaskman),
      nameKey    = exceptionPrefix + exceptionNameSuffix,
      messageKey = exceptionPrefix + exceptionMsgSuffix,
      data       = m,
    )
  }

//  private object AsyncActor extends SpecializedLiftActor[Msg] {
//    override protected def messageHandler = {
//      case m: Msg =>
//        try
//          submitMsg(m).unsafePerformFx()
//        catch {
//          case e: Throwable if m.isInstanceOf[ReportServerError] =>
//            log.error(e, s"Error occurred trying to send $m. FUCK.")
//          case e: Throwable =>
//            log.error(e, s"Error occurred send $m.")
//            this ! Taskman.errorMsg(e, None, s"Was trying to send: $m")
//        }
//    }
//  }
}
