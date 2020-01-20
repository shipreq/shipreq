package shipreq.webapp.server.lib

import shipreq.base.util.ArticulateError
import shipreq.taskman.api.Task.ReportServerError
import shipreq.base.util.FxModule._
import shipreq.webapp.base.{Urls, WebappConfig}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.WebappTaskmanConverters._

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}

  def updateCfg(g: Global): Fx[Unit] =
    g.taskman.cfgPutBulk(
      K.appName  -> WebappConfig.appName,
      K.homeUrl  -> g.config.server.baseUrl.value,
      K.loginUrl -> (g.config.server.baseUrl / Urls.login).absoluteUrl)

//  def reportServerError(e: Throwable, url: Option[String], suppInfo: String): ReportServerError =
//    ReportServerError(
//      Global.security.authenticatedUser.unsafeRun().map(_.id.toTaskman),
//      url,
//      ArticulateError(e).hint(suppInfo).show)

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
