package shipreq.webapp.server.lib

import shipreq.base.util.ArticulateError
import shipreq.taskman.api.Msg.WebappErrorOccurred
import shipreq.base.util.FxModule._
import shipreq.webapp.base.{Urls, WebappConfig}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.WebappTaskmanConverters._

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}

  def updateCfg(g: Global): Fx[Unit] =
    g.taskman.cfgPutBulk(
      K.appName  -> WebappConfig.appName,
      K.homeUrl  -> g.config.baseUrl.value,
      K.loginUrl -> (g.config.baseUrl / Urls.login).absoluteUrl)

//  def webappErrorOccurred(e: Throwable, url: Option[String], suppInfo: String): WebappErrorOccurred =
//    WebappErrorOccurred(
//      Global.security.authenticatedUser.unsafeRun().map(_.id.toTaskman),
//      url,
//      ArticulateError(e).hint(suppInfo).show)

//  private object AsyncActor extends SpecializedLiftActor[Msg] {
//    override protected def messageHandler = {
//      case m: Msg =>
//        try
//          submitMsg(m).unsafePerformFx()
//        catch {
//          case e: Throwable if m.isInstanceOf[WebappErrorOccurred] =>
//            log.error(e, s"Error occurred trying to send $m. FUCK.")
//          case e: Throwable =>
//            log.error(e, s"Error occurred send $m.")
//            this ! Taskman.errorMsg(e, None, s"Was trying to send: $m")
//        }
//    }
//  }
}
