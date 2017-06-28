package shipreq.webapp.server.lib

import scalaz.Monad
import scalaz.effect.IO
import shipreq.base.util.Error
import shipreq.taskman.api.Msg.WebappErrorOccurred
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.app.Global

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}
  import shipreq.webapp.server.app.{AppSiteMap => SM}
  import SM.Implicits._

  def updateCfg[F[_]: Monad](api: TaskmanApi[F]): F[Unit] =
    api.cfgPutBulk(
      K.appName  -> WebappConfig.appName,
      K.homeUrl  -> SM.Home.absoluteUrl,
      K.loginUrl -> SM.LoginAbsoluteUrl)

  def webappErrorOccurred(e: Throwable, url: Option[String], suppInfo: String): WebappErrorOccurred =
    WebappErrorOccurred(
      Global.security.loggedInUser().map(_.id),
      url,
      s"${Error stackTraceStr e}\n\nSUPP: $suppInfo")

  def submitAsync(w: WebappErrorOccurred): IO[Unit] =
    IO(()) // TODO

//  private object AsyncActor extends SpecializedLiftActor[Msg] {
//    override protected def messageHandler = {
//      case m: Msg =>
//        try
//          submitMsg(m).unsafePerformIO()
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
