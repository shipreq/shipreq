package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import java.time.Duration
import org.scalajs.dom.window.location
import scala.scalajs.js.annotation.JSExportTopLevel
import scalacss.ScalaCssReact._
import shipreq.base.util.{ErrorMsg, Retries, Url}
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.feature.ErrorHandlingFeature
import shipreq.webapp.base.lib.{ConfirmJs, LoggerJs, PromptJs}
import shipreq.webapp.base.protocol.ajax.CommonProtocolsJs
import shipreq.webapp.base.protocol.entrypoint.ProjectSpaEntryPoint.InitData
import shipreq.webapp.base.protocol.entrypoint.{ClientSideProcImpl, ProjectSpaEntryPoint}
import shipreq.webapp.base.protocol.websocket.ProjectSpaProtocols.InitAppData
import shipreq.webapp.base.protocol.websocket.{ProjectSpaProtocols, WebSocketClient}
import shipreq.webapp.base.ui.{BaseStyles, ReauthenticationModal}
import shipreq.webapp.client.loaders.ProjectSpaLoader
import shipreq.webapp.client.project.app.pages.root._
import shipreq.webapp.client.project.app.state.Global

@JSExportTopLevel(ProjectSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(ProjectSpaEntryPoint.proc) {

  private var stopBackground = Callback.empty

  override def run(i: InitData): Unit = {
    BaseStyles.addToDocument()
    ErrorHandlingFeature.enable()
    Style.addToDocument()

    val reauth    = ReauthenticationModal(i.username)
    val protocol  = ProjectSpaProtocols.WebSocket(i.projectId)
    val wsUrlBase = Url.Absolute.Base(location.protocol + "//" + location.host).forWebSocket
    val wsClient  = WebSocketClient.Builder(wsUrlBase, protocol, wsRetries)
    val global    = Global(reauth, wsClient, onLoad(i), onFailure(i), LoggerJs.on)

    val keepAliveEvery     = Duration.ofSeconds(21)
    val syncEvery          = Duration.ofSeconds(30)
    val syncStaleTolerance = Duration.ofSeconds(30)

    val keepAliveHnd = global.wsClient.keepAlive.setInterval(keepAliveEvery).runNow()
    val staleSyncHnd = global.requestSyncIfStaleFor(syncStaleTolerance).setInterval(syncEvery).runNow()

    stopBackground = keepAliveHnd.cancel >> staleSyncHnd.cancel >> global.wsClient.close

    global.wsClient.connect.runNow()
  }

  private def onLoad(i: InitData)(g: Global, ia: InitAppData): Callback =
    Callback {
      val root     = new LoadedRoot(i, g, ConfirmJs.real, PromptJs.real)
      val baseUrl  = determineBaseUrl(location.href)
      val router   = Router(baseUrl, Routes.routerConfig(root))
      val metadata = CommonProtocolsJs.Metadata.client(i.username, g.projectMetadata(i.projectId))
      val reactApp = ErrorHandlingFeature(router(), metadata)
      reactApp.renderIntoDOM(`#root`)
    }

  private def onFailure(i: InitData)(error: ErrorMsg): Callback =
    Callback {
      val lp       = ProjectSpaLoader.Props(i.username, i.projectName)
      val lf       = LoadFailedPage.Props(lp, error)
      val view     = LoadFailedPage.Component(lf)
      val metadata = CommonProtocolsJs.Metadata.client(i.username, i.projectId)
      val reactApp = ErrorHandlingFeature(view, metadata)
      reactApp.renderIntoDOM(`#root`)
    } >> stopBackground

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  private def wsRetries: Retries =
    Retries.exponentially(Duration.ofMillis(1000)).takeWhile(_.getSeconds < 64) ++
      Retries.continually(Duration.ofSeconds(60))
}
