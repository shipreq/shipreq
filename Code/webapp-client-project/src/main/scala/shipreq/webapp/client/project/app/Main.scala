package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import java.time.Duration
import org.scalajs.dom.window.location
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.base.util.{ErrorMsg, Retries, Url}
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.feature.ErrorHandlingFeature
import shipreq.webapp.base.lib.{ConfirmJs, LoggerJs, PromptJs}
import shipreq.webapp.base.protocol.ajax.CommonProtocolsJs
import shipreq.webapp.base.protocol.entrypoint.ClientSideProcImpl
import shipreq.webapp.base.protocol.websocket.WebSocketClient
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.client.loaders.ProjectSpaLoader
import shipreq.webapp.client.project.app.pages.root._
import shipreq.webapp.client.project.app.state.Global
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint.InitData
import shipreq.webapp.member.ui.{BaseStyles, OptionalFullscreen, ReauthenticationModal}

@JSExportTopLevel(ProjectSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(ProjectSpaEntryPoint.proc) {

  private var stopBackground = Callback.empty

  override def run(i: InitData): Unit = {
    BaseStyles.addToDocument()
    ErrorHandlingFeature.enable()
    Style.addToDocument()

    val localStorage = AbstractWebStorage.localOrEmpty()
    val reauth       = ReauthenticationModal(i.username)(localStorage)
    val protocol     = ProjectSpaProtocols.WebSocket(i.projectId)
    val wsUrlBase    = Url.Absolute.Base(location.protocol + "//" + location.host).forWebSocket
    val wsClient     = WebSocketClient.Builder(wsUrlBase, protocol, wsRetries)

    val global = Global(
      reauth        = reauth,
      wscBuilder    = wsClient,
      onFirstLoad   = (g, _) => onLoad(i, g),
      onInitFailure = onFailure(i),
      localStorage  = localStorage,
      logger        = LoggerJs.devOnly.prefixedWith("[WCP] "))

    val keepAliveEvery     = Duration.ofSeconds(21)
    val syncEvery          = Duration.ofSeconds(30)
    val syncStaleTolerance = Duration.ofSeconds(30)

    val keepAliveHnd = global.wsClient.keepAlive.setInterval(keepAliveEvery).runNow()
    val staleSyncHnd = global.requestSyncIfStaleFor(syncStaleTolerance).setInterval(syncEvery).runNow()

    stopBackground = keepAliveHnd.cancel >> staleSyncHnd.cancel >> global.wsClient.close

    global.wsClient.connect.runNow()
  }

  private def onLoad(i: InitData, g: Global): Callback =
    Callback {
      val ww       = WebWorkerClient(i.webWorkerJsUrl, g.logger)
      val root     = new LoadedRoot(i, g, ConfirmJs.real, PromptJs.real, OptionalFullscreen.real, ww)
      val baseUrl  = determineBaseUrl(location.href)
      val router   = Router(baseUrl, Routes.routerConfig(root))
      val metadata = CommonProtocolsJs.Metadata.client(i.username, g.projectMetadata(i.projectId))
      val reactApp = ErrorHandlingFeature(router(), metadata)
      reactApp.renderIntoDOM(`#root`)
    }

  private def onFailure(i: InitData)(error: ErrorMsg): Callback =
    Callback {
      val lp       = ProjectSpaLoader.Props(i.username, i.projectName, i.assetManifest)
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
