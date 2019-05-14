package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import java.time.Duration
import org.scalajs.dom.window.location
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.RawTimers
import scalacss.ScalaCssReact._
import shipreq.base.util.{ErrorMsg, Retries, Url}
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, InitPageData}
import shipreq.webapp.base.protocol.{ClientSideProcImpl, ProjectSpaProtocols, WebSocketClient}
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.project.app.root._
import shipreq.webapp.client.project.app.state.Global

@JSExportTopLevel(ProjectSpaProtocols.EntryPointName)
object Main extends ClientSideProcImpl(ProjectSpaProtocols.EntryPoint) {

  override def run(i: InitPageData): Unit = {
    BaseStyles.addToDocument()
    Style.addToDocument()

    def onLoad(g: Global, ia: InitAppData): Callback =
      Callback {
        val root    = new LoadedRoot(i, g)
        val baseUrl = determineBaseUrl(location.href)
        val router  = Router(baseUrl, Routes.routerConfig(root))
        router().renderIntoDOM(`#root`)
      }

    def onFailure(error: ErrorMsg): Callback =
      Callback {
        val lp = LoadingPage.Props(i.username, i.projectName)
        val lf = LoadFailedPage.Props(lp, error)
        LoadFailedPage.Component(lf).renderIntoDOM(`#root`)
      }

    val protocol  = ProjectSpaProtocols.WebSocket(i.projectId)
    val wsUrlBase = Url.Absolute.Base(location.protocol + "//" + location.host).forWebSocket
    val wsClient  = WebSocketClient(wsUrlBase, protocol, wsRetries)
    val global    = Global(wsClient, onLoad, onFailure, LoggerJs.on)

    val keepAliveEvery     = Duration.ofSeconds(21)
    val syncEvery          = Duration.ofSeconds(30)
    val syncStaleTolerance = Duration.ofSeconds(30)

    RawTimers.setInterval(global.wsClient.keepAlive.toJsFn, keepAliveEvery.toMillis)
    RawTimers.setInterval(global.requestSyncIfStaleFor(syncStaleTolerance).toJsFn, syncEvery.toMillis)
    global.wsClient.connect.runNow()
  }

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  private def wsRetries: Retries =
    Retries.exponentially(Duration.ofMillis(1000)).takeWhile(_.getSeconds < 16) ++
      Retries.continually(Duration.ofSeconds(16))
}
