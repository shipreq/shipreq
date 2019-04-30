package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.base.util.{ErrorMsg, Url}
import shipreq.webapp.base.protocol.{ClientSideProcImpl, ProjectSpaProtocols, WebSocketClient}
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.project.app.root._
import shipreq.webapp.client.project.app.state.{ClientData, ProjectState}

@JSExportTopLevel(ProjectSpaProtocols.EntryPointName)
object Main extends ClientSideProcImpl(ProjectSpaProtocols.EntryPoint) {

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  override def run(i: ProjectSpaProtocols.InitPageData): Unit = {
    BaseStyles.addToDocument()
    Style.addToDocument()


    val baseUrl = determineBaseUrl(dom.window.location.href)

    val protocol = ProjectSpaProtocols.WebSocket(i.projectId)
    val wsUrlBase = Url.Absolute.Base(baseUrl.value).forWebSocket

    val wsClient = WebSocketClient(wsUrlBase, protocol)(
      push => Callback.log("WS PUSH RECV: " + push),
      rs => _ => Callback.log("WS READYSTATE: " + rs))


//    ClientData.initAsync(cp, i.initAsync)(onSuccess, onFailure).runNow()
//
//    def onSuccess(cd: ClientData): Callback =
//      Callback {
//        CometListener.init(cd)
//        val root    = new LoadedRoot(i, cp, cd)
//        val baseUrl = determineBaseUrl(dom.window.location.href)
//        val router  = Router(baseUrl, Routes.routerConfig(root))
//        router().renderIntoDOM(`#root`)
//      }
//
//    def onFailure(error: ErrorMsg): Callback =
//      Callback {
//        val lp = LoadingPage.Props(i.username, i.projectName)
//        val lf = LoadFailedPage.Props(lp, error)
//        LoadFailedPage.Component(lf).renderIntoDOM(`#root`)
//      }
  }
}

final class AppState(wsClient: AppState.WsClient) {
  // protected val mutableState = new ProjectState.Mutable(initialState)
}

//sealed trait AppState
object AppState {
  type WsClient = WebSocketClient[ProjectSpaProtocols.WsReqRes]

  sealed trait State
  object State {
    final case class Xxxx() extends State
  }
}