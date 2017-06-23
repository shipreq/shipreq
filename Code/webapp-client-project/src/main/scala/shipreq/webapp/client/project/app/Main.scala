package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.client.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.client.base.protocol.{ClientSideProcImpl, ClientProtocol}
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.project.app.root.{LoadFailedPage, LoadedRoot, LoadingPage, Routes}
import shipreq.webapp.client.project.app.state.ClientData

@JSExportTopLevel(ProjectSpaProtocols.EntryPointName)
object Main extends ClientSideProcImpl(ProjectSpaProtocols.EntryPoint) {

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  override def run(i: ProjectSpaProtocols.InitData): Unit = {
    val cp = ClientProtocol.Default
    BaseStyles.addToDocument()
    Style.addToDocument()
    ClientData.initAsync(cp, i.initAsync)(onSuccess, onFailure).runNow()

    def domTarget() =
      dom.document.getElementById("tgt")

    def onSuccess(cd: ClientData): Callback =
      Callback {
        CometListener.init(cd)
        val root    = new LoadedRoot(i, cp, cd)
        val baseUrl = determineBaseUrl(dom.window.location.href)
        val router  = Router(baseUrl, Routes.routerConfig(root))
        router().renderIntoDOM(domTarget())
      }

    def onFailure(error: String): Callback =
      Callback {
        val lp = LoadingPage.Props(i.username, i.projectName)
        val lf = LoadFailedPage.Props(lp, error)
        LoadFailedPage.Component(lf).renderIntoDOM(domTarget())
      }
  }
}
