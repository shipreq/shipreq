package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.{ClientFnDecl, InitDataForProjectSpa}
import shipreq.webapp.client.base.protocol.{ClientFnImpl, ClientProtocol}
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.project.app.root.{LoadFailedPage, LoadedRoot, LoadingPage, Routes}
import shipreq.webapp.client.project.app.state.ClientData

@JSExport(ClientFnDecl.ProjectSpaName)
object Main extends ClientFnImpl(ClientFnDecl.ProjectSpa) {

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  override def run(i: InitDataForProjectSpa): Unit = {

    val cp = ClientProtocol.Default
    BaseStyles.addToDocument()
    Style.addToDocument()

    ClientData.init(i.project, cp, i.projectInit)(onSuccess, onFailure)
      .runNow()

    def domTarget() =
      dom.document.getElementById("tgt")

    def onSuccess(cd: ClientData): Callback =
      Callback {
        val root    = new LoadedRoot(i, cp, cd)
        val baseUrl = determineBaseUrl(dom.window.location.href)
        val router  = Router(baseUrl, Routes.routerConfig(root))
        router().renderIntoDOM(domTarget())
      }

    def onFailure(error: String): Callback =
      Callback {
        val lp = LoadingPage.Props(i.username, i.project)
        val lf = LoadFailedPage.Props(lp, error)
        LoadFailedPage.Component(lf).renderIntoDOM(domTarget())
      }
  }
}
