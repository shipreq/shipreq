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
import shipreq.webapp.client.project.app.root.{LoadedRoot, Routes}
import shipreq.webapp.client.project.app.state.ClientData

@JSExport(ClientFnDecl.ProjectSpaName)
object Main extends ClientFnImpl(ClientFnDecl.ProjectSpa) {

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  override def run(initData: InitDataForProjectSpa): Unit = {

    val cp = ClientProtocol.Default
    BaseStyles.addToDocument()
    Style.addToDocument()

    ClientData.init(cp, initData.projectInit, cd => Callback {
      val root    = new LoadedRoot(initData, cp, cd)
      val baseUrl = determineBaseUrl(dom.window.location.href)
      val router  = Router(baseUrl, Routes.routerConfig(root))
      router() render dom.document.getElementById("tgt")
    }).runNow()
  }
}
