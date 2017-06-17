package shipreq.webapp.client.home

import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.client.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.HomeSpaProtocols
import shipreq.webapp.client.base.protocol.{ClientSideProcImpl, ClientProtocol}
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.home.ui.{Home, Styles}

@JSExportTopLevel(HomeSpaProtocols.EntryPointName)
object Main extends ClientSideProcImpl(HomeSpaProtocols.EntryPoint) {

  override def run(i: HomeSpaProtocols.InitClient): Unit = {

    BaseStyles.addToDocument()
    Styles.addToDocument()

    Home.Props(i, ClientProtocol.Default).render
      .renderIntoDOM(dom.document.getElementById("tgt"))
  }
}

