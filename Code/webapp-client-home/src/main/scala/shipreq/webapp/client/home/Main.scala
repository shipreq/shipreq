package shipreq.webapp.client.home

import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.{ClientFnDecl, InitDataForHomeSpa}
import shipreq.webapp.client.base.protocol.{ClientFnImpl, ClientProtocol}
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.home.ui.{Home, Styles}

@JSExport(ClientFnDecl.HomeSpaName)
object Main extends ClientFnImpl(ClientFnDecl.HomeSpa) {

  override def run(i: InitDataForHomeSpa): Unit = {

    BaseStyles.addToDocument()
    Styles.addToDocument()

    Home.Props(i, ClientProtocol.Default).render
      .renderIntoDOM(dom.document.getElementById("tgt"))
  }
}

