package shipreq.webapp.client.home

import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.protocol.{AjaxClient, ClientSideProcImpl, HomeSpaEntryPoint}
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.home.ui.{Home, Styles}

@JSExportTopLevel(HomeSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(HomeSpaEntryPoint.proc) {

  override def run(i: HomeSpaEntryPoint.InitData): Unit = {

    BaseStyles.addToDocument()
    Styles.addToDocument()

    Home.Props(i, AjaxClient.Binary).render.renderIntoDOM(`#root`)
  }
}

