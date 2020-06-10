package shipreq.webapp.client.home

import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.feature.ErrorHandlingFeature
import shipreq.webapp.base.protocol.ajax.{AjaxClient, CommonProtocolsJs}
import shipreq.webapp.base.protocol.entrypoint.{ClientSideProcImpl, HomeSpaEntryPoint}
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.home.ui.{Home, Styles}

@JSExportTopLevel(HomeSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(HomeSpaEntryPoint.proc) {

  override def run(i: HomeSpaEntryPoint.InitData): Unit = {

    BaseStyles.addToDocument()
    ErrorHandlingFeature.enable()
    Styles.addToDocument()

    val view     = Home.Props(i, AjaxClient.Binary).render
    val metadata = CommonProtocolsJs.Metadata.client(i.username)
    val reactApp = ErrorHandlingFeature(view, metadata)
    reactApp.renderIntoDOM(`#root`)
  }
}

