package shipreq.webapp.client.project.app

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.base.protocol.ClientSideProcImpl
import shipreq.webapp.client.project.app.state.ClientData

@JSExportTopLevel(ProjectSpaProtocols.CometListenerName)
object CometListener extends ClientSideProcImpl(ProjectSpaProtocols.CometListener) {

  private var cd: js.UndefOr[ClientData] = js.undefined

  def init(cd: ClientData): Unit = {
    this.cd = cd
  }

  override def run(ves: VerifiedEvent.NonEmptySeq): Unit = {
    cd.foreach(_.applyEventSeqCB(ves).runNow())
  }
}
