package shipreq.webapp.client.project.app

import japgolly.microlibs.nonempty.NonEmptyVector
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.client.base.protocol.ClientSideProcImpl
import shipreq.webapp.client.project.app.state.ClientData

@JSExportTopLevel(ProjectSpaProtocols.CometListenerName)
object CometListener extends ClientSideProcImpl(ProjectSpaProtocols.CometListener) {

  private var cd: js.UndefOr[ClientData] = js.undefined

  def init(cd: ClientData): Unit = {
    this.cd = cd
  }

  override def run(i: NonEmptyVector[VerifiedEvent]): Unit = {
    cd.foreach(_.applyEventsS(i.whole).runNow())
  }
}
