package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.member.project.protocol.websocket.UpdateAccessCmd
import shipreq.webapp.member.ui.BaseStyles

object AccessPage {

  final case class Props(state          : StateSnapshot[State],
                         confirmJs      : ConfirmJs,
                         sspUpdateAccess: ServerSideProcInvoker[UpdateAccessCmd, ErrorMsg, Any],
                         async          : AsyncFeature.ReadWrite.D1[UpdateAccessCmd, ErrorMsg]
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State()

  object State {
    implicit val reusability: Reusability[State] =
      Reusability.derive

    def init: State =
      State()
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {

      val leaveProjectSegment = LeaveProjectSegment.Props(
        p.confirmJs,
        p.sspUpdateAccess,
        p.async(UpdateAccessCmd.RemoveSelf),
      ).render

      <.main(BaseStyles.containerLarge,
        leaveProjectSegment)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}
