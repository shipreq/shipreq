package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.{ErrorMsg, Permission}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.protocol.websocket.UpdateAccessCmd
import shipreq.webapp.member.ui.BaseStyles

object AccessPage {

  type AsyncKey = shipreq.webapp.client.project.app.pages.admin.access.AsyncKey

  final case class Props(userId         : UserId.Public,
                         access         : ProjectAccess,
                         rolodex        : Rolodex,
                         editability    : Permission,
                         state          : StateSnapshot[State],
                         confirmJs      : ConfirmJs,
                         sspUpdateAccess: ServerSideProcInvoker[UpdateAccessCmd, ErrorMsg, Any],
                         async          : AsyncFeature.ReadWrite.D1[AsyncKey, ErrorMsg]
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(existingUserSegment: ExistingUserSegment.State)

  object State {
    import ExistingUserSegment.State.{reusability => existingUserSegmentReusability}

    implicit val reusability: Reusability[State] =
      Reusability.derive

    def init: State =
      State(ExistingUserSegment.State.init)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {

      val existingUserSegment = ExistingUserSegment.Props(
        p.userId,
        p.access,
        p.rolodex,
        p.editability,
        p.state.zoomStateL(State.existingUserSegment),
        p.confirmJs,
        p.sspUpdateAccess,
        p.async,
      ).render

      val leaveProjectSegment = LeaveProjectSegment.Props(
        p.confirmJs,
        p.sspUpdateAccess,
        p.async(AsyncKey(p.userId)),
      ).render

      <.main(BaseStyles.containerLarge,
        existingUserSegment,
        leaveProjectSegment)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}
