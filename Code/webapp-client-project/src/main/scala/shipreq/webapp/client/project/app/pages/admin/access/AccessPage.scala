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
                         async          : AsyncFeature.ReadWrite.D1[AsyncKey, ErrorMsg]) {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(newUserSegment     : NewUserSegment.State,
                         existingUserSegment: ExistingUserSegment.State)

  object State {
    def init: State =
      State(NewUserSegment.State.init, ExistingUserSegment.State.init)
  }

  private def render(p: Props) = {

    val newUserSegment = NewUserSegment.Props(
      p.state.zoomStateL(State.newUserSegment),
      p.editability,
      p.sspUpdateAccess,
      p.async(AsyncKey.newUser),
    ).render

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
      newUserSegment,
      existingUserSegment,
      leaveProjectSegment)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .build
}
