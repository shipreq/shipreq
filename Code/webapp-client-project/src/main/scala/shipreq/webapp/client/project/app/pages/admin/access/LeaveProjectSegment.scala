package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Button, ColourPlus, Header, Segment}
import shipreq.webapp.client.project.app.Style.accessPage.{leaveProjectSegment => *}
import shipreq.webapp.member.project.protocol.websocket.UpdateAccessCmd

object LeaveProjectSegment {

  final case class Props(confirmJs      : ConfirmJs,
                         sspUpdateAccess: ServerSideProcInvoker[UpdateAccessCmd.RemoveSelf.type, ErrorMsg, Any],
                         async          : AsyncFeature.ReadWrite.D0[ErrorMsg]) {
    @inline def render: VdomElement = Component(this)
  }

  object Props {
    implicit val reusability: Reusability[Props] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val onClick: Callback =
      $.props.flatMap { p =>
        for {
          proceed <- p.confirmJs("Are you sure you want to leave this project?")
          _ <- p.async.write.onFailureShowAndForget(p.sspUpdateAccess(UpdateAccessCmd.RemoveSelf)).when(proceed)
        } yield ()
      }

    def render(p: Props) = {

      val button = Button(
        tipe   = Button.Type.Text("Leave This Project"),
        state  = Button.State.loadingWhen(p.async.isInProgress),
        colour = ColourPlus.Negative,
      )

      Segment.raised(*.segment,
        <.div(*.left,
          Header.h4("Leave This Project"),
          <.p("Revoke your own access to this project. If you do this and change your mind, you'll need an admin to invite you back.")),
        <.div(
          button.onClick(onClick)(*.button)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
